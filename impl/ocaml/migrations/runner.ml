type direction =
  | Apply
  | Revert

type phase =
  | Begin
  | Run
  | Finish

type operation =
  | Create_state_table
  | Read_state
  | Migration of {
      direction : direction;
      phase : phase;
      migration_id : Migration_id.t;
    }

type failure_recording = {
  migration_id : Migration_id.t;
  direction : direction;
  failure : exn;
  recording_failure : [ `Raised of exn | `Unexpected_state ];
}

type error = [
  | `Planner of Planner.error
  | `State_row of Migration_state.error
  | `Database of operation * exn
  | `State_conflict of operation
  | `Failure_recording of failure_recording
]

type state_change =
  | Insert_applying of { batch : int }
  | Mark_applied
  | Mark_reverting
  | Mark_apply_failed
  | Mark_revert_failed
  | Delete_reverting

module type Migrations = sig
  type -'a connection
  type execute_response
  type 'a future

  val baseline : Migration_id.t option

  val migration_plan :
    ([ `WR ] connection -> execute_response future) Migration.t list
end

module type State_store = sig
  type -'a connection
  type 'a future

  val create_state_table : [ `RO | `WR ] connection -> unit future

  val list_rows :
    [ `RO | `WR ] connection ->
    (Migration_state.row list, Migration_state.error) result future

  val change :
    [ `RO | `WR ] connection ->
    Migration_id.t ->
    state_change ->
    [ `Changed | `Unexpected_state ] future
end

module type S = sig
  type -'a connection
  type 'a future

  val pending :
    [ `RO | `WR ] connection -> (Migration_id.t list, error) result future

  val migrate :
    [ `RO | `WR ] connection -> (Migration_id.t list, error) result future

  val rollback :
    [ `RO | `WR ] connection -> (Migration_id.t list, error) result future
end

module Make_with_store
    (T : sig
       type -'a connection
       type execute_response

       module IO : sig
         type 'a future

         val return : 'a -> 'a future
         val ( >>= ) : 'a future -> ('a -> 'b future) -> 'b future

         val try_bind :
           (unit -> 'a future) ->
           ('a -> 'b future) ->
           (exn -> 'b future) ->
           'b future
       end
     end)
    (Store : State_store
               with type 'a connection := 'a T.connection
                and type 'a future := 'a T.IO.future)
    (History : sig
       val history : Planner.history
     end)
    (Migrations : Migrations
                    with type 'a connection := 'a T.connection
                     and type execute_response := T.execute_response
                     and type 'a future := 'a T.IO.future) =
struct
  let ( let* ) future continue =
    T.IO.(
      future >>= function
      | Ok value -> continue value
      | Error error -> return (Error error))

  let database operation work =
    T.IO.try_bind
      work
      (fun value -> T.IO.return (Ok value))
      (fun failure -> T.IO.return (Error (`Database (operation, failure))))

  let run_steps connection direction steps =
    let run_step (step : _ Planner.step) =
      let migration_id = step.migration_id in
      let operation phase = Migration { direction; phase; migration_id } in
      let begin_change, finish_change, failure_change =
        match direction with
        | Apply ->
            ( Insert_applying { batch = step.batch },
              Mark_applied,
              Mark_apply_failed )
        | Revert -> Mark_reverting, Delete_reverting, Mark_revert_failed
      in
      let change_state phase change =
        let* outcome =
          database (operation phase) (fun () ->
            Store.change connection migration_id change)
        in
        T.IO.return
          (match outcome with
           | `Changed -> Ok ()
           | `Unexpected_state -> Error (`State_conflict (operation phase)))
      in
      let* () = change_state Begin begin_change in
      T.IO.try_bind
        (fun () -> step.run (connection :> [ `WR ] T.connection))
        (fun (_ : T.execute_response) -> change_state Finish finish_change)
        (fun failure ->
          let unrecorded recording_failure =
            `Failure_recording
              { migration_id; direction; failure; recording_failure }
          in
          T.IO.try_bind
            (fun () -> Store.change connection migration_id failure_change)
            (fun recorded ->
              T.IO.return
                (Error
                   (match recorded with
                    | `Changed -> `Database (operation Run, failure)
                    | `Unexpected_state -> unrecorded `Unexpected_state)))
            (fun raised -> T.IO.return (Error (unrecorded (`Raised raised)))))
    in
    let rec loop = function
      | [] ->
          T.IO.return
            (Ok
               (List.map
                  (fun (step : _ Planner.step) -> step.migration_id)
                  steps))
      | step :: rest ->
          let* () = run_step step in
          loop rest
    in
    loop steps

  let planned connection plan =
    let* () =
      database Create_state_table (fun () -> Store.create_state_table connection)
    in
    let* rows = database Read_state (fun () -> Store.list_rows connection) in
    T.IO.return
      (match rows with
       | Error error -> Error (`State_row error)
       | Ok rows ->
           Result.map_error
             (fun error -> `Planner error)
             (plan
                ?baseline:Migrations.baseline
                History.history
                Migrations.migration_plan
                rows))

  let pending connection = planned connection Planner.pending

  let migrate connection =
    let* steps = planned connection Planner.migrate in
    run_steps connection Apply steps

  let rollback connection =
    let* steps = planned connection Planner.rollback in
    run_steps connection Revert steps
end

let error_to_string error =
  let describe_operation = function
    | Create_state_table -> "create state table"
    | Read_state -> "read state"
    | Migration { direction; phase; migration_id } ->
        (match phase with
         | Begin -> "begin "
         | Run -> ""
         | Finish -> "finish ")
        ^ (match direction with
           | Apply -> "apply "
           | Revert -> "revert ")
        ^ Migration_id.to_string migration_id
  in
  let changed_concurrently = "the state row changed concurrently" in
  match error with
  | `Planner error -> Planner.error_to_string error
  | `State_row error -> Migration_state.error_to_string error
  | `Database (operation, failure) ->
      describe_operation operation ^ ": " ^ Printexc.to_string failure
  | `State_conflict operation ->
      describe_operation operation
      ^ ": " ^ changed_concurrently
      ^ "; is another migration run in progress?"
  | `Failure_recording
      { migration_id; direction; failure; recording_failure } ->
      describe_operation (Migration { direction; phase = Run; migration_id })
      ^ " failed: "
      ^ Printexc.to_string failure
      ^ "; recording the failure also failed: "
      ^ (match recording_failure with
         | `Raised raised -> Printexc.to_string raised
         | `Unexpected_state -> changed_concurrently)
