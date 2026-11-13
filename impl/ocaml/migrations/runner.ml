type operation =
  | Create_state_table
  | Read_state
  | Begin_apply of Migration_id.t
  | Apply of Migration_id.t
  | Finish_apply of Migration_id.t
  | Begin_revert of Migration_id.t
  | Revert of Migration_id.t
  | Finish_revert of Migration_id.t

type failure_recording = {
  migration_id : Migration_id.t;
  operation : [ `Apply | `Revert ];
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

module type MIGRATIONS = sig
  type -'a connection
  type execute_response
  type 'a future

  module Migration : sig
    type t = private
      | Reversible of {
          id : Migration_id.t;
          apply : [ `WR ] connection -> execute_response future;
          revert : [ `WR ] connection -> execute_response future;
        }
      | Irreversible of {
          id : Migration_id.t;
          apply : [ `WR ] connection -> execute_response future;
        }
  end

  val migration_plan : Migration.t list
end

module type STATE_STORE = sig
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
    [ `RO | `WR ] connection -> (string list, error) result future

  val migrate :
    [ `RO | `WR ] connection -> (string list, error) result future

  val rollback :
    [ `RO | `WR ] connection -> (string list, error) result future
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
    (Store : STATE_STORE
               with type 'a connection := 'a T.connection
                and type 'a future := 'a T.IO.future)
    (Migrations : MIGRATIONS
                    with type 'a connection := 'a T.connection
                     and type execute_response := T.execute_response
                     and type 'a future := 'a T.IO.future) =
struct
  let planner_migrations =
    List.map
      (function
        | Migrations.Migration.Reversible { id; apply; revert } ->
            Planner.reversible id ~apply ~revert
        | Migrations.Migration.Irreversible { id; apply } ->
            Planner.irreversible id ~apply)
      Migrations.migration_plan

  let database operation work =
    T.IO.try_bind
      work
      (fun value -> T.IO.return (Ok value))
      (fun failure -> T.IO.return (Error (`Database (operation, failure))))

  let load_state connection =
    T.IO.(
      database Create_state_table (fun () ->
        Store.create_state_table connection)
      >>= function
      | Error error -> return (Error error)
      | Ok () ->
          database Read_state (fun () -> Store.list_rows connection)
          >>= function
          | Error error -> return (Error error)
          | Ok (Error error) -> return (Error (`State_row error))
          | Ok (Ok rows) -> return (Ok rows))

  let run_steps connection direction steps =
    let run_step (step : _ Planner.step) =
      let migration_id = step.migration_id in
      let ( begin_operation,
            begin_change,
            run_operation,
            finish_operation,
            finish_change,
            failure_change ) =
        match direction with
        | `Apply ->
            ( Begin_apply migration_id,
              Insert_applying { batch = step.batch },
              Apply migration_id,
              Finish_apply migration_id,
              Mark_applied,
              Mark_apply_failed )
        | `Revert ->
            ( Begin_revert migration_id,
              Mark_reverting,
              Revert migration_id,
              Finish_revert migration_id,
              Delete_reverting,
              Mark_revert_failed )
      in
      let change_state operation change =
        T.IO.(
          database operation (fun () ->
            Store.change connection migration_id change)
          >>= function
          | Error error -> return (Error error)
          | Ok `Changed -> return (Ok ())
          | Ok `Unexpected_state ->
              return (Error (`State_conflict operation)))
      in
      T.IO.(
        change_state begin_operation begin_change >>= function
        | Error error -> return (Error error)
        | Ok () ->
            try_bind
              (fun () -> step.run (connection :> [ `WR ] T.connection))
              (fun (_ : T.execute_response) ->
                change_state finish_operation finish_change)
              (fun failure ->
                let unrecorded recording_failure =
                  return
                    (Error
                       (`Failure_recording
                         {
                           migration_id;
                           operation = direction;
                           failure;
                           recording_failure;
                         }))
                in
                try_bind
                  (fun () ->
                    Store.change connection migration_id failure_change)
                  (function
                    | `Changed ->
                        return (Error (`Database (run_operation, failure)))
                    | `Unexpected_state -> unrecorded `Unexpected_state)
                  (fun raised -> unrecorded (`Raised raised))))
    in
    let rec loop = function
      | [] ->
          T.IO.return
            (Ok
               (List.map
                  (fun (step : _ Planner.step) ->
                    Migration_id.to_string step.migration_id)
                  steps))
      | step :: rest ->
          T.IO.(
            run_step step >>= function
            | Error error -> return (Error error)
            | Ok () -> loop rest)
    in
    loop steps

  let planned connection plan continue =
    T.IO.(
      load_state connection >>= function
      | Error error -> return (Error error)
      | Ok rows ->
          (match plan planner_migrations rows with
           | Error error -> return (Error (`Planner error))
           | Ok planned -> continue planned))

  let pending connection =
    planned connection Planner.pending (fun ids ->
      T.IO.return (Ok (List.map Migration_id.to_string ids)))

  let migrate connection =
    planned connection Planner.migrate (run_steps connection `Apply)

  let rollback connection =
    planned connection Planner.rollback (run_steps connection `Revert)
end

module Make
    (T : Sqlgg_traits.M_control_io)
    (Migrations : MIGRATIONS
                    with type 'a connection := 'a T.connection
                     and type execute_response := T.execute_response
                     and type 'a future := 'a T.IO.future) =
  Make_with_store
    (T)
    (struct
      let create_state_table connection =
        T.IO.(
          T.execute_unprepared
            connection
            (Sqlgg_query.make
               ~sql:
                 ("CREATE TABLE IF NOT EXISTS `sqlgg_migrations` (\n"
                  ^ "  `migration_id` VARCHAR("
                  ^ string_of_int Migration_id.max_length
                  ^ ") NOT NULL PRIMARY KEY,\n"
                  ^ "  `batch` BIGINT UNSIGNED NOT NULL,\n"
                  ^ "  `state` ENUM(\n"
                  ^ "    'applying',\n"
                  ^ "    'applied',\n"
                  ^ "    'reverting',\n"
                  ^ "    'apply_failed',\n"
                  ^ "    'revert_failed'\n"
                  ^ "  ) NOT NULL,\n"
                  ^ "  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\n"
                  ^ "  `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP\n"
                  ^ "    ON UPDATE CURRENT_TIMESTAMP\n"
                  ^ ")")
               ~name:"sqlgg_migrations_create_state_table"
               ~kind:(Sqlgg_query.Create "sqlgg_migrations")
               ())
          >>= fun (_ : T.execute_response) -> return ())

      let list_rows connection =
        let rows = ref (Ok []) in
        T.IO.(
          T.select
            connection
            (Sqlgg_query.make
               ~sql:
                 ("SELECT `migration_id`, CAST(`batch` AS CHAR), `state` "
                  ^ "FROM `sqlgg_migrations`")
               ~name:"sqlgg_migrations_list_rows"
               ~kind:(Sqlgg_query.Select Sqlgg_query.Nat)
               ())
            T.no_params
            (fun row ->
              rows :=
                Result.bind !rows (fun parsed ->
                  Result.map
                    (fun parsed_row -> parsed_row :: parsed)
                    (Migration_state.parse_row
                       ~id:(T.get_column_string row 0)
                       ~batch:(T.get_column_string row 1)
                       ~state:(T.get_column_string row 2))))
          >>= fun () -> return !rows)

      let change connection migration_id state_change =
        let id = `Text (Migration_id.to_string migration_id) in
        let state value = `Text (Migration_state.to_string value) in
        let transition ~expected ~next =
          ( Sqlgg_query.make
              ~sql:
                ("UPDATE `sqlgg_migrations` SET `state` = ? "
                 ^ "WHERE `migration_id` = ? AND `state` = ?")
              ~name:"sqlgg_migrations_transition_state"
              ~kind:(Sqlgg_query.Update (Some "sqlgg_migrations"))
              (),
            [ state next; id; state expected ] )
        in
        let query, parameters =
          match state_change with
          | Insert_applying { batch } ->
              ( Sqlgg_query.make
                  ~sql:
                    ("INSERT INTO `sqlgg_migrations` "
                     ^ "(`migration_id`, `batch`, `state`) "
                     ^ "VALUES (?, ?, ?)")
                  ~name:"sqlgg_migrations_insert_applying"
                  ~kind:(Sqlgg_query.Insert "sqlgg_migrations")
                  (),
                [ id; `Batch batch; state Migration_state.Applying ] )
          | Mark_applied ->
              transition
                ~expected:Migration_state.Applying
                ~next:Migration_state.Applied
          | Mark_reverting ->
              transition
                ~expected:Migration_state.Applied
                ~next:Migration_state.Reverting
          | Mark_apply_failed ->
              transition
                ~expected:Migration_state.Applying
                ~next:Migration_state.Apply_failed
          | Mark_revert_failed ->
              transition
                ~expected:Migration_state.Reverting
                ~next:Migration_state.Revert_failed
          | Delete_reverting ->
              ( Sqlgg_query.make
                  ~sql:
                    ("DELETE FROM `sqlgg_migrations` "
                     ^ "WHERE `migration_id` = ? AND `state` = ?")
                  ~name:"sqlgg_migrations_delete_reverting"
                  ~kind:(Sqlgg_query.Delete [ "sqlgg_migrations" ])
                  (),
                [ id; state Migration_state.Reverting ] )
        in
        T.IO.(
          T.execute connection query (fun statement ->
            let params =
              T.start_params statement (Stdlib.List.length parameters)
            in
            Stdlib.List.iter
              (function
                | `Text value -> T.set_param_string params value
                | `Batch batch ->
                    T.set_param_int64 params (Int64.of_int batch))
              parameters;
            T.finish_params params)
          >>= fun response ->
          if Int64.equal response.T.affected_rows 1L then
            return `Changed
          else
            return `Unexpected_state)
    end)
    (Migrations)

let error_to_string error =
  let describe_operation operation =
    let describe action migration_id =
      action ^ " " ^ Migration_id.to_string migration_id
    in
    match operation with
    | Create_state_table -> "create state table"
    | Read_state -> "read state"
    | Begin_apply migration_id -> describe "begin apply" migration_id
    | Apply migration_id -> describe "apply" migration_id
    | Finish_apply migration_id -> describe "finish apply" migration_id
    | Begin_revert migration_id -> describe "begin revert" migration_id
    | Revert migration_id -> describe "revert" migration_id
    | Finish_revert migration_id -> describe "finish revert" migration_id
  in
  match error with
  | `Planner error -> Planner.error_to_string error
  | `State_row error -> Migration_state.error_to_string error
  | `Database (operation, failure) ->
      describe_operation operation ^ ": " ^ Printexc.to_string failure
  | `State_conflict operation ->
      describe_operation operation
      ^ ": the state row changed concurrently; "
      ^ "is another migration run in progress?"
  | `Failure_recording
      { migration_id; operation; failure; recording_failure } ->
      (match operation with
       | `Apply -> "apply "
       | `Revert -> "revert ")
      ^ Migration_id.to_string migration_id
      ^ " failed: "
      ^ Printexc.to_string failure
      ^ "; recording the failure also failed: "
      ^ (match recording_failure with
         | `Raised raised -> Printexc.to_string raised
         | `Unexpected_state -> "the state row changed concurrently")
