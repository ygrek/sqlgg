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

type failure_recording = private {
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
    (_ : State_store
           with type 'a connection := 'a T.connection
            and type 'a future := 'a T.IO.future)
    (_ : sig
       val history : Planner.history
     end)
    (_ : Migrations
           with type 'a connection := 'a T.connection
            and type execute_response := T.execute_response
            and type 'a future := 'a T.IO.future) :
  S
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future

val error_to_string : error -> string
