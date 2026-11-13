

type operation =
  | Create_state_table
  | Read_state
  | Begin_apply of Migration_id.t
  | Apply of Migration_id.t
  | Finish_apply of Migration_id.t
  | Begin_revert of Migration_id.t
  | Revert of Migration_id.t
  | Finish_revert of Migration_id.t

type failure_recording = private {
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
    (_ : STATE_STORE
           with type 'a connection := 'a T.connection
            and type 'a future := 'a T.IO.future)
    (_ : MIGRATIONS
           with type 'a connection := 'a T.connection
            and type execute_response := T.execute_response
            and type 'a future := 'a T.IO.future) :
  S
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future

module Make
    (T : Sqlgg_traits.M_control_io)
    (_ : MIGRATIONS
           with type 'a connection := 'a T.connection
            and type execute_response := T.execute_response
            and type 'a future := 'a T.IO.future) :
  S
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future

val error_to_string : error -> string
