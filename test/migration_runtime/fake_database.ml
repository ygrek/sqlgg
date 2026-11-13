type operation =
  | Create_state_table
  | List_rows
  | Insert_applying of string * int
  | Apply of string
  | Mark_applied of string
  | Mark_reverting of string
  | Revert of string
  | Mark_apply_failed of string
  | Mark_revert_failed of string
  | Delete_reverting of string

type action_failure =
  | Fail_apply of string
  | Fail_apply_deferred of string
  | Fail_revert of string

type database_failure =
  | Fail_mark_apply_failed of string

type -'a connection =
  | Connection of {
      mutable rows : (string * int * string) list;
      mutable operations : operation list;
      mutable action_failure : action_failure option;
      mutable database_failure : database_failure option;
    }
type execute_response = Executed

module IO = struct
  type 'a future =
    | Completed of 'a
    | Failed of exn

  let return value = Completed value

  let ( >>= ) future next =
    match future with
    | Completed value -> next value
    | Failed failure -> Failed failure

  let try_bind work succeed recover =
    match work () with
    | Completed value -> succeed value
    | Failed failure -> recover failure
    | exception failure -> recover failure
end

let create rows =
  Connection
    {
      rows;
      operations = [];
      action_failure = None;
      database_failure = None;
    }

let operations (Connection state) =
  List.rev state.operations

let rows (Connection state) =
  state.rows

let fail_action (Connection state) failure =
  state.action_failure <- Some failure

let fail_database (Connection state) failure =
  state.database_failure <- Some failure

let create_state_table (Connection state) =
  state.operations <- Create_state_table :: state.operations;
  IO.return ()

let list_rows (Connection state) =
  state.operations <- List_rows :: state.operations;
  IO.return
    (List.fold_right
       (fun (id, batch, migration_state) parsed ->
         Result.bind parsed (fun rows ->
           Result.map
             (fun row -> row :: rows)
             (Sqlgg_migrations.Migration_state.parse_row
                ~id
                ~batch:(string_of_int batch)
                ~state:migration_state)))
       state.rows
       (Ok []))

let change
    (Connection state)
    migration_id
    (state_change : Sqlgg_migrations.Runner.state_change) =
  let id = Sqlgg_migrations.Migration_id.to_string migration_id in
  let transition operation ~expected next =
    state.operations <- operation :: state.operations;
    let expected_row (stored_id, _, stored_state) =
      String.equal stored_id id && String.equal stored_state expected
    in
    if List.exists expected_row state.rows then (
      state.rows <-
        List.filter_map
          (fun ((stored_id, batch, _) as row) ->
            if expected_row row then
              match next with
              | `Becomes replacement -> Some (stored_id, batch, replacement)
              | `Is_deleted -> None
            else
              Some row)
          state.rows;
      IO.return `Changed)
    else
      IO.return `Unexpected_state
  in
  match state_change with
  | Sqlgg_migrations.Runner.Insert_applying { batch } ->
      state.operations <- Insert_applying (id, batch) :: state.operations;
      state.rows <- state.rows @ [ id, batch, "applying" ];
      IO.return `Changed
  | Sqlgg_migrations.Runner.Mark_applied ->
      transition (Mark_applied id) ~expected:"applying" (`Becomes "applied")
  | Sqlgg_migrations.Runner.Mark_reverting ->
      transition
        (Mark_reverting id)
        ~expected:"applied"
        (`Becomes "reverting")
  | Sqlgg_migrations.Runner.Mark_apply_failed ->
      (match state.database_failure with
       | Some (Fail_mark_apply_failed failed_id)
         when String.equal id failed_id ->
           state.operations <- Mark_apply_failed id :: state.operations;
           raise (Failure ("mark apply failed: " ^ id))
       | None | Some (Fail_mark_apply_failed _) ->
           transition
             (Mark_apply_failed id)
             ~expected:"applying"
             (`Becomes "apply_failed"))
  | Sqlgg_migrations.Runner.Mark_revert_failed ->
      transition
        (Mark_revert_failed id)
        ~expected:"reverting"
        (`Becomes "revert_failed")
  | Sqlgg_migrations.Runner.Delete_reverting ->
      transition (Delete_reverting id) ~expected:"reverting" `Is_deleted

let run_apply (Connection state) id =
  state.operations <- Apply id :: state.operations;
  match state.action_failure with
  | Some (Fail_apply failed_id) when String.equal id failed_id ->
      raise (Failure ("apply: " ^ id))
  | Some (Fail_apply_deferred failed_id)
    when String.equal id failed_id ->
      IO.Failed (Failure ("deferred apply: " ^ id))
  | None
  | Some (Fail_apply _)
  | Some (Fail_apply_deferred _)
  | Some (Fail_revert _) ->
      IO.return Executed

let run_revert (Connection state) id =
  state.operations <- Revert id :: state.operations;
  match state.action_failure with
  | Some (Fail_revert failed_id) when String.equal id failed_id ->
      raise (Failure ("revert: " ^ id))
  | None
  | Some (Fail_apply _)
  | Some (Fail_apply_deferred _)
  | Some (Fail_revert _) ->
      IO.return Executed
