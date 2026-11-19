module Make_state_store
    (T : Sqlgg_traits.M_io)
    (Config : sig
       val state_table_name : string
     end) :
  Sqlgg_migrations.Runner.State_store
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future =
struct
  module Queries = State_table_queries.State_table_queries (T)

  let create_state_table connection =
    T.IO.(
      Queries.create_state_table connection
        ~state_table:Config.state_table_name
      >>= fun (_ : T.execute_response) -> return ())

  let list_rows connection =
    Queries.Fold.list_rows connection
      ~state_table:Config.state_table_name
      (fun ~migration_id ~batch ~state rows ->
        Result.bind rows (fun rows ->
          Result.map
            (fun row -> row :: rows)
            (Sqlgg_migrations.Migration_state.parse_row
               ~id:migration_id
               ~batch
               ~state)))
      (Ok [])

  let change connection migration_id state_change =
    let open Sqlgg_migrations in
    let transition ~expected ~next =
      Queries.transition_state connection
        ~state_table:Config.state_table_name
        ~next
        ~migration_id
        ~expected
    in
    let changed =
      match state_change with
      | Runner.Insert_applying { batch } ->
          Queries.insert_applying connection
            ~state_table:Config.state_table_name
            ~migration_id
            ~batch
            ~state:Migration_state.Applying
      | Runner.Mark_applied ->
          transition
            ~expected:Migration_state.Applying
            ~next:Migration_state.Applied
      | Runner.Mark_reverting ->
          transition
            ~expected:Migration_state.Applied
            ~next:Migration_state.Reverting
      | Runner.Mark_apply_failed ->
          transition
            ~expected:Migration_state.Applying
            ~next:Migration_state.Apply_failed
      | Runner.Mark_revert_failed ->
          transition
            ~expected:Migration_state.Reverting
            ~next:Migration_state.Revert_failed
      | Runner.Delete_reverting ->
          Queries.delete_reverting connection
            ~state_table:Config.state_table_name
            ~migration_id
            ~state:Migration_state.Reverting
    in
    T.IO.(
      changed
      >>= fun response ->
      if Int64.equal response.T.affected_rows 1L then
        return `Changed
      else
        return `Unexpected_state)
end

module type Generated_migrations = functor (T : Sqlgg_traits.M_io) ->
  Sqlgg_migrations.Runner.Migrations
    with type 'a connection := 'a T.connection
     and type execute_response := T.execute_response
     and type 'a future := 'a T.IO.future

module Make_runner
    (Db : Sqlgg_traits.M_control_io)
    (Migrations : Generated_migrations) =
  Sqlgg_migrations.Runner.Make_with_store
    (Db)
    (Make_state_store
       (Db)
       (struct
         let state_table_name = "sqlgg_migrations"
       end))
    (struct
      let history = Sqlgg_migrations.Planner.Linear
    end)
    (Migrations (Db))
