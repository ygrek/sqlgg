module type Connection = sig
  type -'a connection
  type 'a future
  type options
  type connection_error

  val options : options Cmdliner.Term.t

  val connection_error_to_string : connection_error -> string

  val with_connection :
    options ->
    ([ `RO | `WR ] connection -> 'a future) ->
    ('a, connection_error) result
end

module Make_with_runner
    (T : sig
       type -'a connection

       module IO : sig
         type 'a future
       end
     end)
    (Runner : Sqlgg_migrations.Runner.S
                with type 'a connection := 'a T.connection
                 and type 'a future := 'a T.IO.future)
    (Connection : Connection
                    with type 'a connection := 'a T.connection
                     and type 'a future := 'a T.IO.future) =
struct
  let command =
    let open Cmdliner in
    let pending_exit = 1 in
    let failure_exit = 2 in
    let subcommand name ~doc run report =
      Cmd.v
        (Cmd.info name ~doc)
        Term.(
          const (fun options ->
            let failed message =
              prerr_endline ("migrations " ^ name ^ ": " ^ message);
              failure_exit
            in
            match Connection.with_connection options run with
            | Error error ->
                failed (Connection.connection_error_to_string error)
            | Ok (Ok ids) -> report ids
            | Ok (Error error) ->
                failed (Sqlgg_migrations.Runner.error_to_string error))
          $ Connection.options)
    in
    let report_done ~nothing ~verb ids =
      (match ids with
       | [] -> print_endline nothing
       | _ :: _ ->
           List.iter
             (fun id ->
               print_endline
                 (verb ^ " " ^ Sqlgg_migrations.Migration_id.to_string id))
             ids);
      Cmd.Exit.ok
    in
    Cmd.group
      (Cmd.info "migrations"
         ~doc:"Apply and revert database schema migrations."
         ~exits:
           (Cmd.Exit.info pending_exit
              ~doc:"on $(b,pending), when there are migrations to apply."
            :: Cmd.Exit.info failure_exit
                 ~doc:
                   "when a migration failed or the migration state could \
                    not be read or changed."
            :: Cmd.Exit.defaults))
      [
        subcommand "migrate"
          ~doc:"Apply every pending migration as one batch."
          Runner.migrate
          (report_done ~nothing:"nothing to migrate" ~verb:"applied");
        subcommand "rollback"
          ~doc:"Revert the most recently applied batch."
          Runner.rollback
          (report_done ~nothing:"nothing to roll back" ~verb:"reverted");
        subcommand "pending"
          ~doc:"List the migrations that are not applied yet."
          Runner.pending
          (function
            | [] -> Cmd.Exit.ok
            | _ :: _ as ids ->
                List.iter
                  (fun id ->
                    print_endline (Sqlgg_migrations.Migration_id.to_string id))
                  ids;
                pending_exit);
      ]
end

module Make
    (Db : Sqlgg_traits.M_control_io)
    (Migrations : Sqlgg_migrations_state_table.Generated_migrations)
    (Connection : Connection
                    with type 'a connection := 'a Db.connection
                     and type 'a future := 'a Db.IO.future) =
  Make_with_runner
    (Db)
    (Sqlgg_migrations_state_table.Make_runner (Db) (Migrations))
    (Connection)
