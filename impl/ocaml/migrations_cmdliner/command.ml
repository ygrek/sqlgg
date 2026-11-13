module Make
    (T : sig
       type -'a connection

       module IO : sig
         type 'a future
       end
     end)
    (Runner : Sqlgg_migrations.Runner.S
                with type 'a connection := 'a T.connection
                 and type 'a future := 'a T.IO.future)
    (Application : sig
       type options

       val options : options Cmdliner.Term.t

       val with_connection :
         options ->
         ([ `RO | `WR ] T.connection -> 'a T.IO.future) ->
         'a
     end) =
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
            match Application.with_connection options run with
            | Ok ids -> report ids
            | Error error ->
                prerr_endline
                  ("migrations " ^ name ^ ": "
                   ^ Sqlgg_migrations.Runner.error_to_string error);
                failure_exit)
          $ Application.options)
    in
    let report_done ~nothing ~verb ids =
      (match ids with
       | [] -> print_endline nothing
       | _ :: _ ->
           List.iter (fun id -> print_endline (verb ^ " " ^ id)) ids);
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
                List.iter print_endline ids;
                pending_exit);
      ]
end
