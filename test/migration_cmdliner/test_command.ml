open OUnit2

let () =
  let module Migrations = struct
    let baseline = None

    let migration_plan =
      [ Fake_database.reversible "202611110001_create_users" ]
  end
  in
  let module Runner = Fake_runner.Linear (Migrations) in
  let fresh_databases () =
    [
      "empty", Fake_database.create [];
      ( "migrated",
        Fake_database.create
          [ "202611110001_create_users", 1, "applied" ] );
      ( "interrupted",
        Fake_database.create
          [ "202611110001_create_users", 1, "applying" ] );
    ]
  in
  let databases = ref (fresh_databases ()) in
  let module Command =
    Sqlgg_migrations_cmdliner.Command.Make_with_runner (Fake_database) (Runner)
      (struct
        type options = string
        type connection_error = { missing_database : string }

        let options =
          Cmdliner.Arg.(
            required
            & opt (some string) None
            & info [ "database" ])

        let connection_error_to_string { missing_database } =
          "database does not exist: " ^ missing_database

        let with_connection name run =
          match List.assoc_opt name !databases with
          | None -> Error { missing_database = name }
          | Some database ->
            Ok (Fake_database.completed (run database))
      end)
  in
  let run arguments =
    databases := fresh_databases ();
    let output = Buffer.create 64 in
    let formatter = Format.formatter_of_buffer output in
    let code =
      Cmdliner.Cmd.eval'
        ~help:formatter
        ~err:formatter
        ~argv:(Array.of_list ("app" :: arguments))
        Command.command
    in
    Format.pp_print_flush formatter ();
    code, Buffer.contents output
  in
  let state name =
    Fake_database.rows (List.assoc name !databases)
  in
  let mutations name =
    List.filter
      (function
        | Fake_database.Create_state_table | Fake_database.List_rows ->
            false
        | Fake_database.Insert_applying _
        | Fake_database.Apply _
        | Fake_database.Mark_applied _
        | Fake_database.Mark_reverting _
        | Fake_database.Revert _
        | Fake_database.Mark_apply_failed _
        | Fake_database.Mark_revert_failed _
        | Fake_database.Delete_reverting _ ->
            true)
      (Fake_database.operations (List.assoc name !databases))
  in
  let test_pending_reports_work_with_exit_one _ =
    let code, _ = run [ "pending"; "--database"; "empty" ] in
    assert_equal ~printer:string_of_int 1 code;
    assert_equal [] (mutations "empty")
  in
  let test_pending_is_zero_when_up_to_date _ =
    let code, _ = run [ "pending"; "--database"; "migrated" ] in
    assert_equal ~printer:string_of_int 0 code
  in
  let test_migrate_applies _ =
    let code, _ = run [ "migrate"; "--database"; "empty" ] in
    assert_equal ~printer:string_of_int 0 code;
    assert_equal
      [ "202611110001_create_users", 1, "applied" ]
      (state "empty")
  in
  let test_rollback_reverts _ =
    let code, _ = run [ "rollback"; "--database"; "migrated" ] in
    assert_equal ~printer:string_of_int 0 code;
    assert_equal [] (state "migrated")
  in
  let test_runner_error_is_exit_two _ =
    let code, _ = run [ "migrate"; "--database"; "interrupted" ] in
    assert_equal ~printer:string_of_int 2 code;
    assert_equal [] (mutations "interrupted")
  in
  let test_connection_error_is_exit_two _ =
    let code, _ = run [ "pending"; "--database"; "missing" ] in
    assert_equal ~printer:string_of_int 2 code
  in
  let test_help_lists_subcommands _ =
    let code, help = run [ "--help=plain" ] in
    assert_equal ~printer:string_of_int 0 code;
    List.iter
      (fun name ->
        assert_bool
          ("help mentions " ^ name)
          (List.exists
             (fun line ->
               String.starts_with
                 ~prefix:name
                 (String.trim line))
             (String.split_on_char '\n' help)))
      [ "migrate"; "pending"; "rollback" ]
  in
  let test_connection_options_are_required _ =
    let code, _ = run [ "migrate" ] in
    assert_equal
      ~printer:string_of_int
      Cmdliner.Cmd.Exit.cli_error
      code
  in
  run_test_tt_main
    ("migration command"
     >::: [
            "pending work" >:: test_pending_reports_work_with_exit_one;
            "nothing pending" >:: test_pending_is_zero_when_up_to_date;
            "migrate" >:: test_migrate_applies;
            "rollback" >:: test_rollback_reverts;
            "runner error" >:: test_runner_error_is_exit_two;
            "connection error" >:: test_connection_error_is_exit_two;
            "help" >:: test_help_lists_subcommands;
            "connection options" >:: test_connection_options_are_required;
          ])
