open OUnit2

let () =
  let module Reversible_migrations = struct
    let baseline = None

    let migration_plan =
      [
        Fake_database.reversible "202611110001_create_users";
        Fake_database.reversible "202611110002_add_email";
      ]
  end
  in
  let module Runner = Fake_runner.Linear (Reversible_migrations) in
  let executed affected_rows =
    Production_traits.MockExecute { affected_rows; insert_id = None }
  in
  let module Production_action = struct
    type behavior =
      | Succeed
      | Fail

    let behavior = ref Succeed
    let applied_ids = ref []

    let reset next_behavior =
      behavior := next_behavior;
      applied_ids := []

    let apply id =
      applied_ids := id :: !applied_ids;
      match !behavior with
      | Succeed -> ()
      | Fail -> raise (Failure ("production apply: " ^ id))
  end
  in
  let module Sorted_production_migrations (T : Sqlgg_traits.M_io) = struct
    let baseline = None

    let execute db =
      T.execute_unprepared
        db
        (Sqlgg_traits.Query.make
           ~sql:"SELECT 1"
           ~name:"sorted_production_migration"
           ~kind:Sqlgg_traits.Query.Other
           ())

    let migration_plan =
      [
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"2_a")
          ~apply:execute
          ~revert:execute;
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"10_b")
          ~apply:execute
          ~revert:execute;
      ]
  end
  in
  let module Sorted_production_runner =
    Sqlgg_migrations_state_table.Make_runner
      (Production_traits)
      (Sorted_production_migrations)
  in
  let module Sorted_production_command =
    Sqlgg_migrations_cmdliner.Command.Make
      (Production_traits)
      (Sorted_production_migrations)
      (struct
        type options = unit
        type connection_error = |

        let options = Cmdliner.Term.const ()

        let connection_error_to_string (error : connection_error) =
          match error with
          | _ -> .

        let with_connection () run = Ok (run ())
      end)
  in
  let (_ : Cmdliner.Cmd.Exit.code Cmdliner.Cmd.t) =
    Sorted_production_command.command
  in
  let module Production_migrations (T : Sqlgg_traits.M_io) = struct
    let baseline = None

    let succeeded (_ : [ `WR ] T.connection) =
      T.IO.return { T.affected_rows = 1L; insert_id = None }

    let migration_plan =
      [
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"2_a")
          ~apply:(fun connection ->
            Production_action.apply "2_a";
            succeeded connection)
          ~revert:succeeded;
      ]
  end
  in
  let module Production_runner =
    Sqlgg_migrations_state_table.Make_runner
      (Production_traits)
      (Production_migrations)
  in
  let expect_ok = function
    | Ok ids -> List.map Sqlgg_migrations.Migration_id.to_string ids
    | Error error ->
        assert_failure
          (Sqlgg_migrations.Runner.error_to_string error)
  in
  let unexpected expectation = function
    | Ok (_ : Sqlgg_migrations.Migration_id.t list) -> assert_failure ("expected " ^ expectation)
    | Error error ->
        assert_failure
          ("unexpected error: "
           ^ Sqlgg_migrations.Runner.error_to_string error)
  in
  let expect_apply_failure expectation ~id ~message = function
    | Error
        (`Database
          ( Sqlgg_migrations.Runner.Migration
              { direction = Apply; phase = Run; migration_id },
            Failure failure )) ->
        assert_equal
          id
          (Sqlgg_migrations.Migration_id.to_string migration_id);
        assert_equal message failure
    | outcome -> unexpected expectation outcome
  in
  let reset_production responses behavior =
    Production_traits.response_queue := responses;
    Production_traits.reset_mock_stats ();
    Production_traits.reset_bound_statements ();
    Production_action.reset behavior
  in
  let test_migrate_operation_sequence _ =
    let connection = Fake_database.create [] in
    assert_equal
      [ "202611110001_create_users"; "202611110002_add_email" ]
      (expect_ok (Fake_database.completed (Runner.migrate connection)));
    assert_equal
      [
        Fake_database.Create_state_table;
        Fake_database.List_rows;
        Fake_database.Insert_applying
          ("202611110001_create_users", 1);
        Fake_database.Apply "202611110001_create_users";
        Fake_database.Mark_applied "202611110001_create_users";
        Fake_database.Insert_applying
          ("202611110002_add_email", 1);
        Fake_database.Apply "202611110002_add_email";
        Fake_database.Mark_applied "202611110002_add_email";
      ]
      (Fake_database.operations connection)
  in
  let test_apply_failure_records_failed_state _ =
    let connection = Fake_database.create [] in
    Fake_database.fail_action
      connection
      (Fake_database.Fail_apply "202611110001_create_users");
    expect_apply_failure
      "apply failure"
      ~id:"202611110001_create_users"
      ~message:"apply: 202611110001_create_users"
      (Fake_database.completed (Runner.migrate connection));
    assert_equal
      [
        Fake_database.Create_state_table;
        Fake_database.List_rows;
        Fake_database.Insert_applying
          ("202611110001_create_users", 1);
        Fake_database.Apply "202611110001_create_users";
        Fake_database.Mark_apply_failed
          "202611110001_create_users";
      ]
      (Fake_database.operations connection);
    assert_equal
      [
        "202611110001_create_users", 1, "apply_failed";
      ]
      (Fake_database.rows connection)
  in
  let test_deferred_apply_failure_is_caught _ =
    let connection = Fake_database.create [] in
    Fake_database.fail_action
      connection
      (Fake_database.Fail_apply_deferred
         "202611110001_create_users");
    expect_apply_failure
      "deferred apply failure"
      ~id:"202611110001_create_users"
      ~message:"deferred apply: 202611110001_create_users"
      (Fake_database.completed (Runner.migrate connection));
    assert_equal
      [
        "202611110001_create_users", 1, "apply_failed";
      ]
      (Fake_database.rows connection)
  in
  let test_recording_failure_preserves_both_failures _ =
    let connection = Fake_database.create [] in
    Fake_database.fail_action
      connection
      (Fake_database.Fail_apply "202611110001_create_users");
    Fake_database.fail_database
      connection
      (Fake_database.Fail_mark_apply_failed
         "202611110001_create_users");
    match Fake_database.completed (Runner.migrate connection) with
    | Error
        (`Failure_recording
          {
            migration_id;
            direction = Apply;
            failure = Failure action_message;
            recording_failure = `Raised (Failure recording_message);
          }) ->
        assert_equal
          "202611110001_create_users"
          (Sqlgg_migrations.Migration_id.to_string migration_id);
        assert_equal
          "apply: 202611110001_create_users"
          action_message;
        assert_equal
          "mark apply failed: 202611110001_create_users"
          recording_message
    | outcome ->
        unexpected "recording failure" outcome
  in
  let test_rollback_reverses_and_deletes_latest_batch _ =
    let connection =
      Fake_database.create
        [
          "202611110001_create_users", 2, "applied";
          "202611110002_add_email", 2, "applied";
        ]
    in
    assert_equal
      [ "202611110002_add_email"; "202611110001_create_users" ]
      (expect_ok (Fake_database.completed (Runner.rollback connection)));
    assert_equal
      [
        Fake_database.Create_state_table;
        Fake_database.List_rows;
        Fake_database.Mark_reverting
          "202611110002_add_email";
        Fake_database.Revert "202611110002_add_email";
        Fake_database.Delete_reverting
          "202611110002_add_email";
        Fake_database.Mark_reverting
          "202611110001_create_users";
        Fake_database.Revert "202611110001_create_users";
        Fake_database.Delete_reverting
          "202611110001_create_users";
      ]
      (Fake_database.operations connection);
    assert_equal [] (Fake_database.rows connection)
  in
  let test_revert_failure_keeps_failed_row _ =
    let connection =
      Fake_database.create
        [
          "202611110001_create_users", 1, "applied";
          "202611110002_add_email", 2, "applied";
        ]
    in
    Fake_database.fail_action
      connection
      (Fake_database.Fail_revert "202611110002_add_email");
    (match Fake_database.completed (Runner.rollback connection) with
     | Error
         (`Database
           ( Sqlgg_migrations.Runner.Migration
               { direction = Revert; phase = Run; migration_id },
             Failure _ )) ->
         assert_equal
           "202611110002_add_email"
           (Sqlgg_migrations.Migration_id.to_string migration_id)
     | outcome ->
         unexpected "revert failure" outcome);
    assert_equal
      [
        "202611110001_create_users", 1, "applied";
        "202611110002_add_email", 2, "revert_failed";
      ]
      (Fake_database.rows connection)
  in
  let test_irreversible_preflight_has_no_rollback_mutation _ =
    let module Irreversible_migrations = struct
      let baseline = None

      let migration_plan =
        [
          Fake_database.reversible "202611110001_create_users";
          Sqlgg_migrations.Migration.irreversible
            (Sqlgg_migrations.Migration_id.parse_generated
               ~id:"202611110002_drop_legacy")
            ~apply:(fun connection ->
              Fake_database.run_apply
                connection
                "202611110002_drop_legacy");
        ]
    end
    in
    let module Irreversible_runner =
      Fake_runner.Linear (Irreversible_migrations)
    in
    let connection =
      Fake_database.create
        [
          "202611110001_create_users", 2, "applied";
          "202611110002_drop_legacy", 2, "applied";
        ]
    in
    (match Fake_database.completed (Irreversible_runner.rollback connection) with
     | Error (`Planner (`Irreversible migration_id)) ->
         assert_equal
           "202611110002_drop_legacy"
           (Sqlgg_migrations.Migration_id.to_string migration_id)
     | outcome ->
         unexpected "irreversible error" outcome);
    assert_equal
      [
        Fake_database.Create_state_table;
        Fake_database.List_rows;
      ]
      (Fake_database.operations connection)
  in
  let test_pending_reads_without_state_mutation _ =
    let connection =
      Fake_database.create
        [
          "202611110001_create_users", 1, "applied";
        ]
    in
    let pending =
      expect_ok (Fake_database.completed (Runner.pending connection))
    in
    assert_equal
      [ "202611110002_add_email" ]
      pending;
    assert_equal
      [
        Fake_database.Create_state_table;
        Fake_database.List_rows;
      ]
      (Fake_database.operations connection)
  in
  let test_production_rows_use_canonical_id_order _ =
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect
          [
            Production_traits.make_mock_row
              [
                Production_traits.mock_text "10_b";
                Production_traits.mock_int 1L;
                Production_traits.mock_text "applied";
              ];
            Production_traits.make_mock_row
              [
                Production_traits.mock_text "2_a";
                Production_traits.mock_int 1L;
                Production_traits.mock_text "applied";
              ];
          ];
      ]
      Production_action.Succeed;
    assert_equal [] (expect_ok (Sorted_production_runner.pending ()))
  in
  let test_production_sql_and_bound_transitions _ =
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect [];
        executed 1L;
        executed 1L;
      ]
      Production_action.Succeed;
    assert_equal [ "2_a" ] (expect_ok (Production_runner.migrate ()));
    let sql =
      List.rev_map
        (fun (statement : Production_traits.statement) ->
          statement.sql)
        !(Production_traits.prepared_statements)
    in
    (match sql with
     | create :: select :: insert :: transition :: [] ->
         assert_bool
           "state table schema query"
           (String.starts_with
              ~prefix:
                "CREATE TABLE IF NOT EXISTS `sqlgg_migrations`"
              create);
         let columns =
           List.map String.trim (String.split_on_char '\n' create)
         in
         assert_bool
           "the id column is as wide as the longest id"
           (List.mem
              ("`migration_id` VARBINARY("
               ^ string_of_int Sqlgg_migrations.Migration_id.max_length
               ^ ") NOT NULL PRIMARY KEY,")
              columns);
         assert_bool
           "every OCaml batch fits the database column"
           (List.mem "`batch` BIGINT NOT NULL," columns);
         assert_equal
           ("SELECT `migration_id`, `batch`, `state` "
            ^ "FROM `sqlgg_migrations`")
           select;
         assert_equal
           ("INSERT INTO `sqlgg_migrations` "
            ^ "(`migration_id`, `batch`, `state`) "
            ^ "VALUES (?, ?, ?)")
           insert;
         assert_equal
           ("UPDATE `sqlgg_migrations` SET `state` = ? "
            ^ "WHERE `migration_id` = ? AND `state` = ?")
           transition
     | _ -> assert_failure "unexpected production SQL sequence");
    assert_equal
      [
        {
          Production_traits.sql =
            ("INSERT INTO `sqlgg_migrations` "
             ^ "(`migration_id`, `batch`, `state`) "
             ^ "VALUES (?, ?, ?)");
          params = [ "'2_a'"; "1"; "'applying'" ];
        };
        {
          Production_traits.sql =
            ("UPDATE `sqlgg_migrations` SET `state` = ? "
             ^ "WHERE `migration_id` = ? AND `state` = ?");
          params = [ "'applied'"; "'2_a'"; "'applying'" ];
        };
      ]
      (Production_traits.bound_statements ())
  in
  let test_configured_state_table_name_reaches_every_query _ =
    let module Custom_table_runner =
      Sqlgg_migrations.Runner.Make_with_store
        (Production_traits)
        (Sqlgg_migrations_state_table.Make_state_store
           (Production_traits)
           (struct
             let state_table_name = "custom_migrations"
           end))
        (struct
          let history = Sqlgg_migrations.Planner.Linear
        end)
        (Production_migrations (Production_traits))
    in
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect [];
        executed 1L;
        executed 1L;
        executed 0L;
        Production_traits.MockSelect
          [
            Production_traits.make_mock_row
              [
                Production_traits.mock_text "2_a";
                Production_traits.mock_int 1L;
                Production_traits.mock_text "applied";
              ];
          ];
        executed 1L;
        executed 1L;
      ]
      Production_action.Succeed;
    assert_equal [ "2_a" ] (expect_ok (Custom_table_runner.migrate ()));
    assert_equal [ "2_a" ] (expect_ok (Custom_table_runner.rollback ()));
    assert_equal
      [
        "CREATE";
        "SELECT";
        "INSERT";
        "UPDATE";
        "CREATE";
        "SELECT";
        "UPDATE";
        "DELETE";
      ]
      (List.rev_map
         (fun (statement : Production_traits.statement) ->
           String.sub statement.sql 0 6)
         !(Production_traits.prepared_statements));
    List.iter
      (fun (statement : Production_traits.statement) ->
        let fragments = String.split_on_char '`' statement.sql in
        assert_bool
          statement.sql
          (List.mem "custom_migrations" fragments);
        assert_bool
          statement.sql
          (not (List.mem "sqlgg_migrations" fragments)))
      !(Production_traits.prepared_statements)
  in
  let test_parallel_branches_runner_applies_late_migration _ =
    let module Parallel_branches_runner =
      Sqlgg_migrations.Runner.Make_with_store
        (Fake_database)
        (Fake_database)
        (struct
          let history = Sqlgg_migrations.Planner.Interleaved
        end)
        (Reversible_migrations)
    in
    let connection =
      Fake_database.create [ "202611110002_add_email", 4, "applied" ]
    in
    assert_equal
      [ "202611110001_create_users" ]
      (expect_ok
         (Fake_database.completed
            (Parallel_branches_runner.migrate connection)));
    assert_equal
      [
        "202611110002_add_email", 4, "applied";
        "202611110001_create_users", 5, "applied";
      ]
      (Fake_database.rows connection)
  in
  let test_same_timestamp_migrations_run_in_plan_order _ =
    let module Parent_then_child_migrations = struct
      let baseline = None

      let migration_plan =
        [
          Fake_database.reversible "20260710000000_create_jobs";
          Fake_database.reversible "20260710000000_create_job_items";
        ]
    end
    in
    let module Parent_then_child_runner =
      Fake_runner.Linear (Parent_then_child_migrations)
    in
    let connection = Fake_database.create [] in
    let printer = String.concat ", " in
    assert_equal
      ~printer
      [ "20260710000000_create_jobs"; "20260710000000_create_job_items" ]
      (expect_ok
         (Fake_database.completed
            (Parent_then_child_runner.migrate connection)));
    assert_equal
      ~printer
      [ "20260710000000_create_job_items"; "20260710000000_create_jobs" ]
      (expect_ok
         (Fake_database.completed
            (Parent_then_child_runner.rollback connection)));
    assert_equal
      ~printer
      [
        "apply 20260710000000_create_jobs";
        "apply 20260710000000_create_job_items";
        "revert 20260710000000_create_job_items";
        "revert 20260710000000_create_jobs";
      ]
      (List.filter_map
         (function
           | Fake_database.Apply id -> Some ("apply " ^ id)
           | Fake_database.Revert id -> Some ("revert " ^ id)
           | Fake_database.Create_state_table
           | Fake_database.List_rows
           | Fake_database.Insert_applying _
           | Fake_database.Mark_applied _
           | Fake_database.Mark_reverting _
           | Fake_database.Mark_apply_failed _
           | Fake_database.Mark_revert_failed _
           | Fake_database.Delete_reverting _ -> None)
         (Fake_database.operations connection))
  in
  let test_baseline_runner_skips_squashed_rows_and_advances_batch _ =
    let module Squashed_migrations = struct
      let baseline =
        Some
          (Sqlgg_migrations.Migration_id.parse_generated
             ~id:"202611110001_create_users")

      let migration_plan =
        [ Fake_database.reversible "202611110002_add_email" ]
    end
    in
    let module Squashed_runner = Fake_runner.Linear (Squashed_migrations) in
    let connection =
      Fake_database.create
        [ "202611110001_create_users", 3, "applied" ]
    in
    assert_equal
      [ "202611110002_add_email" ]
      (expect_ok (Fake_database.completed (Squashed_runner.migrate connection)));
    assert_equal
      [
        "202611110001_create_users", 3, "applied";
        "202611110002_add_email", 4, "applied";
      ]
      (Fake_database.rows connection)
  in
  let test_production_action_failure_is_caught _ =
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect [];
        executed 1L;
        executed 1L;
      ]
      Production_action.Fail;
    expect_apply_failure
      "production action failure"
      ~id:"2_a"
      ~message:"production apply: 2_a"
      (Production_runner.migrate ());
    match List.rev (Production_traits.bound_statements ()) with
    | failed_transition :: _ ->
        assert_equal
          [ "'apply_failed'"; "'2_a'"; "'applying'" ]
          failed_transition.Production_traits.params
    | [] -> assert_failure "missing failure transition"
  in
  let test_production_rejects_unaffected_transition _ =
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect [];
        executed 1L;
        executed 0L;
      ]
      Production_action.Succeed;
    match Production_runner.migrate () with
    | Error
        (`State_conflict
          (Sqlgg_migrations.Runner.Migration
             { direction = Apply; phase = Finish; migration_id })) ->
        assert_equal
          "2_a"
          (Sqlgg_migrations.Migration_id.to_string migration_id);
        assert_equal [ "2_a" ] !(Production_action.applied_ids)
    | outcome ->
        unexpected "affected-row rejection" outcome
  in
  let test_production_traits_exception_is_caught _ =
    reset_production
      [ Production_traits.MockError "state table unavailable" ]
      Production_action.Succeed;
    match Production_runner.pending () with
    | Error
        (`Database
          ( Sqlgg_migrations.Runner.Create_state_table,
            Production_traits.Oops message )) ->
        assert_equal "state table unavailable" message
    | outcome ->
        unexpected "traits exception" outcome
  in
  run_test_tt_main
    ("migration runner"
     >::: [
            "migrate sequence"
            >:: test_migrate_operation_sequence;
            "apply failure"
            >:: test_apply_failure_records_failed_state;
            "deferred apply failure"
            >:: test_deferred_apply_failure_is_caught;
            "failure recording failure"
            >:: test_recording_failure_preserves_both_failures;
            "rollback reverse order"
            >:: test_rollback_reverses_and_deletes_latest_batch;
            "revert failure"
            >:: test_revert_failure_keeps_failed_row;
            "irreversible preflight"
            >:: test_irreversible_preflight_has_no_rollback_mutation;
            "pending"
            >:: test_pending_reads_without_state_mutation;
            "production canonical row order"
            >:: test_production_rows_use_canonical_id_order;
            "production SQL and params"
            >:: test_production_sql_and_bound_transitions;
            "configured state table name"
            >:: test_configured_state_table_name_reaches_every_query;
            "parallel branches runner applies late migration"
            >:: test_parallel_branches_runner_applies_late_migration;
            "same timestamp runs in plan order"
            >:: test_same_timestamp_migrations_run_in_plan_order;
            "baseline runner"
            >:: test_baseline_runner_skips_squashed_rows_and_advances_batch;
            "production action catch"
            >:: test_production_action_failure_is_caught;
            "production affected rows"
            >:: test_production_rejects_unaffected_transition;
            "production traits exception"
            >:: test_production_traits_exception_is_caught;
          ])
