open OUnit2

let () =
  let module Fake_migration =
    Sqlgg_migrations.Migration.Make (Fake_database)
  in
  let reversible id =
    Fake_migration.make_generated_reversible
      ~id
      ~apply:(fun connection -> Fake_database.run_apply connection id)
      ~revert:(fun connection -> Fake_database.run_revert connection id)
  in
  let module Reversible_migrations = struct
    module Migration =
      Sqlgg_migrations.Migration.Make (Fake_database)

    let migration_plan =
      [
        reversible "202611110001_create_users";
        reversible "202611110002_add_email";
      ]
  end
  in
  let module Runner =
    Sqlgg_migrations.Runner.Make_with_store
      (Fake_database)
      (Fake_database)
      (Reversible_migrations)
  in
  let succeeded _ =
    Production_traits.IO.return
      ({
         affected_rows = 1L;
         insert_id = None;
       } : Production_traits.execute_response)
  in
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
      | Succeed -> succeeded ()
      | Fail -> raise (Failure ("production apply: " ^ id))
  end
  in
  let module Sorted_production_migrations = struct
    module Migration =
      Sqlgg_migrations.Migration.Make (Production_traits)

    let migration_plan =
      [
        Migration.make_generated_reversible
          ~id:"2_a"
          ~apply:succeeded
          ~revert:succeeded;
        Migration.make_generated_reversible
          ~id:"10_b"
          ~apply:succeeded
          ~revert:succeeded;
      ]
  end
  in
  let module Sorted_production_runner =
    Sqlgg_migrations.Runner.Make
      (Production_traits)
      (Sorted_production_migrations)
  in
  let module Production_migrations = struct
    module Migration =
      Sqlgg_migrations.Migration.Make (Production_traits)

    let migration_plan =
      [
        Migration.make_generated_reversible
          ~id:"2_a"
          ~apply:(fun _ -> Production_action.apply "2_a")
          ~revert:succeeded;
      ]
  end
  in
  let module Production_runner =
    Sqlgg_migrations.Runner.Make
      (Production_traits)
      (Production_migrations)
  in
  let completed = function
    | Fake_database.IO.Completed value -> value
    | Fake_database.IO.Failed failure -> raise failure
  in
  let expect_ok = function
    | Ok value -> value
    | Error error ->
        assert_failure
          (Sqlgg_migrations.Runner.error_to_string error)
  in
  let unexpected expectation = function
    | Ok (_ : string list) -> assert_failure ("expected " ^ expectation)
    | Error error ->
        assert_failure
          ("unexpected error: "
           ^ Sqlgg_migrations.Runner.error_to_string error)
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
      (expect_ok (completed (Runner.migrate connection)));
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
    (match completed (Runner.migrate connection) with
     | Error
         (`Database
           ( Sqlgg_migrations.Runner.Apply migration_id,
             Failure message )) ->
         assert_equal
           "202611110001_create_users"
           (Sqlgg_migrations.Migration_id.to_string migration_id);
         assert_equal
           "apply: 202611110001_create_users"
           message
     | outcome ->
         unexpected "apply failure" outcome);
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
    (match completed (Runner.migrate connection) with
     | Error
         (`Database
           ( Sqlgg_migrations.Runner.Apply migration_id,
             Failure message )) ->
         assert_equal
           "202611110001_create_users"
           (Sqlgg_migrations.Migration_id.to_string migration_id);
         assert_equal
           "deferred apply: 202611110001_create_users"
           message
     | outcome ->
         unexpected "deferred apply failure" outcome);
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
    match completed (Runner.migrate connection) with
    | Error
        (`Failure_recording
          {
            migration_id;
            operation = `Apply;
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
      (expect_ok (completed (Runner.rollback connection)));
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
    (match completed (Runner.rollback connection) with
     | Error
         (`Database
           ( Sqlgg_migrations.Runner.Revert migration_id,
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
      module Migration =
        Sqlgg_migrations.Migration.Make (Fake_database)

      let migration_plan =
        [
          reversible "202611110001_create_users";
          Migration.make_generated_irreversible
            ~id:"202611110002_drop_legacy"
            ~apply:(fun connection ->
              Fake_database.run_apply
                connection
                "202611110002_drop_legacy");
        ]
    end
    in
    let module Irreversible_runner =
      Sqlgg_migrations.Runner.Make_with_store
        (Fake_database)
        (Fake_database)
        (Irreversible_migrations)
    in
    let connection =
      Fake_database.create
        [
          "202611110001_create_users", 2, "applied";
          "202611110002_drop_legacy", 2, "applied";
        ]
    in
    (match completed (Irreversible_runner.rollback connection) with
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
      expect_ok (completed (Runner.pending connection))
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
                Production_traits.mock_text "1";
                Production_traits.mock_text "applied";
              ];
            Production_traits.make_mock_row
              [
                Production_traits.mock_text "2_a";
                Production_traits.mock_text "1";
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
         assert_equal
           ("SELECT `migration_id`, CAST(`batch` AS CHAR), `state` "
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
  let test_production_action_failure_is_caught _ =
    reset_production
      [
        executed 0L;
        Production_traits.MockSelect [];
        executed 1L;
        executed 1L;
      ]
      Production_action.Fail;
    (match Production_runner.migrate () with
     | Error
         (`Database
           ( Sqlgg_migrations.Runner.Apply migration_id,
             Failure message )) ->
         assert_equal
           "2_a"
           (Sqlgg_migrations.Migration_id.to_string migration_id);
         assert_equal "production apply: 2_a" message
     | outcome ->
         unexpected "production action failure" outcome);
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
          (Sqlgg_migrations.Runner.Finish_apply migration_id)) ->
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
            "production action catch"
            >:: test_production_action_failure_is_caught;
            "production affected rows"
            >:: test_production_rejects_unaffected_transition;
            "production traits exception"
            >:: test_production_traits_exception_is_caught;
          ])
