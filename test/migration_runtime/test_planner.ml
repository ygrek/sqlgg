open OUnit2

let () =
  let id raw =
    match Sqlgg_migrations.Migration_id.parse raw with
    | Ok id -> id
    | Error error ->
        assert_failure
          (Sqlgg_migrations.Migration_id.error_to_string error)
  in
  let migration raw_id reversibility =
    match reversibility with
    | `Reversible ->
        Sqlgg_migrations.Migration.reversible
          (id raw_id)
          ~apply:`Run_apply
          ~revert:`Run_revert
    | `Irreversible ->
        Sqlgg_migrations.Migration.irreversible (id raw_id) ~apply:`Run_apply
  in
  let step_ids steps =
    List.map
      (fun (step : _ Sqlgg_migrations.Planner.step) ->
        Sqlgg_migrations.Migration_id.to_string step.migration_id)
      steps
  in
  let row id batch state =
    match
      Sqlgg_migrations.Migration_state.parse_row
        ~id
        ~batch:(Int64.of_int batch)
        ~state
    with
    | Ok row -> row
    | Error error ->
        assert_failure
          (Sqlgg_migrations.Migration_state.error_to_string error)
  in
  let planned = function
    | Ok plan -> plan
    | Error error ->
        assert_failure (Sqlgg_migrations.Planner.error_to_string error)
  in
  let expect_planner_error expected outcome =
    assert_equal
      ~printer:(function
        | Ok ids -> "planned " ^ String.concat ", " ids
        | Error error -> Sqlgg_migrations.Planner.error_to_string error)
      (Error expected)
      (Result.map step_ids outcome)
  in
  let migrations () =
    [
      migration "202611110001_create_users" `Reversible;
      migration "202611110002_add_email" `Reversible;
      migration "202611110003_drop_legacy" `Irreversible;
    ]
  in
  let test_rejects_oversized_id _ =
    match
      Sqlgg_migrations.Migration_id.parse (String.make 192 'a')
    with
    | Error (`Too_long 192) -> ()
    | Ok _ | Error _ -> assert_failure "expected Too_long"
  in
  let test_accepts_id_at_byte_limit _ =
    let raw = "1_" ^ String.make 189 'a' in
    match Sqlgg_migrations.Migration_id.parse raw with
    | Ok id ->
        assert_equal
          raw
          (Sqlgg_migrations.Migration_id.to_string id)
    | Error _ -> assert_failure "expected valid 191-byte id"
  in
  let test_rejects_invalid_id_grammar _ =
    List.iter
      (fun raw ->
        match Sqlgg_migrations.Migration_id.parse raw with
        | Error (`Invalid invalid) when String.equal invalid raw -> ()
        | Ok _ | Error _ -> assert_failure ("expected Invalid for " ^ raw))
      [
        "create_users";
        "7_";
        " 7_a";
        "7_bad name";
        "7_bad.name";
        "7_bad\\name";
        "9223372036854775808_overflow";
      ]
  in
  let test_id_keeps_its_text _ =
    List.iter
      (fun raw ->
        assert_equal
          ~printer:Fun.id
          raw
          (Sqlgg_migrations.Migration_id.to_string (id raw)))
      [
        "0001_init";
        "030820241_add_languages";
        "7_Az-09_under_score";
        "7__leading_underscore";
        "20261113";
        "9223372036854775807_max";
      ];
    assert_equal
      ~printer:string_of_int
      0
      (Sqlgg_migrations.Migration_id.compare_position (id "01_a") (id "1_a"));
    assert_bool
      "exact ids have a total order"
      (Sqlgg_migrations.Migration_id.compare_order (id "01_a") (id "1_a")
       <> 0)
  in
  let made ~ordinal ~name =
    match Sqlgg_migrations.Migration_id.make ~ordinal ~name with
    | Ok made -> made
    | Error error ->
        assert_failure
          (Sqlgg_migrations.Migration_id.error_to_string error)
  in
  let test_made_id_has_ordinal_and_name _ =
    List.iter
      (fun (ordinal, name, expected) ->
        assert_equal
          ~printer:Fun.id
          expected
          (Sqlgg_migrations.Migration_id.to_string (made ~ordinal ~name)))
      [
        20261113L, Some "add_email", "20261113_add_email";
        20261113L, None, "20261113";
      ];
    (match
       Sqlgg_migrations.Migration_id.make
         ~ordinal:1L
         ~name:(Some (String.make 190 'a'))
     with
     | Error (`Too_long 192) -> ()
     | Ok _ | Error _ -> assert_failure "expected Too_long");
    match Sqlgg_migrations.Migration_id.make ~ordinal:(-1L) ~name:None with
    | Error (`Invalid "-1") -> ()
    | Ok _ | Error _ -> assert_failure "expected Invalid"
  in
  let test_id_ordinal_and_name _ =
    List.iter
      (fun (raw, ordinal, name) ->
        assert_equal
          ~printer:Int64.to_string
          ordinal
          (Sqlgg_migrations.Migration_id.ordinal (id raw));
        assert_equal
          name
          (Sqlgg_migrations.Migration_id.name (id raw)))
      [
        "0001_init", 1L, Some "init";
        "20261113_add_user_email", 20261113L, Some "add_user_email";
        "20261113", 20261113L, None;
      ]
  in
  let test_next_position_sorts_right_after_the_id _ =
    List.iter
      (fun (raw, next) ->
        assert_equal
          (Ok next)
          (Sqlgg_migrations.Migration_id.next_position (id raw));
        assert_bool
          raw
          (Sqlgg_migrations.Migration_id.compare_position
             (id raw)
             (made ~ordinal:next ~name:None)
           < 0))
      [
        "7_a", 8L;
        "20261113_a", 20261113000001L;
        "20261113120000_a", 20261113120001L;
      ];
    assert_equal
      (Error `Position_overflow)
      (Sqlgg_migrations.Migration_id.next_position
         (made ~ordinal:Int64.max_int ~name:None))
  in
  let test_id_order _ =
    List.iter
      (fun (left, right, expected) ->
        assert_equal
          ~msg:(left ^ " against " ^ right)
          ~printer:string_of_int
          expected
          (Int.compare
             (Sqlgg_migrations.Migration_id.compare_position
                (id left)
                (id right))
             0))
      [
        "2_a", "10_b", -1;
        "9999999_a", "20261113_b", -1;
        "20261113_a", "20261113000000_b", 0;
        "20261113_a", "20261112235959_b", 1;
      ]
  in
  let test_parses_positive_batch _ =
    let parsed = row "202611110001_create_users" 7 "applied" in
    assert_equal 7 parsed.batch
  in
  let test_rejects_non_positive_batch _ =
    match
      Sqlgg_migrations.Migration_state.parse_row
        ~id:"202611110001_create_users"
        ~batch:0L
        ~state:"applied"
    with
    | Error (`Invalid_batch 0L) -> ()
    | Ok _ | Error _ -> assert_failure "expected Invalid_batch"
  in
  let test_empty_database_plans_all_migrations _ =
    assert_equal
      [
        "202611110001_create_users";
        "202611110002_add_email";
        "202611110003_drop_legacy";
      ]
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.migrate
               Sqlgg_migrations.Planner.Linear
               (migrations ())
               [])))
  in
  let test_pending_returns_only_unapplied_suffix _ =
    assert_equal
      [ "202611110002_add_email"; "202611110003_drop_legacy" ]
      (List.map
         Sqlgg_migrations.Migration_id.to_string
         (planned
            (Sqlgg_migrations.Planner.pending
               Sqlgg_migrations.Planner.Linear
               (migrations ())
               [ row "202611110001_create_users" 1 "applied" ])))
  in
  let test_fully_applied_max_batch_returns_empty_plan _ =
    assert_equal
      []
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.migrate
               Sqlgg_migrations.Planner.Linear
               [ migration "202611110001_create_users" `Reversible ]
               [ row "202611110001_create_users" max_int "applied" ])))
  in
  let test_rejects_duplicate_compiled_ids _ =
    expect_planner_error
      (`Duplicate_id (id "202611110001_create_users"))
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         [
           migration "202611110001_create_users" `Reversible;
           migration "202611110001_create_users" `Irreversible;
         ]
         [])
  in
  let test_rejects_out_of_order_compiled_ids _ =
    expect_planner_error
      (`Out_of_order
        (id "202611110002_add_email", id "202611110001_create_users"))
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         [
           migration "202611110002_add_email" `Reversible;
           migration "202611110001_create_users" `Reversible;
         ]
         []);
    expect_planner_error
      (`Out_of_order
        (id "202611110001_create_users_b", id "202611110001_create_users_a"))
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         [
           migration "202611110001_create_users_b" `Reversible;
           migration "202611110001_create_users_a" `Reversible;
         ]
         [])
  in
  let test_rejects_unknown_stored_id history _ =
    expect_planner_error
      (`Unknown_stored_id (id "202611110999_unknown"))
      (Sqlgg_migrations.Planner.migrate
         history
         (migrations ())
         [ row "202611110999_unknown" 1 "applied" ])
  in
  let test_rejects_missing_middle_id _ =
    expect_planner_error
      (`Missing_stored_id (id "202611110002_add_email"))
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110003_drop_legacy" 1 "applied";
         ])
  in
  let test_rejects_non_monotonic_batches _ =
    expect_planner_error
      (`Non_monotonic_batch (2, 1))
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110001_create_users" 2 "applied";
           row "202611110002_add_email" 1 "applied";
         ])
  in
  let test_rejects_skipped_batch_number _ =
    expect_planner_error
      (`Missing_batch 2)
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 3 "applied";
         ])
  in
  let test_rejects_reappeared_batch_number _ =
    expect_planner_error
      (`Reappeared_batch 1)
      (Sqlgg_migrations.Planner.migrate
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 2 "applied";
           row "202611110003_drop_legacy" 1 "applied";
         ])
  in
  let test_non_final_state history expected_state raw_state _ =
    expect_planner_error
      (`Non_final_state (id "202611110001_create_users", expected_state))
      (Sqlgg_migrations.Planner.migrate
         history
         (migrations ())
         [ row "202611110001_create_users" 1 raw_state ])
  in
  let test_rollback_refuses_irreversible_batch_before_commands _ =
    expect_planner_error
      (`Irreversible (id "202611110003_drop_legacy"))
      (Sqlgg_migrations.Planner.rollback
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 2 "applied";
           row "202611110003_drop_legacy" 2 "applied";
         ])
  in
  let test_rollback_reverses_latest_batch history batches expected _ =
    let ids =
      [
        "202611110001_create_users";
        "202611110002_add_email";
        "202611110003_create_index";
      ]
    in
    assert_equal
      expected
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.rollback
               history
               (List.map (fun raw_id -> migration raw_id `Reversible) ids)
               (List.map2
                  (fun raw_id batch -> row raw_id batch "applied")
                  ids
                  batches))))
  in
  let test_steps_carry_direction_and_batch _ =
    let reversible =
      [ migration "202611110001_create_users" `Reversible ]
    in
    (match
       planned
         (Sqlgg_migrations.Planner.migrate
            Sqlgg_migrations.Planner.Linear
            reversible
            [])
     with
     | [ { batch = 1; run = `Run_apply; _ } ] -> ()
     | [] | _ :: _ -> assert_failure "expected one apply step in batch 1");
    match
      planned
        (Sqlgg_migrations.Planner.rollback
           Sqlgg_migrations.Planner.Linear
           reversible
           [ row "202611110001_create_users" 3 "applied" ])
    with
    | [ { batch = 3; run = `Run_revert; _ } ] -> ()
    | [] | _ :: _ -> assert_failure "expected one revert step for batch 3"
  in
  let test_accepts_ids_sharing_a_timestamp _ =
    assert_equal
      [ "20261111000000_create_users" ]
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.migrate
               Sqlgg_migrations.Planner.Linear
               [
                 migration "20261111000000_create_posts" `Reversible;
                 migration "20261111000000_create_users" `Reversible;
               ]
               [ row "20261111000000_create_posts" 1 "applied" ])))
  in
  let test_stored_row_order_is_irrelevant _ =
    expect_planner_error
      (`Irreversible (id "202611110003_drop_legacy"))
      (Sqlgg_migrations.Planner.rollback
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [
           row "202611110003_drop_legacy" 2 "applied";
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 1 "applied";
         ])
  in
  let test_interleaved_applies_migrations_skipped_earlier _ =
    let batches rows =
      List.map
        (fun (step : _ Sqlgg_migrations.Planner.step) ->
          ( Sqlgg_migrations.Migration_id.to_string step.migration_id,
            step.batch ))
        (planned
           (Sqlgg_migrations.Planner.migrate
              Sqlgg_migrations.Planner.Interleaved
              (migrations ())
              rows))
    in
    assert_equal
      [ "202611110001_create_users", 5; "202611110003_drop_legacy", 5 ]
      (batches [ row "202611110002_add_email" 4 "applied" ]);
    assert_equal
      [ "202611110002_add_email", 8 ]
      (batches
         [
           row "202611110001_create_users" 7 "applied";
           row "202611110003_drop_legacy" 4 "applied";
         ])
  in
  let test_interleaved_rollback_refuses_irreversible_batch _ =
    expect_planner_error
      (`Irreversible (id "202611110001_drop_legacy"))
      (Sqlgg_migrations.Planner.rollback
         Sqlgg_migrations.Planner.Interleaved
         [
           migration "202611110001_drop_legacy" `Irreversible;
           migration "202611110002_add_email" `Reversible;
           migration "202611110003_create_index" `Reversible;
         ]
         [
           row "202611110001_drop_legacy" 5 "applied";
           row "202611110002_add_email" 2 "applied";
           row "202611110003_create_index" 5 "applied";
         ])
  in
  let test_baseline_archives_squashed_rows _ =
    let cutoff = id "202611110000_baseline" in
    let archived =
      [
        row "202611100001_old_users" 4 "applied";
        row "202611110000_baseline" 7 "applied";
      ]
    in
    assert_equal
      []
      (planned
         (Sqlgg_migrations.Planner.pending
            ~baseline:cutoff
            Sqlgg_migrations.Planner.Interleaved
            []
            archived));
    expect_planner_error
      (`Archived_batch 7)
      (Sqlgg_migrations.Planner.rollback
         ~baseline:cutoff
         Sqlgg_migrations.Planner.Interleaved
         []
         archived);
    (match
       planned
         (Sqlgg_migrations.Planner.migrate
            ~baseline:cutoff
            Sqlgg_migrations.Planner.Interleaved
            [ migration "202611110001_create_users" `Reversible ]
            archived)
     with
     | [ { migration_id; batch = 8; run = `Run_apply } ] ->
         assert_equal
           "202611110001_create_users"
           (Sqlgg_migrations.Migration_id.to_string migration_id)
     | [] | _ :: _ -> assert_failure "expected batch 8 after archived batch 7");
    expect_planner_error
      (`Unknown_stored_id (id "202611120000_future"))
      (Sqlgg_migrations.Planner.migrate
         ~baseline:cutoff
         Sqlgg_migrations.Planner.Interleaved
         (migrations ())
         (row "202611120000_future" 8 "applied" :: archived));
    expect_planner_error
      (`Before_baseline (id "202611110001_create_users"))
      (Sqlgg_migrations.Planner.migrate
         ~baseline:(id "202611110001_create_users")
         Sqlgg_migrations.Planner.Linear
         (migrations ())
         [])
  in
  let test_migration_keeps_id_and_actions _ =
    (match migration "202611110001_create_users" `Reversible with
     | Sqlgg_migrations.Migration.Reversible
         { id = migration_id; apply; revert } ->
         assert_equal
           "202611110001_create_users"
           (Sqlgg_migrations.Migration_id.to_string migration_id);
         assert_equal `Run_apply apply;
         assert_equal `Run_revert revert
     | Sqlgg_migrations.Migration.Irreversible _ ->
         assert_failure "expected a reversible migration");
    match migration "202611110002_drop_legacy" `Irreversible with
    | Sqlgg_migrations.Migration.Irreversible
        { id = migration_id; apply } ->
        assert_equal
          "202611110002_drop_legacy"
          (Sqlgg_migrations.Migration_id.to_string migration_id);
        assert_equal `Run_apply apply
    | Sqlgg_migrations.Migration.Reversible _ ->
        assert_failure "expected an irreversible migration"
  in
  let test_generated_id_rejects_invalid_literal _ =
    assert_raises
      (Invalid_argument "sqlgg generated invalid migration id: invalid")
      (fun () ->
        Sqlgg_migrations.Migration_id.parse_generated ~id:"invalid")
  in
  run_test_tt_main
    ("migration runtime"
     >::: [
            "rejects oversized id" >:: test_rejects_oversized_id;
            "accepts maximum id" >:: test_accepts_id_at_byte_limit;
            "rejects invalid id" >:: test_rejects_invalid_id_grammar;
            "id keeps its text" >:: test_id_keeps_its_text;
            "made id" >:: test_made_id_has_ordinal_and_name;
            "id ordinal and name" >:: test_id_ordinal_and_name;
            "next position" >:: test_next_position_sorts_right_after_the_id;
            "id order" >:: test_id_order;
            "parses positive batch" >:: test_parses_positive_batch;
            "rejects non-positive batch"
            >:: test_rejects_non_positive_batch;
            "empty database plans all"
            >:: test_empty_database_plans_all_migrations;
            "pending suffix" >:: test_pending_returns_only_unapplied_suffix;
            "fully applied max batch"
            >:: test_fully_applied_max_batch_returns_empty_plan;
            "duplicate ids" >:: test_rejects_duplicate_compiled_ids;
            "out-of-order ids"
            >:: test_rejects_out_of_order_compiled_ids;
            "unknown stored id"
            >:: test_rejects_unknown_stored_id
                  Sqlgg_migrations.Planner.Linear;
            "missing middle id" >:: test_rejects_missing_middle_id;
            "non-monotonic batches"
            >:: test_rejects_non_monotonic_batches;
            "skipped batch number"
            >:: test_rejects_skipped_batch_number;
            "reappeared batch number"
            >:: test_rejects_reappeared_batch_number;
            "applying blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Planner.Linear
                  Sqlgg_migrations.Migration_state.Applying
                  "applying";
            "reverting blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Planner.Linear
                  Sqlgg_migrations.Migration_state.Reverting
                  "reverting";
            "apply failure blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Planner.Linear
                  Sqlgg_migrations.Migration_state.Apply_failed
                  "apply_failed";
            "revert failure blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Planner.Linear
                  Sqlgg_migrations.Migration_state.Revert_failed
                  "revert_failed";
            "irreversible batch blocks rollback"
            >:: test_rollback_refuses_irreversible_batch_before_commands;
            "latest batch reversed"
            >:: test_rollback_reverses_latest_batch
                  Sqlgg_migrations.Planner.Linear
                  [ 1; 2; 2 ]
                  [ "202611110003_create_index"; "202611110002_add_email" ];
            "steps carry direction and batch"
            >:: test_steps_carry_direction_and_batch;
            "ids sharing a timestamp"
            >:: test_accepts_ids_sharing_a_timestamp;
            "stored row order"
            >:: test_stored_row_order_is_irrelevant;
            "interleaved applies skipped migrations"
            >:: test_interleaved_applies_migrations_skipped_earlier;
            "interleaved highest batch reversed"
            >:: test_rollback_reverses_latest_batch
                  Sqlgg_migrations.Planner.Interleaved
                  [ 5; 2; 5 ]
                  [
                    "202611110003_create_index"; "202611110001_create_users";
                  ];
            "interleaved irreversible batch blocks rollback"
            >:: test_interleaved_rollback_refuses_irreversible_batch;
            "interleaved unknown stored id"
            >:: test_rejects_unknown_stored_id
                  Sqlgg_migrations.Planner.Interleaved;
            "interleaved apply failure blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Planner.Interleaved
                  Sqlgg_migrations.Migration_state.Apply_failed
                  "apply_failed";
            "baseline archives squashed rows"
            >:: test_baseline_archives_squashed_rows;
            "migration keeps id and actions"
            >:: test_migration_keeps_id_and_actions;
            "generated id invalid literal"
            >:: test_generated_id_rejects_invalid_literal;
          ])
