open OUnit2

let () =
  let module Descriptor_traits = struct
    type -'a connection = Connection
    type execute_response = Executed

    module IO = struct
      type 'a future = Completed of 'a
    end
  end
  in
  let module Migration =
    Sqlgg_migrations.Migration.Make (Descriptor_traits)
  in
  let run Descriptor_traits.Connection =
    Descriptor_traits.IO.Completed Descriptor_traits.Executed
  in
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
        Sqlgg_migrations.Planner.reversible
          (id raw_id)
          ~apply:`Run_apply
          ~revert:`Run_revert
    | `Irreversible ->
        Sqlgg_migrations.Planner.irreversible (id raw_id) ~apply:`Run_apply
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
        ~batch:(string_of_int batch)
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
    match Sqlgg_migrations.Migration_id.parse "create_users" with
    | Error (`Invalid "create_users") -> ()
    | Ok _ | Error _ -> assert_failure "expected Invalid"
  in
  let test_parses_positive_batch _ =
    let parsed = row "202611110001_create_users" 7 "applied" in
    assert_equal 7 parsed.batch
  in
  let test_rejects_non_positive_batch _ =
    match
      Sqlgg_migrations.Migration_state.parse_row
        ~id:"202611110001_create_users"
        ~batch:"0"
        ~state:"applied"
    with
    | Error (`Invalid_batch "0") -> ()
    | Ok _ | Error _ -> assert_failure "expected Invalid_batch"
  in
  let test_empty_database_plans_all_migrations _ =
    assert_equal
      [
        "202611110001_create_users";
        "202611110002_add_email";
        "202611110003_drop_legacy";
      ]
      (step_ids (planned (Sqlgg_migrations.Planner.migrate (migrations ()) [])))
  in
  let test_pending_returns_only_unapplied_suffix _ =
    assert_equal
      [ "202611110002_add_email"; "202611110003_drop_legacy" ]
      (List.map
         Sqlgg_migrations.Migration_id.to_string
         (planned
            (Sqlgg_migrations.Planner.pending
               (migrations ())
               [ row "202611110001_create_users" 1 "applied" ])))
  in
  let test_fully_applied_max_batch_returns_empty_plan _ =
    assert_equal
      []
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.migrate
               [ migration "202611110001_create_users" `Reversible ]
               [ row "202611110001_create_users" max_int "applied" ])))
  in
  let test_rejects_duplicate_compiled_ids _ =
    expect_planner_error
      (`Duplicate_id (id "202611110001_create_users"))
      (Sqlgg_migrations.Planner.migrate
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
         [
           migration "202611110002_add_email" `Reversible;
           migration "202611110001_create_users" `Reversible;
         ]
         [])
  in
  let test_rejects_unknown_stored_id _ =
    expect_planner_error
      (`Unknown_stored_id (id "202611110999_unknown"))
      (Sqlgg_migrations.Planner.migrate
         (migrations ())
         [ row "202611110999_unknown" 1 "applied" ])
  in
  let test_rejects_missing_middle_id _ =
    expect_planner_error
      (`Missing_stored_id (id "202611110002_add_email"))
      (Sqlgg_migrations.Planner.migrate
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
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 2 "applied";
           row "202611110003_drop_legacy" 1 "applied";
         ])
  in
  let test_non_final_state expected_state raw_state _ =
    expect_planner_error
      (`Non_final_state (id "202611110001_create_users", expected_state))
      (Sqlgg_migrations.Planner.migrate
         (migrations ())
         [ row "202611110001_create_users" 1 raw_state ])
  in
  let test_rollback_refuses_irreversible_batch_before_commands _ =
    expect_planner_error
      (`Irreversible (id "202611110003_drop_legacy"))
      (Sqlgg_migrations.Planner.rollback
         (migrations ())
         [
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 2 "applied";
           row "202611110003_drop_legacy" 2 "applied";
         ])
  in
  let test_rollback_reverses_latest_batch _ =
    assert_equal
      [ "202611110003_create_index"; "202611110002_add_email" ]
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.rollback
               [
                 migration "202611110001_create_users" `Reversible;
                 migration "202611110002_add_email" `Reversible;
                 migration "202611110003_create_index" `Reversible;
               ]
               [
                 row "202611110001_create_users" 1 "applied";
                 row "202611110002_add_email" 2 "applied";
                 row "202611110003_create_index" 2 "applied";
               ])))
  in
  let test_steps_carry_direction_and_batch _ =
    let reversible =
      [ migration "202611110001_create_users" `Reversible ]
    in
    (match planned (Sqlgg_migrations.Planner.migrate reversible []) with
     | [ { batch = 1; run = `Run_apply; _ } ] -> ()
     | [] | _ :: _ -> assert_failure "expected one apply step in batch 1");
    match
      planned
        (Sqlgg_migrations.Planner.rollback
           reversible
           [ row "202611110001_create_users" 3 "applied" ])
    with
    | [ { batch = 3; run = `Run_revert; _ } ] -> ()
    | [] | _ :: _ -> assert_failure "expected one revert step for batch 3"
  in
  let test_accepts_ids_sharing_a_timestamp _ =
    
    assert_equal
      [ "20261111000000_create_posts" ]
      (step_ids
         (planned
            (Sqlgg_migrations.Planner.migrate
               [
                 migration "20261111000000_create_users" `Reversible;
                 migration "20261111000000_create_posts" `Reversible;
               ]
               [ row "20261111000000_create_users" 1 "applied" ])))
  in
  let test_stored_row_order_is_irrelevant _ =
    expect_planner_error
      (`Irreversible (id "202611110003_drop_legacy"))
      (Sqlgg_migrations.Planner.rollback
         (migrations ())
         [
           row "202611110003_drop_legacy" 2 "applied";
           row "202611110001_create_users" 1 "applied";
           row "202611110002_add_email" 1 "applied";
         ])
  in
  let test_descriptor_construction _ =
    match
      Migration.make_reversible
        ~id:"202611110001_create_users"
        ~apply:run
        ~revert:run
    with
    | Ok (Migration.Reversible { id = descriptor_id; apply; revert }) ->
        assert_equal
          "202611110001_create_users"
          (Sqlgg_migrations.Migration_id.to_string descriptor_id);
        assert_equal
          (Descriptor_traits.IO.Completed Descriptor_traits.Executed)
          (apply Descriptor_traits.Connection);
        assert_equal
          (Descriptor_traits.IO.Completed Descriptor_traits.Executed)
          (revert Descriptor_traits.Connection)
    | Ok (Migration.Irreversible _) ->
        assert_failure "expected reversible descriptor"
    | Error _ -> assert_failure "expected valid descriptor"
  in
  let test_descriptor_rejects_invalid_id _ =
    match Migration.make_irreversible ~id:"invalid" ~apply:run with
    | Error (`Invalid_id (`Invalid "invalid")) -> ()
    | Ok _ | Error _ -> assert_failure "expected invalid id error"
  in
  let test_generated_descriptor_rejects_invalid_literal _ =
    assert_raises
      (Invalid_argument "sqlgg generated invalid migration id: invalid")
      (fun () ->
        ignore
          (Migration.make_generated_irreversible ~id:"invalid" ~apply:run))
  in
  let test_generated_descriptor_constructors _ =
    match
      ( Migration.make_generated_reversible
          ~id:"202611110001_create_users"
          ~apply:run
          ~revert:run,
        Migration.make_generated_irreversible
          ~id:"202611110002_drop_legacy"
          ~apply:run )
    with
    | Migration.Reversible _, Migration.Irreversible _ -> ()
    | Migration.Reversible _, Migration.Reversible _
    | Migration.Irreversible _, Migration.Reversible _
    | Migration.Irreversible _, Migration.Irreversible _ ->
        assert_failure "generated descriptor reversibility changed"
  in
  run_test_tt_main
    ("migration runtime"
     >::: [
            "rejects oversized id" >:: test_rejects_oversized_id;
            "accepts maximum id" >:: test_accepts_id_at_byte_limit;
            "rejects invalid id" >:: test_rejects_invalid_id_grammar;
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
            "unknown stored id" >:: test_rejects_unknown_stored_id;
            "missing middle id" >:: test_rejects_missing_middle_id;
            "non-monotonic batches"
            >:: test_rejects_non_monotonic_batches;
            "skipped batch number"
            >:: test_rejects_skipped_batch_number;
            "reappeared batch number"
            >:: test_rejects_reappeared_batch_number;
            "applying blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Migration_state.Applying
                  "applying";
            "reverting blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Migration_state.Reverting
                  "reverting";
            "apply failure blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Migration_state.Apply_failed
                  "apply_failed";
            "revert failure blocks mutation"
            >:: test_non_final_state
                  Sqlgg_migrations.Migration_state.Revert_failed
                  "revert_failed";
            "irreversible batch blocks rollback"
            >:: test_rollback_refuses_irreversible_batch_before_commands;
            "latest batch reversed"
            >:: test_rollback_reverses_latest_batch;
            "steps carry direction and batch"
            >:: test_steps_carry_direction_and_batch;
            "ids sharing a timestamp"
            >:: test_accepts_ids_sharing_a_timestamp;
            "stored row order"
            >:: test_stored_row_order_is_irrelevant;
            "descriptor construction"
            >:: test_descriptor_construction;
            "descriptor invalid id"
            >:: test_descriptor_rejects_invalid_id;
            "generated descriptor invalid literal"
            >:: test_generated_descriptor_rejects_invalid_literal;
            "generated descriptor constructors"
            >:: test_generated_descriptor_constructors;
          ])
