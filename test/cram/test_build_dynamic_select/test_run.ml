(* test_run.ml - Test dynamic select query generation *)

open Printf

module M (T: Sqlgg_traits.M with 
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  (* === Test 1: Basic select_one_maybe === *)
  (* Now all columns (id, name, price, category) are dynamic *)
  module Test1 = struct
    open Sql.Select_product_col

    let single_field_name connection =
      printf "[TEST 1.1] Single field: Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.select_product connection ~col:name ~id:1L in
      printf "[TEST 1.1] Completed\n\n"

    let single_field_price connection =
      printf "[TEST 1.2] Single field: Price\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_float 99.99]
      ));
      let _ = Sql.select_product connection ~col:price ~id:2L in
      printf "[TEST 1.2] Completed\n\n"

    let combined_name_and_price connection =
      printf "[TEST 1.3] Combined fields: Name and Price using let+/and+\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Gadget";
          Print_ocaml_impl.mock_float 149.99
        ]
      ));
      let combined = 
        let+ n = name
        and+ p = price in
        (n, p)
      in
      let _ = Sql.select_product connection ~col:combined ~id:3L in
      printf "[TEST 1.3] Completed\n\n"

    let three_fields connection =
      printf "[TEST 1.4] Three fields: Name, Price, Category\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Phone";
          Print_ocaml_impl.mock_float 599.99;
          Print_ocaml_impl.mock_text "Electronics"
        ]
      ));
      let all_three = 
        let+ n = name
        and+ p = price
        and+ c = category in
        (n, p, c)
      in
      let _ = Sql.select_product connection ~col:all_three ~id:4L in
      printf "[TEST 1.4] Completed\n\n"

    let mapped_field connection =
      printf "[TEST 1.5] Mapped field: Price with transformation\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_float 100.0]
      ));
      let doubled_price = 
        let+ p = price in
        Option.map (fun x -> x *. 2.0) p
      in
      let _ = Sql.select_product connection ~col:doubled_price ~id:5L in
      printf "[TEST 1.5] Completed\n\n"

    let with_return connection =
      printf "[TEST 1.6] Return constructor (constant value)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row []
      ));
      let constant = pure "constant_value" in
      let _ = Sql.select_product connection ~col:constant ~id:6L in
      printf "[TEST 1.6] Completed\n\n"

    let run connection =
      single_field_name connection;
      single_field_price connection;
      combined_name_and_price connection;
      three_fields connection;
      mapped_field connection;
      with_return connection
  end

  (* === Test 2: select with callback (multiple rows) === *)
  (* Now all columns (id, name, price) are dynamic - callback is (fun ~col -> ...) *)
  module Test2 = struct
    open Sql.List_products_col

    let single_field connection =
      printf "[TEST 2.1] List with single field: Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"];
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Gadget"];
      ];
      Sql.list_products connection ~col:name ~min_stock:10L (fun ~col ->
        printf "  Row: col=%s\n" (match col with Some s -> s | None -> "NULL")
      );
      printf "[TEST 2.1] Completed\n\n"

    let combined_fields connection =
      printf "[TEST 2.2] List with combined fields: Id, Name and Price\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L; Print_ocaml_impl.mock_text "Widget"; Print_ocaml_impl.mock_float 19.99];
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 2L; Print_ocaml_impl.mock_text "Gadget"; Print_ocaml_impl.mock_float 29.99];
      ];
      let combined = 
        let+ i = id
        and+ n = name
        and+ p = price in
        (i, n, p)
      in
      Sql.list_products connection ~col:combined ~min_stock:5L (fun ~col ->
        let (i, n, p) = col in
        printf "  Row: id=%Ld, name=%s, price=%s\n" i 
          (match n with Some s -> s | None -> "NULL")
          (match p with Some f -> sprintf "%.2f" f | None -> "NULL")
      );
      printf "[TEST 2.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined_fields connection
  end

  (* === Test 3: Dynamic select with aliased expressions === *)
  module Test3 = struct
    open Sql.Multi_dynamic_col

    let single_field connection =
      printf "[TEST 3.1] Single field: label\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget - Electronics"
        ]
      ));
      let _ = Sql.multi_dynamic connection ~col:label ~id:1L in
      printf "[TEST 3.1] Completed\n\n"

    let combined connection =
      printf "[TEST 3.2] Combined: label and total_value\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget - Electronics";
          Print_ocaml_impl.mock_float 999.50
        ]
      ));
      let combined =
        let+ l = label
        and+ tv = total_value in
        (l, tv)
      in
      let _ = Sql.multi_dynamic connection ~col:combined ~id:2L in
      printf "[TEST 3.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined connection
  end

  (* === Test 4: Literal value column === *)
  module Test4 = struct
    open Sql.With_verbatim_col

    let single_field connection =
      printf "[TEST 4.1] Fallback literal field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "N/A"]
      ));
      let _ = Sql.with_verbatim connection ~col:fallback ~id:1L in
      printf "[TEST 4.1] Completed\n\n"

    let combined connection =
      printf "[TEST 4.2] Combined: id, name, fallback, category\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "N/A";
          Print_ocaml_impl.mock_text "Electronics"
        ]
      ));
      let combined =
        let+ i = id
        and+ n = name
        and+ f = fallback
        and+ c = category in
        (i, n, f, c)
      in
      let _ = Sql.with_verbatim connection ~col:combined ~id:2L in
      printf "[TEST 4.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined connection
  end

  (* === Test 5: Typed parameter column === *)
  module Test5 = struct
    open Sql.With_param_col

    let name_field connection =
      printf "[TEST 5.1] Name field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.with_param connection ~col:name ~id:1L in
      printf "[TEST 5.1] Completed\n\n"

    let custom_param connection =
      printf "[TEST 5.2] Custom param field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Custom Value"]
      ));
      let _ = Sql.with_param connection ~col:(custom "Custom Value") ~id:2L in
      printf "[TEST 5.2] Completed\n\n"

    let combined connection =
      printf "[TEST 5.3] Combined: id, name, custom\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "Hello"
        ]
      ));
      let combined =
        let+ i = id
        and+ n = name
        and+ c = custom "Hello" in
        (i, n, c)
      in
      let _ = Sql.with_param connection ~col:combined ~id:3L in
      printf "[TEST 5.3] Completed\n\n"

    let run connection =
      name_field connection;
      custom_param connection;
      combined connection
  end


  (* === Test 6: All columns dynamic, different order === *)
  module Test6 = struct
    open Sql.First_position_col

    let first_position connection =
      printf "[TEST 6.1] Dynamic select at first position\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget"
        ]
      ));
      let _ = Sql.first_position connection ~col:name ~id:1L in
      printf "[TEST 6.1] Completed\n\n"

    let first_combined connection =
      printf "[TEST 6.2] Dynamic select at first position with combinator\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 99.99
        ]
      ));
      let combined = 
        let+ n = name
        and+ p = price in
        (n, p)
      in
      let _ = Sql.first_position connection ~col:combined ~id:2L in
      printf "[TEST 6.2] Completed\n\n"

    let run connection =
      first_position connection;
      first_combined connection
  end

  (* === Test 7: select_one (guaranteed row) === *)
  module Test7 = struct
    open Sql.Select_one_product_col

    let select_one_single connection =
      printf "[TEST 7.1] select_one with single field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.select_one_product connection ~col:name ~id:1L in
      printf "[TEST 7.1] Completed\n\n"

    let select_one_combined connection =
      printf "[TEST 7.2] select_one with combined fields\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 99.99
        ]
      ));
      let combined = 
        let+ n = name
        and+ p = price in
        (n, p)
      in
      let _ = Sql.select_one_product connection ~col:combined ~id:2L in
      printf "[TEST 7.2] Completed\n\n"

    let run connection =
      select_one_single connection;
      select_one_combined connection
  end

  (* === Test 8: module-wrapped column === *)
  module Test8 = struct
    open Sql.With_module_col

    let with_module_id connection =
      printf "[TEST 8.1] Module-wrapped column: Id\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 42L]
      ));
      let _ = Sql.with_module connection ~col:id ~id:1L in
      printf "[TEST 8.1] Completed\n\n"

    let with_module_name connection =
      printf "[TEST 8.2] Module-wrapped: regular column Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.with_module connection ~col:name ~id:2L in
      printf "[TEST 8.2] Completed\n\n"

    let run connection =
      with_module_id connection;
      with_module_name connection
  end
  

  (* === Test 9: IN @list inside subquery branch === *)
  (* Now all columns (id, name, filtered) are dynamic *)
  module Test9 = struct
    open Sql.With_in_subquery_col

    let in_subquery_filtered connection =
      printf "[TEST 9.1] IN list inside subquery branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let combined =
        let+ i = id
        and+ n = name
        and+ f = filtered [1.0; 2.0] in
        (i, n, f)
      in
      let _ = Sql.with_in_subquery connection ~col:combined ~id:1L in
      printf "[TEST 9.1] Completed\n\n"

    let run connection =
      in_subquery_filtered connection
  end

  (* === Test 10: arithmetic param inside branch === *)
  (* Now all columns (id, add_tax) are dynamic *)
  module Test10 = struct
    open Sql.With_arith_param_col

    let add_tax connection =
      printf "[TEST 10.1] Arithmetic param in branch (price + tax)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_float 120.0
        ]
      ));
      let combined =
        let+ i = id
        and+ at = add_tax (Some 20.0) in
        (i, at)
      in
      let _ = Sql.with_arith_param connection ~col:combined ~id:1L in
      printf "[TEST 10.1] Completed\n\n"

    let run connection =
      add_tax connection
  end

  (* === Test 11: two params inside branch === *)
  module Test11 = struct
    open Sql.With_two_params_col

    let test_in_range connection =
      printf "[TEST 11.1] Two params in branch (range)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_bool true
        ]
      ));
      let combined =
        let+ i = id
        and+ r = in_range (Some 10.0) (Some 20.0) in
        (i, r)
      in
      let _ = Sql.with_two_params connection ~col:combined ~id:1L in
      printf "[TEST 11.1] Completed\n\n"

    let run connection =
      test_in_range connection
  end

  (* === Test 12: normal param + IN @list inside one branch === *)
  module Test12 = struct
    open Sql.With_param_and_in_col

    let match_with_suffix connection =
      printf "[TEST 12.1] Param + IN list in branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_bool true
        ]
      ));
      let combined =
        let+ i = id
        and+ m = match_ "_x" ["a_x"; "b_x"] in
        (i, m)
      in
      let _ = Sql.with_param_and_in connection ~col:combined ~id:1L in
      printf "[TEST 12.1] Completed\n\n"

    let run connection =
      match_with_suffix connection
  end

  (* === Test 13: option-actions inside subquery WHERE === *)
  module Test13 = struct
    open Sql.With_option_actions_in_subquery_col

    let opt_none connection =
      printf "[TEST 13.1] Option-actions in subquery (None)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let combined =
        let+ i = id
        and+ o = opt None in
        (i, o)
      in
      let _ = Sql.with_option_actions_in_subquery connection ~col:combined ~id:1L in
      printf "[TEST 13.1] Completed\n\n"

    let opt_some connection =
      printf "[TEST 13.2] Option-actions in subquery (Some)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let combined =
        let+ i = id
        and+ o = opt (Some 10.0) in
        (i, o)
      in
      let _ = Sql.with_option_actions_in_subquery connection ~col:combined ~id:2L in
      printf "[TEST 13.2] Completed\n\n"

    let run connection =
      opt_none connection;
      opt_some connection
  end

  (* === Test 14: tuple list IN inside subquery WHERE === *)
  module Test14 = struct
    open Sql.With_tuple_list_in_subquery_col

    let test_pairs connection =
      printf "[TEST 14.1] Tuple list IN inside subquery\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let combined =
        let+ i = id
        and+ p = pairs [ (1L, Some 10L) ] in
        (i, p)
      in
      let _ = Sql.with_tuple_list_in_subquery connection ~col:combined ~id:1L in
      printf "[TEST 14.1] Completed\n\n"

    let run connection =
      test_pairs connection
  end

  (* === Test 15: CASE expression inside branch === *)
  module Test15 = struct
    open Sql.With_case_expr_col

    let test_casey connection =
      printf "[TEST 15.1] CASE expression inside branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 123L
        ]
      ));
      let combined =
        let+ i = id
        and+ c = casey 2L 123L in
        (i, c)
      in
      let _ = Sql.with_case_expr connection ~col:combined ~id:1L in
      printf "[TEST 15.1] Completed\n\n"

    let run connection =
      test_casey connection
  end

  (* === Test 16: typed param inside branch === *)
  module Test16 = struct
    open Sql.With_typed_param_col

    let test_typed connection =
      printf "[TEST 16.1] Typed param inside branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "hello"
        ]
      ));
      let combined =
        let+ i = id
        and+ t = typed "hello" in
        (i, t)
      in
      let _ = Sql.with_typed_param connection ~col:combined ~id:1L in
      printf "[TEST 16.1] Completed\n\n"

    let run connection =
      test_typed connection
  end

  (* === Test 17: Complex subquery as plain dynamic column === *)
  module Test17 = struct
    open Sql.Monster_nested_col

    let monster_field connection =
      printf "[TEST 17.1] Monster subquery field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let col = monster 2L 1L "then_v" "else_v" (Some 10.0) ["a"; "b"] [(1L, Some 10L)] in
      let _ = Sql.monster_nested connection ~col ~id:1L in
      printf "[TEST 17.1] Completed\n\n"

    let combined connection =
      printf "[TEST 17.2] Combined: id + monster\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let combined =
        let+ i = id
        and+ m = monster 2L 1L "then_v" "else_v" (Some 10.0) ["a"; "b"] [(1L, Some 10L)] in
        (i, m)
      in
      let _ = Sql.monster_nested connection ~col:combined ~id:1L in
      printf "[TEST 17.2] Completed\n\n"

    let run connection =
      monster_field connection;
      combined connection
  end

  (* === Test 18: Various SQL constructs as plain dynamic columns === *)
  module Test18 = struct
    open Sql.Ultimate_combo_col

    let test_plain connection =
      printf "[TEST 18.1] Plain stock field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 100L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:plain ~id:1L in
      printf "[TEST 18.1] Completed\n\n"

    let test_with_in_list connection =
      printf "[TEST 18.2] IN list subquery field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 3L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(with_in_list [1L; 2L; 3L]) ~id:1L in
      printf "[TEST 18.2] Completed\n\n"

    let test_with_optional connection =
      printf "[TEST 18.3] Optional subquery field (None)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 50L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(with_optional None) ~id:1L in
      printf "[TEST 18.3] Completed\n\n"

    let test_with_case connection =
      printf "[TEST 18.4] CASE expression field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 5L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(with_case 1L ["foo"; "bar"]) ~id:1L in
      printf "[TEST 18.4] Completed\n\n"

    let test_full_combo connection =
      printf "[TEST 18.5] Full combo: all fields combined\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 100L;
          Print_ocaml_impl.mock_int 3L;
          Print_ocaml_impl.mock_int 50L;
          Print_ocaml_impl.mock_int 5L;
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let combined =
        let+ i = id
        and+ p = plain
        and+ il = with_in_list [1L; 2L]
        and+ wo = with_optional (Some 5L)
        and+ wc = with_case 1L ["foo"]
        and+ wt = with_tuple_list [(1L, Some 10L)]
        and+ fc = full_combo (Some 5L) ["x"] 100.0 in
        (i, p, il, wo, wc, wt, fc)
      in
      let _ = Sql.ultimate_combo connection ~col:combined ~id:1L in
      printf "[TEST 18.5] Completed\n\n"

    let run connection =
      test_plain connection;
      test_with_in_list connection;
      test_with_optional connection;
      test_with_case connection;
      test_full_combo connection
  end

  (* === Test 19: Mixed columns with arithmetic expression === *)
  module Test19 = struct
    open Sql.Ultimate_combo_simple2_col

    let test_all_fields connection =
      printf "[TEST 19] All fields combined\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "Electronics";
          Print_ocaml_impl.mock_int 50L;
          Print_ocaml_impl.mock_float 119.99
        ]
      ];
      let combined =
        let+ i = id
        and+ n = name
        and+ c = category
        and+ s = stock
        and+ p = price_with_tax 10L in
        (i, n, c, s, p)
      in
      let _ = Sql.List.ultimate_combo_simple2 connection ~col:combined
        (fun ~col -> ignore col) in
      printf "[TEST 19] Completed\n\n"

    let run connection = test_all_fields connection
  end
 

  let run_all_tests connection =
    printf "=== Starting Dynamic Select Tests ===\n\n";
    
    try
      printf "--- Test Group 1: Basic select_one_maybe ---\n";
      Test1.run connection;
      
      printf "--- Test Group 2: Select with callback ---\n";
      Test2.run connection;
      
      printf "--- Test Group 3: Multiple dynamic selects ---\n";
      Test3.run connection;
      
      printf "--- Test Group 4: Verbatim branches ---\n";
      Test4.run connection;
      
      printf "--- Test Group 5: Parameter in branch ---\n";
      Test5.run connection;
      
      printf "--- Test Group 6: Dynamic at first position ---\n";
      Test6.run connection;
      
      printf "--- Test Group 7: select_one ---\n";
      Test7.run connection;
      
      printf "--- Test Group 8: module-wrapped column ---\n";
      Test8.run connection;

      printf "--- Test Group 9: IN list inside subquery branch ---\n";
      Test9.run connection;

      printf "--- Test Group 10: arithmetic param inside branch ---\n";
      Test10.run connection;

      printf "--- Test Group 11: two params inside branch ---\n";
      Test11.run connection;

      printf "--- Test Group 12: param + IN list inside one branch ---\n";
      Test12.run connection;

      printf "--- Test Group 13: option-actions inside subquery WHERE ---\n";
      Test13.run connection;

      printf "--- Test Group 14: tuple list IN inside subquery WHERE ---\n";
      Test14.run connection;

      printf "--- Test Group 15: CASE expression inside branch ---\n";
      Test15.run connection;

      printf "--- Test Group 16: typed param inside branch ---\n";
      Test16.run connection;

      printf "--- Test Group 17: monster nested scenario ---\n";
      Test17.run connection;

      printf "--- Test Group 18: ultimate combo (multiple branches) ---\n";
      Test18.run connection;

      printf "--- Test Group 19---\n";
      Test19.run connection;
      
      printf "=== All Dynamic Select Tests Passed ===\n"
    with
    | exn -> 
      printf "\n=== Test Failed with Exception: %s ===\n" (Printexc.to_string exn);
      raise exn
end

module Test = M(Print_ocaml_impl)

let () = 
  let con = () in
  
  printf "Dynamic Select Query Generation Tests\n";
  printf "%s\n" (String.make 50 '=');
  
  Test.run_all_tests con;
  
  printf "\n%s\n" (String.make 50 '=');
  printf "All tests executed successfully!\n"
