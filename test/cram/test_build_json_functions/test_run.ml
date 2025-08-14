(* test_run.ml - Inline mocking version *)

open Printf

module M (T: Sqlgg_traits.M with type Types.Json.t = Yojson.Basic.t and
  type Types.Json_path.t = Sqlgg_json_path.Ast.t and
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.One_or_all.t = [`One | `All]) = struct

  module Sql = Output.Sqlgg(T)

  let test_call connection = 
    printf "[TEST 1] Testing JSON parameter with nested structure\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_json (`String "$.users[0].settings.themes[0]")]
    ));
    
    let result = Sql.test1 ~json:
      (`Assoc
         [ "users",
           `List
             [ `Assoc
                 [ "id", `Int 1;
                   "settings",
                   `Assoc [ "themes", `List [ `String "dark" ] ]
                 ]
             ]
         ])
    ~one:`One connection in
    printf "[TEST 1] Result: completed\n\n";
    result

  let test_call2 connection =
    printf "[TEST 2] Testing JSON path parameter with combinators\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_json (`Assoc [("theme", `String "dark"); ("language", `String "en")])]
    ));
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let from_combinator = root / ~."data" / any / ~."users" / last / ~."settings" in
    let result = Sql.test2 ~path:from_combinator connection in
    printf "[TEST 2] Result: completed\n\n";
    result

  let test_call3 connection =
    printf "[TEST 3] Testing SELECT with JSON path extraction\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L; Print_ocaml_impl.mock_text "Alice"; Print_ocaml_impl.mock_text "Alice Johnson"]
    ));

    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let name_path = root / ~."name" in
    let result = Sql.test3 ~id:1L ~name_path connection in
    printf "[TEST 3] Result: completed\n\n";
    result

  let test_call4 connection =
    printf "[TEST 4] Testing SELECT with nested JSON path\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 2L; Print_ocaml_impl.mock_text "user@example.com"]
    ));
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in  
    let email_path = root / ~."user" / ~."email" in
    let result = Sql.test4 ~id:2L ~email_path connection in
    printf "[TEST 4] Result: completed\n\n";
    result

  let test_call5 connection =
    printf "[TEST 5] Testing SELECT ONE with JSON path\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ~insert_id:(Some 5L) ();
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let login_path = root / ~."last_login" in
    let result = Sql.test5 ~id:3L ~login_path connection in
    printf "[TEST 5] Result: completed\n\n";
    result

  let test_call6 connection =
    printf "[TEST 6] Testing simple SELECT with callback\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 4L; Print_ocaml_impl.mock_text "active"]
    ));
    
    let result = Sql.test6 ~id:4L connection in
    printf "[TEST 6] Result: completed\n\n";
    result

  let test_call7 connection =
    printf "[TEST 7] Testing SELECT with JSON path filter\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 101L; Print_ocaml_impl.mock_text "Laptop"];
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 102L; Print_ocaml_impl.mock_text "Phone"];
    ];
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let category_path = root / ~."metadata" / ~."category" in
    let result = Sql.test7 ~category_path ~category:(`String "electronics") connection (fun ~id ~name -> 
      printf "  -> Callback executed for test7 with id and name\n"
    ) in
    printf "[TEST 7] Result: completed\n\n";
    result

  let test_call8 connection =
    printf "[TEST 8] Testing complex SELECT with multiple JSON paths\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L; Print_ocaml_impl.mock_text "John Doe"; Print_ocaml_impl.mock_text "dark"];
    ];
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let name_path = root / ~."profile" / ~."name" in
    let theme_path = root / ~."settings" / ~."theme" in
    let active_path = root / ~."profile" / ~."active" in
    let result = Sql.test8 ~name_path ~theme_path ~active_path connection (fun ~id ~user_name ~theme -> 
      printf "  -> Callback executed for test8 with id, user_name, theme\n"
    ) in
    printf "[TEST 8] Result: completed\n\n";
    result

  let test_call9 connection =
    printf "[TEST 9] Testing UPDATE with JSON path conditions\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:3L ~insert_id:(Some 42L) ();
    
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let update_path = root / ~."timestamps" / ~."last_update" in
    let role_path = root / ~."user" / ~."role" in
    let result = Sql.test9 ~update_path ~role_path ~role:(`String "admin") connection in
    printf "[TEST 9] Result: affected_rows=%Ld, insert_id=%s\n\n" 
      result.affected_rows 
      (match result.insert_id with Some id -> Int64.to_string id | None -> "None");
    result

  let test_call10 connection =
    printf "[TEST 10] Testing search with text parameter\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_json (`String "$.test[0]")]
    ));
    
    let result = Sql.test10 ~search_value:"admin" ~id:5L connection in
    printf "[TEST 10] Result: completed\n\n";
    result

  let run_all_tests connection =
    printf "=== Starting JSON Path Tests with Inline Mocking ===\n\n";
    
    try
      printf "Running all tests...\n\n";
      
      let _ = test_call connection in
      let _ = test_call2 connection in
      let _ = test_call3 connection in
      let _ = test_call4 connection in
      let _ = test_call5 connection in
      let _ = test_call6 connection in
      let _ = test_call7 connection in
      let _ = test_call8 connection in
      let result9 = test_call9 connection in
      let _ = test_call10 connection in
      
      printf "=== All JSON Path Tests Completed Successfully ===\n";
      printf "Summary:\n";
      printf "- Tests 1-8, 10: completed with inline mocked data\n";
      printf "- Test 9: returned execute_response with affected_rows=%Ld\n" result9.affected_rows;
      printf "- All SQL queries were logged\n";
      printf "- Each test setup its own mock data\n";
      
      ()
      
    with
    | exn -> 
      printf "\n=== Test Failed with Exception: %s ===\n" (Printexc.to_string exn);
      raise exn
end

module Test = M(Print_ocaml_impl)

let () = 
  let con = () in
  
  printf "Starting JSON Path Tests with Inline Mock Implementation\n";
  printf "%s\n" (String.make 60 '=');
  
  Test.run_all_tests con;
  
  printf "\n%s\n" (String.make 60 '=');
  printf "All tests executed successfully!\n"
