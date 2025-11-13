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

  let test_call11 connection =
    printf "[TEST 11] Testing JSON with string literals and special characters\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L]
    ));
    
    (* Test with JSON containing string with special SQL characters *)
    let json_with_string = `String "value with 'quotes' and \\backslash" in
    let result = Sql.test11 ~json_value:json_with_string ~id:6L connection in
    printf "[TEST 11] Result: completed (testing string quoting)\n\n";
    result

  let test_call12 connection =
    printf "[TEST 12] Testing INSERT with complex JSON structure\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ~insert_id:(Some 7L) ();
    
    (* Test with complex JSON containing strings, numbers, booleans, and nested objects *)
    let complex_json : T.Types.Json.t = `Assoc [
      ("name", `String "John O'Brien");
      ("age", `Int 30);
      ("active", `Bool true);
      ("score", `Float 95.5);
      ("tags", `List [`String "admin"; `String "user"; `String "it's a tag"]);
      ("metadata", `Assoc [
        ("created", `String "2024-01-15");
        ("notes", `String "Special chars: 'quote', \\backslash, \x00null")
      ])
    ] in
    let result = Sql.test12 ~id:7L ~json_data:(Some complex_json) connection in
    printf "[TEST 12] Result: affected_rows=%Ld, insert_id=%s (testing complex JSON quoting)\n\n"
      result.affected_rows
      (match result.insert_id with Some id -> Int64.to_string id | None -> "None");
    result

  (* Helper: test any JSON type via test11 (JSON_CONTAINS) *)
  let test_json_via_contains test_num desc json_value id connection =
    printf "[TEST %d] Testing JSON type: %s\n" test_num desc;
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L]
    ));
    let _ = Sql.test11 ~json_value ~id connection in
    printf "[TEST %d] Result: %s completed\n\n" test_num desc;
    ()

  let test_call13 connection =
    test_json_via_contains 13 "Bool true -> 'true'" (`Bool true) 1L connection

  let test_call14 connection =
    test_json_via_contains 14 "Int 42 -> '42'" (`Int 42) 2L connection

  let test_call15 connection =
    test_json_via_contains 15 "Float 3.14159 -> '3.14159'" (`Float 3.14159) 4L connection

  let test_call16 connection =
    test_json_via_contains 16 "String with quotes -> '\"O\\'Reilly...\"'" (`String "O'Reilly's book: \"MySQL\" \\path\\to\\file") 5L connection

  let test_call17 connection =
    test_json_via_contains 17 "Null -> 'null'" `Null 6L connection

  let test_call18 connection =
    let json_array : T.Types.Json.t = `List [`String "item1"; `Int 123; `Bool false; `String "it's ok"] in
    test_json_via_contains 18 "Array mixed -> '[\"item1\",123,false,...]'" json_array 7L connection

  let test_call19 connection =
    let json_object : T.Types.Json.t = `Assoc [
      ("key1", `String "value's with apostrophe");
      ("key2", `Int 100);
      ("key3", `Bool true)
    ] in
    test_json_via_contains 19 "Object simple -> '{\"key1\":\"value\\'s\",\"key2\":100,...}'" json_object 8L connection

  let test_call20 connection =
    test_json_via_contains 20 "Bool false -> 'false'" (`Bool false) 9L connection

  let test_call21 connection =
    test_json_via_contains 21 "Int negative -> '-999'" (`Int (-999)) 10L connection

  let test_call22 connection =
    test_json_via_contains 22 "Int zero -> '0'" (`Int 0) 11L connection

  let test_call23 connection =
    test_json_via_contains 23 "Float negative -> '-123.456'" (`Float (-123.456)) 12L connection

  let test_call24 connection =
    test_json_via_contains 24 "String empty -> '\"\"'" (`String "") 13L connection

  let test_call25 connection =
    test_json_via_contains 25 "String with null byte -> '\"before\\\\u0000after\"'" (`String "before\x00after") 14L connection

  let test_call26 connection =
    test_json_via_contains 26 "String with \\n and \\t -> '\"line1\\\\nline2\\\\ttab\"'" (`String "line1\nline2\ttab") 15L connection

  let test_call27 connection =
    test_json_via_contains 27 "Array empty -> '[]'" (`List []) 16L connection

  let test_call28 connection =
    let json_array_nested : T.Types.Json.t = `List [
      `Int 1;
      `List [`Int 2; `Int 3];
      `List [`String "nested"; `List [`Bool true]]
    ] in
    test_json_via_contains 28 "Nested array -> '[1,[2,3],[\"nested\",[true]]]'" json_array_nested 17L connection

  let test_call29 connection =
    test_json_via_contains 29 "Object empty -> '{}'" (`Assoc []) 18L connection

  let test_call30 connection =
    let json_object_nested : T.Types.Json.t = `Assoc [
      ("user", `Assoc [
        ("name", `String "Alice");
        ("age", `Int 30);
        ("address", `Assoc [
          ("city", `String "Paris");
          ("coordinates", `List [`Float 48.8566; `Float 2.3522])
        ])
      ]);
      ("active", `Bool true)
    ] in
    test_json_via_contains 30 "Nested object with coordinates" json_object_nested 19L connection

  (* Reuse test12 for INSERT *)
  let test_call31 connection =
    printf "[TEST 31] Testing JSON complex nested structure with INSERT (reusing test12)\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ~insert_id:(Some 20L) ();
    
    let json_complex : T.Types.Json.t = `Assoc [
      ("users", `List [
        `Assoc [
          ("id", `Int 1);
          ("name", `String "O'Reilly");
          ("emails", `List [`String "test@example.com"; `String "admin's email"]);
          ("metadata", `Assoc [
            ("created", `String "2024-01-01");
            ("tags", `List [`String "admin"; `String "user"])
          ])
        ];
        `Assoc [
          ("id", `Int 2);
          ("name", `String "User \"Two\"");
          ("active", `Bool false);
          ("score", `Float 98.765)
        ]
      ]);
      ("settings", `Assoc [
        ("theme", `String "dark");
        ("notifications", `Bool true);
        ("limits", `Assoc [
          ("max", `Int 1000);
          ("current", `Int 42)
        ])
      ]);
      ("nullValue", `Null);
      ("emptyArray", `List []);
      ("emptyObject", `Assoc [])
    ] in
    let result = Sql.test12 ~id:20L ~json_data:(Some json_complex) connection in
    printf "[TEST 31] Result: Complex structure (reused test12), affected_rows=%Ld, insert_id=%s\n\n"
      result.affected_rows
      (match result.insert_id with Some id -> Int64.to_string id | None -> "None");
    result

  (* Custom module tests now use test13/test14 from SQL *)
  let test_call32 connection =
    printf "[TEST 32] Testing custom JSON type with INSERT (module=Custom, reusing SQL test13)\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ~insert_id:(Some 21L) ();
    
    let custom_data : Custom.t = {
      a = "test with 'quotes' and \\backslash";
      b = 42L;
    } in
    let result = Sql.test13 ~id:21L ~custom_data:(Some custom_data) connection in
    printf "[TEST 32] Result: Custom type INSERT into table_custom, affected_rows=%Ld, insert_id=%s\n"
      result.affected_rows
      (match result.insert_id with Some id -> Int64.to_string id | None -> "None");
    printf "[TEST 32] Custom data: a='%s', b=%Ld\n\n" custom_data.a custom_data.b;
    result

  let test_call33 connection =
    printf "[TEST 33] Testing custom JSON type with SELECT (module=Custom, reusing SQL test14)\n";
    Print_ocaml_impl.clear_mock_responses ();
    
    (* Setup mock response with JSON that will be parsed by Custom.of_yojson *)
    let mock_json = `Assoc [
      ("a", `String "retrieved value");
      ("b", `Int 123)
    ] in
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_json mock_json]
    ));
    
    let result = Sql.test14 ~id:21L connection in
    printf "[TEST 33] Result: Custom type SELECT from table_custom completed\n";
    (match result with
     | Some (Some data) -> printf "[TEST 33] Retrieved custom data: a='%s', b=%Ld\n\n" data.a data.b
     | _ -> printf "[TEST 33] No data retrieved\n\n");
    ()

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
      let _ = test_call9 connection in
      let _ = test_call10 connection in
      let _ = test_call11 connection in
      let _ = test_call12 connection in
      test_call13 connection;
      test_call14 connection;
      test_call15 connection;
      test_call16 connection;
      test_call17 connection;
      test_call18 connection;
      test_call19 connection;
      test_call20 connection;
      test_call21 connection;
      test_call22 connection;
      test_call23 connection;
      test_call24 connection;
      test_call25 connection;
      test_call26 connection;
      test_call27 connection;
      test_call28 connection;
      test_call29 connection;
      test_call30 connection;
      let _ = test_call31 connection in
      let _ = test_call32 connection in
      test_call33 connection;
      
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
