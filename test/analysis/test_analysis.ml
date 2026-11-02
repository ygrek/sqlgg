open OUnit2
open Sqlgg
open Test_helpers

let test_parse_select _ =
  match get_ok (parse "SELECT 1") with
  | { stmt = Sql.Select _; _ } -> ()
  | _ -> assert_failure "expected SELECT AST"

let test_parse_keeps_column_annotations _ =
  match get_ok (parse {|
  -- leading comment
CREATE TABLE t (
  id INT,
  -- [sqlgg] module=Codecs.Cid
  cid BIGINT
)|}) with
  | { stmt = Sql.Create (_, Schema { schema = [ id; cid ]; _ }); _ } ->
    assert_equal [] id.meta;
    assert_equal [ "module", "Codecs.Cid" ] cid.meta
  | _ -> assert_failure "expected CREATE TABLE with two columns"

let test_schema_contexts_are_isolated _ =
  let users =
    context "CREATE TABLE users (id INT NOT NULL)"
  in
  let posts =
    context "CREATE TABLE posts (id INT NOT NULL)"
  in
  ignore (get_ok (analyze ~context:users "SELECT id FROM users"));
  ignore (get_ok (analyze ~context:posts "SELECT id FROM posts"));
  begin match analyze ~context:posts "SELECT id FROM users" with
  | Error _ -> ()
  | Ok _ -> assert_failure "users table leaked into posts schema"
  end;
  match analyze ~context:users "SELECT id FROM posts" with
  | Error _ -> ()
  | Ok _ -> assert_failure "posts table leaked into users schema"

let test_schema_accepts_multiple_ddl _ =
  let schema =
    context "CREATE TABLE users (id INT); CREATE TABLE posts (id INT)"
  in
  ignore (get_ok (analyze ~context:schema "SELECT id FROM users"));
  ignore (get_ok (analyze ~context:schema "SELECT id FROM posts"))

let assert_schema_error_at expected sql =
  match Analysis.context_of_schema_sql sql with
  | Error (statement, { location = Span pos; _ }, []) ->
    assert_equal ~cmp:Sql.Pos.equal ~printer:Sql.Pos.show expected
      (Sql.Pos.shift (fst statement.pos) pos)
  | Error _ -> assert_failure "expected one positioned diagnostic"
  | Ok _ -> assert_failure "expected the schema to be rejected"

let test_schema_locates_second_parser_diagnostic _ =
  assert_schema_error_at (36, 40) "CREATE TABLE users (id INT);\nSELECT FROM"

let test_schema_locates_second_semantic_diagnostic _ =
  assert_schema_error_at (36, 43) "CREATE TABLE users (id INT);\nSELECT missing FROM users"

let test_schema_property_diagnostic _ =
  match
    Analysis.context_of_schema_sql
      "CREATE TABLE users (id INT);\n-- @foo | broken\nCREATE TABLE posts (id INT)"
  with
  | Error ({ text = "CREATE TABLE posts (id INT)"; _ }, { location = Properties; _ }, []) -> ()
  | Error _ -> assert_failure "expected one property diagnostic of the second statement"
  | Ok _ -> assert_failure "expected the schema to be rejected"

let test_schema_returns_expected_sql_diagnostic _ =
  match Analysis.context_of_schema_sql "CREATE TABLE users (id INT); CREATE TABLE users (id INT)" with
  | Error (_, { message = "table users already exists"; location = Statement }, []) -> ()
  | Error (_, { message; _ }, _) ->
    assert_failure ("unexpected schema diagnostic: " ^ message)
  | Ok _ -> assert_failure "expected duplicate table to fail"

let test_syntax_diagnostic _ =
  match parse "SELECT FROM" with
  | Error ({ message; location = Span _ }, []) ->
    assert_bool "diagnostic message is empty" (String.length message > 0)
  | Error _ -> assert_failure "expected one positioned syntax diagnostic"
  | Ok _ -> assert_failure "expected malformed SQL to fail"

let test_property_diagnostic_before_statement _ =
  match parse "-- [sqlgg] bogus=1\nSELECT 1" with
  | Error ({ message = "unknown property bogus"; location = Properties }, []) -> ()
  | Error (first, rest) -> assert_failure ("unexpected diagnostics: " ^ messages (first :: rest))
  | Ok _ -> assert_failure "expected the unknown property to fail"

let test_property_diagnostic_inside_statement _ =
  let sql = "SELECT 3 +\n-- @bar | broken\n 4" in
  match parse sql with
  | Error ({ message = "malformed property list"; location = Span (start, stop) }, []) ->
    assert_equal ~printer:Fun.id "-- @bar | broken\n" (String.sub sql start (stop - start))
  | Error (first, rest) -> assert_failure ("unexpected diagnostics: " ^ messages (first :: rest))
  | Ok _ -> assert_failure "expected the malformed property list to fail"

let test_semantic_diagnostic _ =
  match
    analyze ~context:(context "CREATE TABLE users (id INT NOT NULL)") "SELECT missing FROM users"
  with
  | Error ({ message; _ }, []) ->
    assert_bool
      "diagnostic should identify the missing column"
      (String.length message > 0)
  | Error _ -> assert_failure "expected one semantic diagnostic"
  | Ok _ -> assert_failure "expected unknown column to fail"

let test_calls_restore_ambient_schema _ =
  Analysis.with_context (fun () ->
      compile_all "CREATE TABLE ambient (id INT NOT NULL)";
      ignore
        (get_ok
           (analyze
              ~context:(context "CREATE TABLE isolated (id INT NOT NULL)")
              "SELECT id FROM isolated"));
      let ambient = Compile.snapshot () in
      ignore (get_ok (analyze ~context:ambient "SELECT id FROM ambient"));
      match analyze ~context:ambient "SELECT id FROM isolated" with
      | Error _ -> ()
      | Ok _ -> assert_failure "isolated table leaked into ambient schema")

let test_expected_schema_error_restores_ambient_state _ =
  Analysis.with_context (fun () ->
      compile_all "CREATE TABLE ambient (id INT NOT NULL)";
      begin match Analysis.context_of_schema_sql "CREATE TABLE broken (" with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected invalid schema SQL to fail"
      end;
      ignore (get_ok (analyze ~context:(Compile.snapshot ()) "SELECT id FROM ambient")))

let suite =
  "analysis" >::: [
    "parse SELECT" >:: test_parse_select;
    "parse keeps column annotations" >:: test_parse_keeps_column_annotations;
    "schema contexts are isolated" >:: test_schema_contexts_are_isolated;
    "schema accepts multiple DDL" >:: test_schema_accepts_multiple_ddl;
    "schema locates second parser diagnostic" >:: test_schema_locates_second_parser_diagnostic;
    "schema locates second semantic diagnostic" >:: test_schema_locates_second_semantic_diagnostic;
    "schema property diagnostic" >:: test_schema_property_diagnostic;
    "schema returns expected SQL diagnostic" >:: test_schema_returns_expected_sql_diagnostic;
    "syntax diagnostic" >:: test_syntax_diagnostic;
    "property diagnostic before statement" >:: test_property_diagnostic_before_statement;
    "property diagnostic inside statement" >:: test_property_diagnostic_inside_statement;
    "semantic diagnostic" >:: test_semantic_diagnostic;
    "calls restore ambient schema" >:: test_calls_restore_ambient_schema;
    "expected schema error restores ambient state" >:: test_expected_schema_error_restores_ambient_state;
  ]

let () = run_test_tt_main suite
