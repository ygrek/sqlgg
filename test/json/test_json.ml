open OUnit2
open Sqlgg
open Test_helpers

let big_select_sql = {|
WITH RECURSIVE hierarchy AS (
  SELECT id, dept_id, name FROM users WHERE id = 1
  UNION ALL
  SELECT u.id, u.dept_id, u.name
  FROM users u
  JOIN hierarchy h ON u.dept_id = h.id
)
SELECT h.id, h.name,
  (SELECT MAX(s.score) FROM scores s WHERE s.user_id = h.id) AS max_score
FROM hierarchy h
LEFT JOIN departments d ON d.id = h.dept_id
JOIN (SELECT id FROM departments WHERE title = 'eng') e ON e.id = d.id
WHERE h.id > 0
  AND (h.name = 'x' OR EXISTS (SELECT 1 FROM scores s2 WHERE s2.user_id = h.id))
  AND LENGTH(h.name) > 0
GROUP BY h.id, h.name, d.id
HAVING COUNT(*) > 0
ORDER BY h.id
LIMIT 10
|}

let test_stmt_jsonschema_builds _ =
  let schema = Jsonkit.Jsonschema.make Sql.stmt_jsonschema in
  let bytes = String.length (Yojson.Basic.to_string schema) in
  assert_bool (Printf.sprintf "stmt schema too small: %d" bytes) (Int.compare bytes 1000 > 0);
  Printf.printf "Sql.stmt JSON Schema size: %d bytes\n%!" bytes

let test_corpus_roundtrip _ =
  List.iter
    (fun sql ->
      let ast = (get_ok (parse sql)).Parser.stmt in
      assert_equal ~cmp:Sql.equal_stmt ~printer:Sql.show_stmt ast
        (Sql.stmt_of_json (Sql.stmt_to_json ast)))
    [
      big_select_sql;
      "CREATE TABLE t (id INT NOT NULL, name TEXT NULL)";
      "CREATE INDEX idx_t_id ON t (id)";
      "INSERT INTO t (id, name) VALUES (1, 'a')";
      "INSERT INTO t (id, name) VALUES (1, 'a') ON DUPLICATE KEY UPDATE name = VALUES(name)";
      "UPDATE t SET name = 'b' WHERE id = 1";
      "DELETE FROM t WHERE id = 1";
      "ALTER TABLE t ADD COLUMN x INT NULL";
      "SELECT 1 WHERE @c { A { 1 = 1 } | B { 1 = 2 } }";
      "SELECT 1 WHERE { 1 = 1 }?";
      "SELECT 1 WHERE id IN @ids";
      "SELECT 1 WHERE (1, 2) IN @pairs";
    ]

let test_syntax_signature_roundtrip _ =
  let (_, (result : Syntax.result)) =
    get_ok
      (analyze
         ~context:(context "CREATE TABLE t (id INT NOT NULL, name TEXT NULL)")
         "SELECT id, name FROM t WHERE id = 1")
  in
  assert_equal ~cmp:Syntax.equal_signature ~printer:Syntax.show_signature result.typed
    (Syntax.signature_of_json (Syntax.signature_to_json result.typed))

let test_manual_enum_kind_ctors _ =
  let open Sql.Type.Enum_kind in
  let s = Ctors.of_list ["b"; "a"; "c"] in
  let json = Ctors.to_json s in
  assert_equal ~cmp:Yojson.Basic.equal ~printer:Yojson.Basic.to_string
    (`List [`String "a"; `String "b"; `String "c"]) json;
  assert_bool "ctors roundtrip" (Ctors.equal s (Ctors.of_json json));
  begin match Ctors.of_json (`Int 1) with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Ctors.of_json should reject garbage"
  end

let test_manual_constraints _ =
  let open Sql in
  let s = Constraints.of_list [Constraint.NotNull; Constraint.PrimaryKey] in
  assert_bool "Constraints roundtrip"
    (Constraints.equal s (Constraints.of_json (Constraints.to_json s)));
  begin match Constraints.of_json (`String "nope") with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Constraints.of_json should reject garbage"
  end

let test_manual_meta _ =
  let open Sql.Meta in
  let m = of_list ["z", "1"; "a", "2"] in
  let json = to_json m in
  begin match json with
  | `Assoc (("a", _) :: _) -> ()
  | `Assoc _ -> assert_failure "Meta.to_json keys must be sorted"
  | _ -> assert_failure "Meta.to_json must be object"
  end;
  assert_bool "Meta roundtrip" (equal m (of_json json));
  begin match of_json (`List []) with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Meta.of_json should reject garbage"
  end

let test_manual_string_set _ =
  let open Sql.Constraint.StringSet in
  let s = of_list ["y"; "x"] in
  let json = to_json s in
  assert_equal ~cmp:Yojson.Basic.equal ~printer:Yojson.Basic.to_string
    (`List [`String "x"; `String "y"]) json;
  assert_bool "StringSet roundtrip" (equal s (of_json json))

let test_fun_shadow_schema_contains_fun _ =
  assert_bool "schema must mention fun_ (shadow for 't still works)"
    (ExtLib.String.exists
       (Yojson.Basic.to_string (Jsonkit.Jsonschema.make Sql.stmt_jsonschema))
       ~sub:"fun_")

let extension_prefix = "sqlgg extension not allowed: "

let test_allow_extensions_rejected _ =
  List.iter
    (fun sql ->
      match parse ~allow_extensions:false sql with
      | Error ({ message; location = Span pos }, _) ->
        assert_bool sql (String.starts_with ~prefix:extension_prefix message);
        assert_bool sql (not (Sql.Pos.is_empty pos))
      | Error _ -> assert_failure ("expected a positioned diagnostic for " ^ sql)
      | Ok _ -> assert_failure ("expected rejection of " ^ sql))
    [
      "SELECT * FROM t WHERE id = @id";
      "SELECT 1 WHERE @c { A { 1 = 1 } | B { 1 = 2 } }";
      "SELECT 1 WHERE { 1 = 1 }?";
      "WITH t AS &some_query SELECT * FROM t";
      "CREATE TABLE t (x INT DEFAULT (@p))";
      "SELECT 1 WHERE (1, 2) IN @pairs";
    ]

let test_allow_extensions_accepted _ =
  ignore (get_ok (parse ~allow_extensions:false "SELECT id FROM t WHERE id = 1"));
  ignore (get_ok (parse "SELECT * FROM t WHERE id = @id"));
  ignore (get_ok (parse "SELECT id FROM t WHERE id = 1"))

let test_allow_extensions_analyze _ =
  let context = context "CREATE TABLE t (id INT NOT NULL)" in
  begin match analyze ~allow_extensions:false ~context
                "SELECT id FROM t WHERE id = @id" with
  | Error ({ message; _ }, _) ->
    assert_bool "analyze rejects param" (String.starts_with ~prefix:extension_prefix message)
  | Ok _ -> assert_failure "expected analyze rejection"
  end;
  ignore (get_ok (analyze ~allow_extensions:false ~context
                    "SELECT id FROM t WHERE id = 1"))

let suite =
  "json" >:::
  [
    "stmt_jsonschema_builds" >:: test_stmt_jsonschema_builds;
    "corpus_roundtrip" >:: test_corpus_roundtrip;
    "syntax_signature_roundtrip" >:: test_syntax_signature_roundtrip;
    "manual_enum_kind_ctors" >:: test_manual_enum_kind_ctors;
    "manual_string_set" >:: test_manual_string_set;
    "manual_constraints" >:: test_manual_constraints;
    "manual_meta" >:: test_manual_meta;
    "fun_shadow_schema_contains_fun" >:: test_fun_shadow_schema_contains_fun;
    "allow_extensions_rejected" >:: test_allow_extensions_rejected;
    "allow_extensions_accepted" >:: test_allow_extensions_accepted;
    "allow_extensions_analyze" >:: test_allow_extensions_analyze;
  ]

let () = run_test_tt_main suite
