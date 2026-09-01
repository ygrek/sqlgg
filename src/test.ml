open Printf
open ExtLib
open OUnit
open Sqlgg
open Sql
(* open Sql.Type *)
open Stmt

let schema_to_attrs schema =
  List.filter_map (function
    | Sql.Attr attr -> Some attr
    | Dynamic _ -> None
  ) schema

let cmp_param p1 p2 = p1.id.value = p2.id.value && Type.equal p1.typ p2.typ && p1.id.pos = (0,0) && snd p2.id.pos > fst p2.id.pos

let cmp_params p1 p2 =
  try
    List.for_all2 cmp_param p1 p2
  with
    _ -> false

let parse sql =
  match Main.extract_statement' (Main.lex_tokens (Lexing.from_string sql)) with
  | None -> raise Enum.No_more_elements 
  | Some (buffer, _, _) ->
      match Main.parse_one (buffer,[]) with
      | exception exn -> assert_failure @@ sprintf "failed : %s : %s" (Printexc.to_string exn) sql
      | [] -> assert_failure @@ sprintf "Failed to parse : %s" sql
      | stmt :: _ -> stmt
let assert_params_with_meta stmt meta = 
    let meta = List.map (fun (p, m) -> p, Meta.of_list m) meta in
    assert_equal 
      ~msg:"params with meta" 
      ~cmp:(fun p1 p2 ->
        try
          List.for_all2 
            (fun (p1, m1) (p2, m2) -> cmp_param p1 p2 && Meta.equal m1 m2) 
            p1 
            p2
        with _ -> false)
      ~printer:[%derive.show: (Type.t Sql.param * Sql.Meta.t) list]
      meta
      (List.map 
        (
          function
          | Single (p, m) -> (p, m) 
          | SingleIn (p, m) -> (p, m) 
          | ChoiceIn { vars = [ SingleIn (p, m) ]; _ } -> (p, m)
          | DynamicSelect _ -> failwith "dynamic selects not supported for this host language"
          | _ -> assert false
          ) 
        stmt.Gen.vars)

let cmp_attrs = Stdlib.List.equal Sql.equal_attr

let do_test ?kind sql schema params =
  let stmt = parse sql in
  assert_equal ~msg:"schema" ~cmp:cmp_attrs ~printer:Sql.Schema.to_string schema (schema_to_attrs stmt.schema);
  assert_equal ~msg:"params" ~cmp:cmp_params ~printer:Sql.show_params params
  (List.map (function Single (p, _) -> p | DynamicSelect _ -> failwith "dynamic selects not supported for this host language" | _ -> assert false) stmt.vars);

  match kind with
  | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
  | None -> ()

let tt sql ?kind schema params =
  let test () = do_test sql ?kind schema params in
  sql >:: test

(** Test helper for queries with Choice parameters - only checks schema *)
let tt_schema_only sql ?kind schema =
  let test () =
    let stmt = parse sql in
    assert_equal ~msg:"schema" ~cmp:cmp_attrs ~printer:Sql.Schema.to_string schema (schema_to_attrs stmt.schema);
    match kind with
    | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
    | None -> ()
  in
  sql >:: test

let wrong sql =
  sql >:: (fun () -> ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one' (sql,[])); false with _ -> true))

let attr ?(extra=[]) ?(meta = []) n d = make_attribute ~meta n (Some d) (Constraints.of_list extra)
let check name sql expected = name >:: (fun () -> assert_params_with_meta (parse sql) expected)

let attr' ?(extra=[]) ?(nullability=Type.Strict) ?(meta = []) name kind =
  let domain: Type.t = { t = kind; nullability; } in
  {name;domain;extra=Constraints.of_list extra; meta = Meta.of_list meta; }

let named s t = make_param ~id:(make_located ~value:(Some s) ~pos:(0,0)) ~typ:(Type.strict t)
let named_nullable s t = make_param ~id:(make_located ~value:(Some s) ~pos:(0,0)) ~typ:(Type.nullable t)
let param_nullable t = make_param ~id:(make_located ~value:None ~pos:(0,0)) ~typ:(Type.nullable t)
let param t = make_param ~id:(make_located ~value:None ~pos:(0,0)) ~typ:(Type.strict t)

let test = Type.[
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)" [] [];
  tt "SELECT str FROM test WHERE id=?"
     [attr' ~nullability:Nullable "str" Text]
     [param Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr' ~nullability:Nullable "x" Text; attr' "z" Int]
     [param Int; named "id" Int; named "lim" Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [  attr' ~nullability:Nullable "name" Text;
        attr' ~nullability:Nullable "other_name" Text
     ]
     [named "delta" Int];
  tt "select test.name from test where test.id + @x = ? or test.id - @x = ?"
     [attr' ~nullability:Nullable "name" Text;]
     [named "x" Int; param Int; named "x" Int; param Int;];
  tt "SELECT name FROM test WHERE name IS NOT NULL"
     [attr' "name" Text]  (* Strict, not Nullable *)
     [];
  (* IS NOT NULL refinement: column still nullable in output if not in WHERE *)
  tt "SELECT name, str FROM test WHERE str IS NOT NULL"
     [attr' "name" ~nullability:Nullable Text;  (* name not refined *)
      attr' "str" Text]                          (* str IS refined *)
     [];
  (* IS NOT NULL refinement: multiple columns *)
  tt "SELECT name, str FROM test WHERE name IS NOT NULL AND str IS NOT NULL"
     [attr' "name" Text;
      attr' "str" Text]
     [];
  (* IS NOT NULL refinement: doesn't apply if OR *)
  tt "SELECT name FROM test WHERE name IS NOT NULL OR id = 1"
     [attr' "name" ~nullability:Nullable Text]  (* name could still be null in OR branch *)
     [];
  tt "SELECT name, str FROM test WHERE name IS NOT NULL OR str IS NOT NULL"
     [attr' "name" ~nullability:Nullable Text;
      attr' "str"  ~nullability:Nullable Text]
     [];
  (* IS NOT NULL refinement: apply with AND *)
  tt "SELECT name FROM test WHERE name IS NOT NULL AND id = 1"
     [attr' "name" Text]
     [];
  (* IS NOT NULL refinement: with select * *)
  tt "SELECT * FROM test WHERE name IS NOT NULL"
     [attr' "id" ~nullability:Nullable Int;
      attr' "str" ~nullability:Nullable Text;
      attr' "name" Text]
     [];
  (* IS NOT NULL refinement: with alias *)
  tt "SELECT name as my_name FROM test WHERE name IS NOT NULL"
     [attr' "my_name" Text]
     [];
  (* IS NOT NULL refinement: with IS NULL *)
  tt "SELECT name FROM test WHERE name IS NULL"
     [attr' "name" ~nullability:Nullable Text]
     [];
  (* IS NOT NULL refinement: with computed expression - tests position alignment *)
  tt "SELECT 2+2, str FROM test WHERE str IS NOT NULL"
     [attr' "" Int;
      attr' "str" Text]
     [];
  (* IS NOT NULL refinement: double negation *)
  tt "SELECT name FROM test WHERE NOT (name IS NULL)"
     [attr' "name" Text]
     [];
  (* IS NOT NULL refinement: De Morgan's law - NOT (A AND B) *)
  tt "SELECT name, str FROM test WHERE name IS NOT NULL AND NOT (str IS NULL AND id IS NULL)"
     [attr' "name" Text;
      attr' "str" ~nullability:Nullable Text]
     [];
  (* IS NOT NULL refinement: nested negations with De Morgan *)
  tt "SELECT name FROM test WHERE NOT (NOT (name IS NOT NULL))"
     [attr' "name" Text]
     [];
  (* IS NOT NULL refinement: combined with other conditions *)
  tt "SELECT name FROM test WHERE name IS NOT NULL AND name LIKE 'A%'"
     [attr' "name" Text]
     [];
  (* IS NOT NULL refinement: with joins *)
  tt "SELECT t1.name, t2.str FROM test t1 JOIN test t2 ON t1.id = t2.id WHERE t1.name IS NOT NULL AND t2.str IS NOT NULL"
     [attr' "name" Text;
      attr' "str" Text]
     [];
  (* IS NOT NULL refinement: self-join *)
  tt "SELECT a.name, b.name FROM test a, test b WHERE a.name IS NOT NULL AND b.name IS NOT NULL"
     [attr' "name" Text;
      attr' "name" Text]
     [];
  (* IS NOT NULL refinement: subquery in WHERE *)
  tt "SELECT name FROM test WHERE name IS NOT NULL AND id IN (SELECT id FROM test WHERE str IS NOT NULL)"
     [attr' "name" Text]
     [];
  tt "SELECT name, str FROM test WHERE (name IS NOT NULL AND id > 10) OR (str IS NOT NULL AND id < 5)"
     [attr' "name" ~nullability:Nullable Text;
      attr' "str"  ~nullability:Nullable Text]
     [];
  tt "SELECT name FROM test WHERE (name IS NOT NULL OR id IN (SELECT id FROM test WHERE str IS NOT NULL))"
     [attr' "name" ~nullability:Nullable Text]
     [];
  (* IS NOT NULL refinement: with AND IS NULL *)
  tt "SELECT name, str FROM test WHERE name IS NOT NULL AND str IS NULL"
     [attr' "name" Text; attr' "str" ~nullability:Nullable Text]
     [];
  (* IS NOT NULL refinement: Choices - must be in ALL branches to refine *)
  tt_schema_only "SELECT name FROM test WHERE @choice { A { name IS NOT NULL } | B { TRUE } }"
     [attr' "name" ~nullability:Nullable Text];  (* name still nullable - only checked in A branch *)
  tt_schema_only "SELECT name FROM test WHERE @choice { A { name IS NOT NULL } | B { name IS NOT NULL } }"
     [attr' "name" Text];  (* name refined - checked in all branches *)
  tt_schema_only "SELECT name, str FROM test WHERE @choice { A { name IS NOT NULL AND str IS NOT NULL } | B { TRUE } }"
     [attr' "name" ~nullability:Nullable Text; attr' "str" ~nullability:Nullable Text];  (* both nullable *)
  tt_schema_only "SELECT name, str FROM test WHERE @choice { A { name IS NOT NULL } | B { str IS NOT NULL } }"
     [attr' "name" ~nullability:Nullable Text; attr' "str" ~nullability:Nullable Text];  (* different checks in branches *)
  tt_schema_only "SELECT name, str FROM test WHERE @choice { A { name IS NOT NULL AND str IS NOT NULL } | B { name IS NOT NULL AND str IS NOT NULL } }"
     [attr' "name" Text; attr' "str" Text];  (* both checked in all branches *)

  (* IS NOT NULL refinement with aggregations *)
  (* Aggregation results themselves are not affected by IS NOT NULL in WHERE *)
  tt "SELECT COUNT(name) FROM test WHERE name IS NOT NULL"
     [attr' "" Int]
     [];
  tt "SELECT SUM(id) FROM test WHERE id IS NOT NULL"
     [attr' ~nullability:Nullable "" Int]  (* Still nullable - no rows matching WHERE returns NULL *)
     [];
  (* But GROUP BY columns are refined by IS NOT NULL *)
  tt "SELECT name, COUNT(*), MAX(str) FROM test WHERE name IS NOT NULL GROUP BY name"
     [attr' "name" Text;  (* name is refined to non-nullable *)
      attr' "" Int;
      attr' ~nullability:Nullable "" Text]
     [];
  (* should work with HAVING *)
  tt "SELECT name, COUNT(*) FROM test GROUP BY name HAVING name IS NOT NULL"
     [attr' "name" Text; 
      attr' "" Int]
     [];
  (* De Morgan's law: NOT (A OR name IS NULL) implies name IS NOT NULL *)
  tt "SELECT name FROM test WHERE NOT (id < 10 OR name IS NULL)"
     [attr' "name" Text]
     [];
  (* OR where ALL branches check IS NOT NULL - name guaranteed non-null *)
  tt "SELECT name FROM test WHERE (id > 0 AND name IS NOT NULL) OR (id < 0 AND name IS NOT NULL)"
     [attr' "name" Text]
     [];
  (* Edge case: NOT with AND should not refine *)
  tt "SELECT name FROM test WHERE NOT (id > 0 AND name IS NOT NULL)"
     [attr' "name" ~nullability:Nullable Text]
     [];
  (* Edge case: Mixed OR where only some branches have IS NOT NULL *)
  tt "SELECT name FROM test WHERE (name IS NOT NULL) OR (id > 10)"
     [attr' "name" ~nullability:Nullable Text]
     [];
  (* Edge case: Nested NOT OR *)
  tt "SELECT name, str FROM test WHERE NOT (id > 10 OR (str IS NULL OR name IS NULL))"
     [attr' "name" Text;
      attr' "str" Text]
     [];
  (* All branches of CASES are NOT NULL *)
  tt "SELECT name, str FROM test WHERE CASE WHEN id > 10 THEN name IS NOT NULL WHEN id < 10 THEN str IS NOT NULL AND name IS NOT NULL ELSE name IS NOT NULL END"
     [attr' "name" Text;
      attr' "str" ~nullability:Nullable Text]
     [];
  tt "SELECT name FROM test WHERE CASE WHEN id > 10 THEN name IS NOT NULL END"
     [attr' "name" Text]
     [];
  (* CASE with one branch missing check - should NOT refine *)
  tt "SELECT name FROM test WHERE CASE WHEN id > 10 THEN name IS NOT NULL WHEN id < 10 THEN id > 0 ELSE name IS NOT NULL END"
     [attr' "name" ~nullability:Nullable Text]
     [];
  (* CASE with different columns in branches - should NOT refine either *)
  tt "SELECT name, str FROM test WHERE CASE WHEN id > 10 THEN name IS NOT NULL ELSE str IS NOT NULL END"
     [attr' "name" ~nullability:Nullable Text;
      attr' "str" ~nullability:Nullable Text]
     [];
  tt "SELECT name FROM test WHERE name > ALL (SELECT str FROM test)"
     [attr' "name" ~nullability:Nullable Text]
     [];
  tt "SELECT name FROM test WHERE name > ANY (SELECT str FROM test)"
     [attr' "name" Text]
     [];
  tt "SELECT name, str FROM test WHERE NOT (name IS NULL) AND NOT (str IS NULL)"
     [attr' "name" Text;
      attr' "str" Text]
     [];
  tt "SELECT name, str FROM test WHERE NOT (name IS NULL AND str IS NULL)"
     [attr' "name" ~nullability:Nullable Text;
      attr' "str" ~nullability:Nullable Text]
     [];
  tt "insert into test values"
     []
     [named_nullable "id" Int; named_nullable "str" Text; named_nullable "name" Text];
  tt "insert into test (str,name) values"
     []
     [named_nullable "str" Text; named_nullable "name" Text];
  tt "insert into test values (2,'hello' || ' world',@name)"
     []
     [named_nullable "name" Text];
  tt "insert or replace into test values (2,?,?)" [] [param_nullable Text; param_nullable Text;];
  tt "replace into test values (2,?,?)" [] [param_nullable Text; param_nullable Text;];
 tt "select str, case when id > @id then name when id < @id then 'qqq' else @def end as q from test"
    [attr' ~nullability:Nullable "str" Text; attr' ~nullability:Nullable "q" Text]
    [named_nullable "id" Int; named_nullable "id" Int; named_nullable "def" Text];
   wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr' ~nullability:Nullable "str" Text;]
    [named "id" Int; named "x" Int; named "id" Int];
  tt "select 3/5"
    [attr' ~nullability:Strict "" Float;]
    [];
]

let test2 = [
  tt "CREATE TABLE test2 (id INT, str TEXT)" [] [];
  (* Column 'str' in field list is ambiguous *)
  wrong "update test, (select * from test2) as x set str = x.str where test.id=x.id";
  tt    "update test, (select * from test2) as x set name = x.str where test.id=x.id" [] [];
  tt    "update test, (select * from test2) as x set test.str = x.str where test.id=x.id" [] [];
  wrong "update test, (select * from test2) as x set test.name = x.name where test.id=x.id";
  wrong "update test, (select * from test2) as x set test.str = str where test.id=x.id";
]

let test3 = [
  tt "SELECT id FROM test WHERE str IN ( SELECT str FROM test2 )" [attr "id" Int] [];
  "tuples" >:: (fun () -> todo "tuples");
  (* from http://stackoverflow.com/questions/1063866/sql-portability-gotchas/1063946#1063946 *)
(*   tt "SELECT id FROM test WHERE (id, str) IN ( SELECT id, str FROM test2)" [attr "id" Int] []; *)
]

let test4 =
  let a = [attr "" Int] in
  [
  tt "CREATE TABLE test4 (x INT, y INT)" [] [];
  tt "select max(x) as q from test4" [attr "q" Int] [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x+y) from test4 limit 1" a [] ~kind:(Select `One);
  tt "select max(y) from test4 limit 2" a [] ~kind:(Select `One);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4 limit 1" a [] ~kind:(Select `Zero_one);
  tt "select max(x,y) from test4 limit 2" a [] ~kind:(Select `Nat);
  tt "select 1" [attr' ~nullability:Strict "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10)"  [attr' ~nullability:Strict "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10) where 1 = 2"  [attr' ~nullability:Strict "" Int] [] ~kind:(Select `Zero_one);
  tt "select 1 from test4"  [attr' ~nullability:Strict "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4"  [attr' ~nullability:Strict "" Int] [] ~kind:(Select `Nat);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test"
    [attr' ~nullability:Strict  "" Int; attr' ~nullability:Strict "" Text] [] ~kind:(Select `Nat);
  tt "select greatest(10,x) from test4" [attr' ~nullability:Nullable "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y"  [attr' ~nullability:Strict "" Int] [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr' ~nullability:Nullable "q" Int] [named "n" Int] ~kind:(Select `One);
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr' ~nullability:Strict "q" Int] [named "n" Int] ~kind:(Select `One); 
]

let test_parsing = [
  tt "CREATE TABLE test5_1 (x INT NOT NULL, y INT NOT NULL DEFAULT -1) ENGINE=MEMORY" [] [];
  tt "SELECT 2+3, 2+-3, -10 FROM test5_1" [attr' "" Int; attr' "" Int; attr' "" Int] [];
]

(*
  see MySQL 5.4 refman -- 12.2.8.1. JOIN Syntax
  see SQL:2008 -- 7.7 <joined table>
*)
let test_join_result_cols () =
  Tables.reset ();
  let ints = List.map (fun name ->
    if Stdlib.String.ends_with name ~suffix:"?" then
      attr' ~nullability:Type.Nullable (String.slice ~last:(-1) name) Type.Int
    else
      attr' name Type.Int)
  in
  do_test "CREATE TABLE t1 (i INT, j INT)" [] [];
  do_test "CREATE TABLE t2 (k INT, j INT)" [] [];
  do_test "SELECT * FROM t1 JOIN t2 ON t1.j=t2.j" (ints ["i?";"j";"k?";"j"]) [];
  do_test "SELECT * FROM t1 LEFT JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 RIGHT JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 FULL JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 NATURAL JOIN t2" (ints ["j";"i?";"k?"]) [];
  do_test "SELECT * FROM t1 JOIN t2 USING (j)" (ints ["j";"i?";"k?"]) [];
  do_test "SELECT * FROM t1 NATURAL LEFT JOIN t2" (ints ["j?";"i?";"k?"]) [];
  do_test "SELECT * FROM t1 LEFT JOIN t2 USING (j)" (ints ["j?";"i?";"k?"]) [];
(*   NATURAL JOIN with common column in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE j > @x"
    (ints ["j";"i?";"k?"])
    [named"x" Int];
(*   NATURAL JOIN with common column qualified in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x"
    (ints ["j";"i?";"k?"])
    [named "x" Int];
  ()

let test_enum = [
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["true"; "false"]); is_closed = true })) ~extra:[NotNull;]; attr ~extra:[WithDefault;] "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["true"; "false"]); is_closed = true })) ~extra:[NotNull;]; attr "" Int] [];
]

let test_manual_param = [
  tt "CREATE TABLE test7 (x INT NULL DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test7 WHERE x = @x_arg" [attr' "x" Int ~extra:[Null; WithDefault];] [
    named "x_arg" Int
  ];
  tt "SELECT * FROM test7 WHERE x = @x_arg::Int" [attr' "x" Int ~extra:[Null; WithDefault];] [
    named "x_arg" Int
  ];
  tt "INSERT INTO test7 VALUES (@x_arg)" [] [
    named_nullable "x_arg" Int
  ];
  tt "UPDATE test7 SET x = @x_arg WHERE x = @x_arg_2" [] [
    named_nullable "x_arg" Int;
    named "x_arg_2" Int
  ];
  tt "UPDATE test7 SET x = @x_arg ::Int WHERE x = @x_arg_2 :: Int" [] [
    named "x_arg" Int;
    named "x_arg_2" Int
  ];
]

let test_left_join = [
  tt "CREATE TABLE account_types ( type_id INT NOT NULL PRIMARY KEY, type_name VARCHAR(255) NOT NULL )" [] [];
  tt "CREATE TABLE users (id INT NOT NULL, user_id INT NOT NULL PRIMARY KEY, name VARCHAR(255), email VARCHAR(255), account_type_id INT NULL, FOREIGN KEY (account_type_id) REFERENCES account_types(type_id))" [][];
  tt "SELECT users.name, users.email, account_types.type_name FROM users LEFT JOIN account_types ON users.account_type_id = account_types.type_id"
  [attr "name" Text ~extra:[]; attr "email" Text ~extra:[]; 
  {name="type_name"; domain=Type.nullable Text; extra=(Constraints.of_list [Constraint.NotNull]);meta = Meta.empty();}] [];
]

let test_coalesce = [
  tt "CREATE TABLE test8 (x integer unsigned null)" [] [];
  tt "SELECT COALESCE(x, null, null) as x FROM test8" [attr' ~nullability:Nullable "x" Int;] [];
  tt "SELECT COALESCE(x, coalesce(null, null, 75, null), null) as x FROM test8" [attr' ~nullability:Strict "x" Int;] [];
]

let test_primary_strict = [
  tt "CREATE TABLE test9 (x BIGINT UNSIGNED PRIMARY KEY)" [] [];
  tt "SELECT x FROM test9 WHERE x > 100" [attr' ~extra:[PrimaryKey] ~nullability:Strict "x" UInt64;] [];
]

let test_not_null_default_field = [
  tt "CREATE TABLE test10 (id INT PRIMARY KEY, name VARCHAR(255) NOT NULL)" [] [];
  wrong "INSERT INTO test10 (id) VALUES (1)";
  tt "INSERT INTO test10 (id, name) VALUES (1, '2')" [] [];
  tt "CREATE TABLE test11 (aa int(10) unsigned NOT NULL DEFAULT 2, b TEXT NOT NULL)" [][];
  tt "INSERT INTO test11 (b) VALUES ('abcd')" [][];
]

let test_update_join = [
  tt "CREATE TABLE test12 (c_id INT PRIMARY KEY, c_name VARCHAR(50) NOT NULL)" [] [];
  tt "CREATE TABLE test13 (s_id INT PRIMARY KEY, s_name VARCHAR(50) NOT NULL, c_id INT NOT NULL)" [] [];
  tt "CREATE TABLE test14 (s_id INT PRIMARY KEY, g INT NOT NULL)" [] [];

  tt {|
    UPDATE test12
    JOIN test13 t13 ON t13.c_id = test12.c_id
    JOIN test14 t14 ON t14.s_id = t13.s_id
    SET t14.g = t14.g + 100, 
    test12.c_name = @c_name,
    t13.s_name = @s_name
    WHERE test12.c_id = @c_id
  |} [] [
    named "c_name" Text;
    named "s_name" Text;
    named "c_id" Int
  ];
]

let test_param_not_null_by_default = [
  tt "CREATE TABLE test15 (a INT, b INT NULL, c TEXT NULL)" [] [];
  tt "CREATE TABLE test16 (d INT)" [] [];
  tt {| 
    SELECT a FROM test15 
    WHERE a = @a 
    AND a + b = @ab
    AND a + @x = 10
    AND c = @c AND a < (@a2 :: Int Null)
    AND (SELECT d FROM test16 LIMIT 1) = @d
  |} [attr' "a" Int ~extra:[];] [
    named "a" Int;
    named "ab" Int;
    named "x" Int;
    named "c" Text;
    named_nullable "a2" Int;
    named "d" Int;
  ];
  tt {|
    UPDATE test15 
    SET a = @a 
    WHERE b = @b AND a = @where_a
  |} [] [
    named_nullable "a" Int;
    named "b" Int;
    named "where_a" Int;
  ];
]

(* Since @abc is tuple list, but TupleList isn't a Sql.type *)
let test_in_clause_with_tuple_sets () = 
  do_test "CREATE TABLE test17 (a INT, b INT NULL, c TEXT NULL)" [] [];
  let stmt = parse {| 
    SELECT a FROM test17 
    WHERE (a, b, c) IN @abc
  |} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [attr' "a" Int] (schema_to_attrs stmt.schema);
  ()

let test_agg_nullable = [
  tt {|
    CREATE TABLE test19 (
      a INT NOT NULL,
      b INT NOT NULL
    )
  |} [] [];
  tt {|
    CREATE TABLE test20 (
      c INT NOT NULL,
      d INT NOT NULL
    )
  |} [] [];
  tt "CREATE TABLE test18 (id INT, value INT NOT NULL)" [] [];
  tt {| 
    SELECT AVG(value) as avg_value FROM test18
  |} [attr' ~nullability:Nullable "avg_value" Float] [];
  tt {| 
    SELECT MAX(value) as max_value FROM test18
  |} [attr' ~nullability:Nullable "max_value" Int] [];
  tt {| 
    SELECT MAX(value) as max_value FROM test18 GROUP BY id
  |} [attr' "max_value" Int] [];
  tt {| 
    SELECT MAX(value) as max_value, MAX(id) as max_id
    FROM test18 GROUP BY id
  |} [attr' "max_value" Int; attr' "max_id" ~nullability:Nullable Int] [];
  tt {| 
    SELECT AVG(value) as avg_value, AVG(id) as avg_id
    FROM test18
  |} [
    attr' "avg_value" ~nullability:Nullable Float; 
    attr' "avg_id" ~nullability:Nullable Float
  ] [];
  tt {| 
    SELECT MAX((SELECT value FROM test18 WHERE value = 100)) AS result
    FROM test18
    GROUP BY value
  |} [
  attr' "result" ~nullability:Nullable Int; 
  ] [];
  tt {| 
    SELECT MAX((
      SELECT MAX((
        SELECT value FROM test18 WHERE value = 100 GROUP BY value
      )) AS result_0
    )) AS result
    FROM test18
    GROUP BY value
  |} [
  attr' "result" ~nullability:Nullable Int; 
  ] [];
  tt {| 
    SELECT MAX(COALESCE(((SELECT value FROM test18 WHERE value = 100)), 1)) AS result
    FROM test18
    GROUP BY value
  |} [
  attr' "result" Int; 
  ] [];
  tt {|
    SELECT MAX(c) as result
    FROM test19
    LEFT JOIN test20 on test19.a = test20.c
    GROUP BY b
  |} [ attr' ~nullability:Nullable "result" Int; ][];
  tt {|
    SELECT MAX(a) as result
    FROM test19
    LEFT JOIN test20 on test19.a = test20.c
    GROUP BY b
  |} [ attr' "result" Int; ][];
  tt {|
    SELECT MAX(c) as result
    FROM test19
    JOIN test20 on test19.a = test20.c
    GROUP BY b
  |} [ attr' "result" Int; ][];
]

let cte_possible_rec_non_shared_select_only = [
  tt {|
    WITH RECURSIVE sequence_cte AS (
      SELECT 1 AS num
      UNION ALL
      SELECT num + @param1
      FROM sequence_cte
      WHERE num < @param2
    )
    SELECT num
    FROM sequence_cte
  |} [
    attr' "num" Int;
  ] [
    named "param1" Int;
    named "param2" Int;
  ];
  wrong {|
    WITH RECURSIVE sequence_cte AS (
      SELECT 1 AS num
      UNION ALL
      SELECT num + @param1
      FROM sequence_cte
      WHERE num < @param2
      UNION ALL 
      SELECT 'string'
    )
    SELECT num
    FROM sequence_cte
  |};
  tt {|
    CREATE TABLE test21 (
      num INT
    )
  |} [][];
  tt {|
    WITH cte AS (
      SELECT num
      FROM test21
      WHERE num <= 3
    )
    SELECT num
    FROM cte
  |} [ attr' ~nullability:Strict "num" Int;][];
  tt {|
    CREATE TABLE test22 (
      col_id INT PRIMARY KEY,
      col_value DECIMAL(10, 2),
      col_group VARCHAR(100)
    )
  |} [][];
  tt {|
    SELECT * FROM test22 
    WHERE col_id IN (
        WITH cte_filtered_ids AS (
          SELECT col_id FROM test22 WHERE col_value > 60000
        )
        SELECT col_id FROM cte_filtered_ids
    )
  |} [
    attr' ~extra:[PrimaryKey] "col_id" Int;
    attr' ~nullability:Nullable "col_value" (Decimal { precision = Some 10; scale = Some 2; });
    attr' ~nullability:Nullable "col_group" Text;
  ] [
  ];
  tt {|
    SELECT *
    FROM (
        WITH cte_grouped AS (
            SELECT col_group, AVG(col_value) AS avg_value
            FROM test22
            GROUP BY col_group
        )
        SELECT col_group, avg_value
        FROM cte_grouped
    ) AS dt
    WHERE dt.avg_value
  |} [
    attr' ~nullability:Nullable "col_group" Text;
    attr' ~nullability:Strict "avg_value" Float;
  ] [];
  tt {|
    INSERT INTO test22 (col_id, col_value, col_group)
    WITH new_values AS (
        SELECT 101 AS col_id, 55 AS col_value, 'Group A' AS col_group
        UNION ALL
        SELECT 102, 60, 'Group B'
        UNION ALL
        SELECT 103, 70, 'Group A'
    )
    SELECT col_id, col_value, col_group
    FROM new_values
  |}[][];
  
  tt {|
    WITH RECURSIVE cte(num_name_just_an_alias_here) AS (
      SELECT 1 AS n
      UNION ALL
      SELECT num_name_just_an_alias_here + 1 FROM cte
      LIMIT 10
    )
    SELECT * FROM cte
  |} [attr' "num_name_just_an_alias_here" ~extra:[] Int;] [];
  tt {|
    WITH cte(cg) AS (
      SELECT col_group FROM test22 WHERE col_id > 60000
    )
    SELECT cg FROM cte
  |} [
    attr' ~nullability:Nullable ~extra:[] "cg" Text;
  ] [
  ];
  tt {|
    WITH cte(explicit_null_doesnt_become_not_null) AS (
      SELECT NULL
    )
    SELECT * FROM cte
  |} [
    attr' ~nullability:Nullable ~extra:[] "explicit_null_doesnt_become_not_null" Any;
  ] [
  ];
  wrong {|
    WITH cte(num_name_just_an_alias_here) AS (
      SELECT 1 AS n
      UNION ALL
      SELECT num_name_just_an_alias_here + 1 FROM cte
      LIMIT 10
    )
    SELECT * FROM cte
  |};
]

let test_ambiguous = [
  tt "CREATE TABLE test23 (id INT, column_a TEXT, column_b BOOL)" [] [];
  tt "CREATE TABLE test24 (id INT, column_d INT)" [] [];
  wrong "select id from test23 join test24 on test23.id = test24.id order by id";
  (* The difference between this example, and the same but with WHERE (following "wrong" fn) is
     sql engine uses those columns that were mentioned in the SELECT statement, 
     while it doesn't do that for WHERE.
  *)
  tt "select test23.id from test23 join test24 on test23.id = test24.id order by id" [
    attr' ~nullability:Strict "id" Int;
  ] [];
  (* Wrong parses and asserts fail *)
  wrong "select test23.id from test23 join test24 on test23.id = test24.id where id > 2 order by id";
  tt "select test23.id from test23 join test24 on test23.id = test24.id group by id" [
    attr' ~nullability:Strict "id" Int;
  ] [];
  tt "select test23.id as test from test23 join test24 on test23.id = test24.id group by column_a" [
    attr' ~nullability:Strict "test" Int;
  ][];
  tt "select test23.id, test24.id from test23 join test24 on test23.id = test24.id" [
    attr' ~nullability:Strict "id" Int;
    attr' ~nullability:Strict "id" Int;
  ] [];
  (* Wrong parses and asserts fail *)
  wrong "select id, id from test23 join test24 on test23.id = test24.id group by id";
  wrong "select id as id1, id as id2 from test23 join test24 on test23.id = test24.id group by id";
  wrong "select test23.id, test24.id from test23 join test24 on test23.id = test24.id group by id";
  tt "select test23.id from test23 join test24 on test23.id = test24.id group by id, column_a" [
    attr' ~nullability:Strict "id" Int;
  ] [];
  tt "SELECT COUNT(column_a) as column_a FROM test23 WHERE column_a = @column_a" [
    (* COUNT(column_a :: Text) :: Int *)
    attr' "column_a" Int;
  ] [
    named "column_a" Text;
  ];
  wrong "select * from test23 join test24 on test23.id = test24.id group by id" ;
  tt "CREATE TABLE test25 (id INT)" [] [];
  tt "CREATE TABLE test26 (id INT)" [] [];
  wrong "select * from foo join bar on foo.id";
  tt "SELECT test23.id AS id1, test24.id AS id2 FROM test23 JOIN test24 ON test23.id = test24.id" [
    attr' ~nullability:Strict "id1" Int;
    attr' ~nullability:Strict "id2" Int;
  ] [];
  tt "SELECT test23.id, test24.id FROM test23 JOIN test24 ON test23.id = test24.id GROUP BY test23.id" [
    attr' ~nullability:Strict "id" Int;
    attr' ~nullability:Strict "id" Int;
  ][];
  wrong "SELECT COUNT(id) FROM test23 JOIN test24 ON test23.id = test24.id";
  wrong "SELECT COUNT(id) as id FROM test23 JOIN test24 ON test23.id = test24.id";
  wrong "SELECT id FROM test23 JOIN test24 ON test23.id = test24.id WHERE id > 2";
  tt "SELECT test23.id AS test_id, test24.id AS other_id FROM test23 JOIN test24 ON test23.id = test24.id" [
    attr' ~nullability:Strict "test_id" Int;
    attr' ~nullability:Strict "other_id" Int;
  ] [];
  tt "SELECT COUNT(test23.id) AS count_id FROM test23 JOIN test24 ON test23.id = test24.id" [
    attr' "count_id" Int;
  ] [];
  tt "CREATE TABLE test27 (id INT, value INT)" [] [];
  tt "CREATE TABLE test28 (id INT, value INT)" [] [];
  tt "CREATE TABLE test29 (id INT, value INT)" [] [];
  tt {|
    SELECT t1.id AS id_from_test27, t2.value AS value_from_test28, t3.value AS value_from_test29
    FROM test27 t1
    JOIN test28 t2 ON t1.id = t2.id
    JOIN test29 t3 ON t1.id = t3.id
  |}[
    attr' ~nullability:Strict "id_from_test27" Int;
    attr' ~nullability:Nullable "value_from_test28" Int;
    attr' ~nullability:Nullable "value_from_test29" Int;
  ][];
  (* In WHERE aliases aren't available *)
  wrong {|
    SELECT MAX(id) AS max_id
    FROM test23
    WHERE max_id > 0
  |};  
]

let test_subquery_nullability = [
  tt {| 
    CREATE TABLE table_30 (
      column_1 INT PRIMARY KEY,
      column_2 VARCHAR(50) NOT NULL,
      column_3 VARCHAR(50)
    )
  |} [] [];
  
  tt {| 
    CREATE TABLE table_31 (
      column_4 INT PRIMARY KEY,
      column_5 INT NOT NULL,
      column_6 DATE NOT NULL,
      column_7 DECIMAL(10, 2),
      FOREIGN KEY (column_5) REFERENCES table_30(column_1)
    )
  |} [] [];
  
  (* Possible no rows and which means possible null *)
  tt {| 
    SELECT 
      t30.column_2 AS info1,
      t30.column_3 AS info2,
      (SELECT MAX(column_6)
       FROM table_31 t31
       WHERE t31.column_5 > 99999
       GROUP BY t31.column_5
       LIMIT 1
      ) AS max_info
    FROM table_30 t30
    JOIN table_31 t31 ON t30.column_1 = t31.column_5
    GROUP BY t30.column_1, t30.column_2, t30.column_3
  |} [
    attr' ~extra:[NotNull] "info1" Text;
    attr' ~nullability:Nullable "info2" Text;
    attr' ~nullability:Nullable "max_info" Datetime;
  ] [];
  
  (* Count never returns null, it's counter and it isn't aggregation *)
  tt {| 
    SELECT 
      t30.column_2 AS info1,
      t30.column_3 AS info2,
      (SELECT COUNT(column_6)
       FROM table_31 t31
       WHERE t31.column_5 > 99999
       GROUP BY t31.column_5
       LIMIT 1
      ) AS max_info
    FROM table_30 t30
    JOIN table_31 t31 ON t30.column_1 = t31.column_5
    GROUP BY t30.column_1, t30.column_2, t30.column_3
  |} [
    attr' ~extra:[NotNull] "info1" Text;
    attr' ~nullability:Nullable "info2" Text;
    attr' "max_info" Int;
  ] [];

  (* dependent + null = null *)
  tt {| 
    SELECT 
      t30.column_2 AS info1,
      t30.column_3 AS info2,
      (SELECT IF(COUNT(column_6) = 1111111, 3, NULL)
       FROM table_31 t31
       WHERE t31.column_5 > 99999
       GROUP BY t31.column_5
       LIMIT 1
      ) AS max_info
    FROM table_30 t30
    JOIN table_31 t31 ON t30.column_1 = t31.column_5
    GROUP BY t30.column_1, t30.column_2, t30.column_3
  |} [
    attr' ~extra:[NotNull] "info1" Text;
    attr' ~nullability:Nullable "info2" Text;
    attr' ~nullability:Nullable "max_info" Int;
  ] [];

  tt {| 
    SELECT (SELECT 1 WHERE 0) as result
  |} [
    attr' ~nullability:Nullable  "result" Int;
  ] [];

 (* no way to have null *)
  tt {| 
    SELECT 1 as one, (SELECT COUNT(NULL)) as result
  |} [
    attr' "one" Int;
    attr' "result" Int;
  ] [];

  (* no way to have null *)
  tt {| 
   SELECT 1 as one, (SELECT COUNT(NULL) HAVING FALSE) as result
  |} [
   attr' "one" Int;
   attr' ~nullability:Nullable "result" Int;
  ] [];

  (* it doesn't return null, reason: WHERE is evaluated after the Aggregation *)
  tt {| 
    SELECT 1 as one, (SELECT COUNT(NULL) + 1 WHERE 0) as result
  |} [
    attr' "one" Int;
    attr' "result" Int;
  ] [];

  tt {| 
    SELECT 1 as one, (SELECT (SELECT (SELECT (SELECT COUNT(NULL))))) as result
  |} [
    attr' "one" Int;
    attr' "result" Int;
  ] [];

  tt {| 
    SELECT 1 as one, (SELECT (SELECT (SELECT (SELECT COUNT(NULL) + 1) + 1) + 1)) as result
  |} [
    attr' "one" Int;
    attr' "result" Int;
  ] [];

  tt {| 
   SELECT 1 AS one, 
    (SELECT 
        IF(
            (SELECT 
                (SELECT COUNT(NULL) + 1) + 1
            ) + 1 > 49876, 
            129, 
            NULL
        )
    ) AS result
  |} [
    attr' "one" Int;
    attr' ~nullability:Nullable "result" Int;
  ] [];

  (* good reflects the essence of what is happening *)
  tt {|SELECT 1 AS one, (SELECT COUNT(NULL) + 1 + MAX(NULL)) AS result|} [
   attr' "one" Int;
   attr' ~nullability:Nullable "result" Int;
 ] [];

  tt {| 
    SELECT 
      1 as one,
      (SELECT column_6
       FROM table_31 t31
       WHERE t31.column_5 = 123
      ) as abcd
    FROM table_30 t30
  |} [
    attr' "one" Int;
    attr' ~nullability:Nullable "abcd" Datetime;
  ] [];
]

let test_values_row = [
  tt {|
    SELECT column_2, i
    FROM table_30 t30
    JOIN ( VALUES ROW('a', 1), ROW('b', 2), ROW('c', 3) ) AS x (txt, i)
    ON t30.column_2 = x.txt
  |} [
    attr' ~extra:[NotNull] "column_2" Text;
    attr' "i" Int;
  ] [];

  (* Unification fail (last ROW has different type) *)
  wrong {|
    SELECT column_2, i
    FROM table_30 t30
    JOIN ( VALUES ROW('a', 1), ROW('b', 2), ROW(2, 3) ) AS x (txt, i)
    ON t30.column_2 = x.txt
  |} ;

  (* Alias error *)
  wrong {|
    SELECT column_2, i
    FROM table_30 t30
    JOIN ( VALUES ROW('a', 1), ROW('b', 2), ROW(2, 3) ) AS y (txt, i)
    ON t30.column_2 = y.txt
  |};
]

let test_select_exposed_alias = [
  tt {|
    CREATE TABLE table_32 (
      col_1 INT PRIMARY KEY,
      col_2 VARCHAR(100),
      col_3 VARCHAR(50),
      col_4 DECIMAL(10,2)
    )
  |} [] [];

  tt {|
    CREATE TABLE table_33 (
      col_1 INT PRIMARY KEY,
      col_2 INT,
      col_3 INT,
      col_4 DATE
    )
  |} [] [];

  tt {| SELECT y.* FROM (
    SELECT col_1, col_2, col_1 * col_2 as calc FROM table_33
  ) as y (a, b, c) |} [
    attr' "a" Int;
    attr' ~nullability:Nullable "b" Int;
    attr' ~nullability:Nullable "c" Int;
  ] [];

  tt {| SELECT z.* FROM (
    SELECT 
        t1.col_3,
        COUNT(*) as cnt,
        SUM(t2.col_3 * t1.col_4) as calc,
        t1.col_4
    FROM table_32 t1
    JOIN table_33 t2 ON t1.col_1 = t2.col_2
    GROUP BY t1.col_3
  ) as z (a, b, c, d) |} [
    attr' ~nullability:Nullable "a" Text;
    attr' "b" Int;
    attr' ~nullability:Nullable "c" (Decimal { precision = Some 10; scale = Some 2; });
    attr' ~nullability:Nullable "d" (Decimal { precision = Some 10; scale = Some 2; });
  ] [];

  tt {| SELECT outer_x.* FROM (
    SELECT inner_x.*,
           NOT inner_x.d as bonus_not
    FROM (
        SELECT 
            'abc' as str,
            42 as num,
            2.5 as price,
            true as flag
    ) as inner_x (a, b, c, d)
  ) as outer_x (str, num, price, flag, bonus) |} [
    attr' "str" (StringLiteral "abc");
    attr' "num" Int;
    attr' "price" (FloatingLiteral 2.5);
    attr' "flag" Bool;
    attr' "bonus" Bool;
] [];
]

let test_enum_as_variant = [
  "test_enum_as_variant" >:: (fun _ ->

    do_test "CREATE TABLE test35 (status enum('active','pending','deleted') NOT NULL DEFAULT 'pending')" [] [];
 
    do_test "SELECT status FROM test35" [
      attr' ~extra:[NotNull; WithDefault] "status" 
        (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"]); is_closed = true }))
    ] [];
   
    do_test "INSERT INTO test35 (status) VALUES (@status)" [] [
      named "status" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"]); is_closed = true }))
    ];
  )
]

let test_enum_literal () = 

  do_test "CREATE TABLE test36 (status enum('active','pending','deleted') NOT NULL DEFAULT 'pending')" [] [];
  
  let stmt = parse {|INSERT INTO test36 VALUES('pending')|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] (schema_to_attrs stmt.schema);

  let stmt2 = parse {|INSERT INTO test36 VALUES('active')|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] (schema_to_attrs stmt2.schema);
  
  let stmt3 = parse {|INSERT INTO test36 VALUES('deleted')|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] (schema_to_attrs stmt3.schema);

  let stmt4 = parse {|SELECT * FROM test36 WHERE status = 'active'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string 
    [attr' ~extra:[NotNull; WithDefault] "status" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"] ); is_closed = true }))]
    (schema_to_attrs stmt4.schema);

  let stmt5 = parse {|UPDATE test36 SET status = 'deleted' WHERE status = 'pending'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] (schema_to_attrs stmt5.schema);

  let stmt6 = parse {|
    SELECT * FROM test36 
    WHERE status IN ('active', 'pending') 
    AND status != 'deleted'
  |} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string
    [attr' ~extra:[NotNull; WithDefault] "status" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"]); is_closed = true }))]
    (schema_to_attrs stmt6.schema);

  ignore @@ wrong {|INSERT INTO test36 VALUES('deleteddd')|} ;
  ignore @@ wrong {|INSERT INTO test36 VALUES((IF(TRUE, 'a', 'b')))|} ;

  let stmt7 = parse {|INSERT INTO test36 VALUES((IF(TRUE, 'pending', 'active')))|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] (schema_to_attrs stmt7.schema);

  ignore @@ wrong {|INSERT INTO test36 VALUES((IF(TRUE, 'pending', 'b')))|};
  ignore @@ wrong {|INSERT INTO test36 VALUES(CONCAT(''))|};

  ignore @@ wrong {|SELECT * FROM test36 WHERE status = 'activee'|};

  let stmt8 = parse {|SELECT CONCAT(status, 'test') AS named FROM test36 WHERE status = 'active'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string 
    [attr' ~extra:[] "named" Text]
    (schema_to_attrs stmt8.schema)

let test_add_with_window_function = [
  (* Most aggregate functions also can be used as window functions *)
  tt {| SELECT SUM(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT COUNT(*) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT AVG(1) OVER() WHERE FALSE |} [attr' ~nullability:Nullable "" Float;] [];
  tt {| SELECT MIN(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT MAX(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT MAX(NULL) OVER() |} [attr' ~nullability:Nullable "" Any;] [];

  (* A frame that never reaches the current row is empty on the rows at that edge *)
  tt "CREATE TABLE win_frame (a INT, b INT)" [] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' "w" Int] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b ROWS UNBOUNDED PRECEDING) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' "w" Int] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' "w" Int] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b ROWS BETWEEN 2 PRECEDING AND 1 PRECEDING) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' ~nullability:Nullable "w" Int] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b ROWS BETWEEN 1 FOLLOWING AND 2 FOLLOWING) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' ~nullability:Nullable "w" Int] [];
  tt {| SELECT SUM(a) OVER (ORDER BY b ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS w FROM win_frame WHERE a IS NOT NULL |}
     [attr' ~nullability:Nullable "w" Int] [];

  (* Same but with PARTITION BY and ORDER BY *)
  tt {| SELECT SUM(1) OVER(PARTITION BY COALESCE(NULL, 'a')) |} [attr' "" Int;] [];
  tt {| SELECT SUM(1) OVER(ORDER BY 1 + 1) |} [attr' "" Int;] [];
  tt {| SELECT SUM(1) OVER(PARTITION BY CONCAT('a') ORDER BY 1 - 0) |} [attr' "" Int;] [];

  tt {| SELECT 1 + (SELECT COUNT(1) OVER() WHERE FALSE) |} [attr' ~nullability:Nullable "" Int;] [];

  tt {| SELECT CASE WHEN SUM(2) OVER() > 100 THEN 'High' ELSE 'Low' END |} 
    [attr' "" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["High"; "Low"]); is_closed = false }));] [];

  tt {| SELECT (NULL - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) |} [attr' ~nullability:Nullable "" Float;] [];  
  tt {| SELECT (0 - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) |} [attr' "" Float;] [];  

  tt {| SELECT 1 + (SELECT (0 - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) ) |} [attr' ~nullability:Nullable "" Float;] [];

  (* Non window, non agregate can't be  used *)
  wrong "SELECT IF(TRUE, 1, 2) OVER() WHERE FALSE" ;
]

let test_meta_propagation = [
  tt {|
    CREATE TABLE table_37 (
      -- [sqlgg] module=HelloWorld
      col_1 INT PRIMARY KEY,
      col_2 INT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE table_38 (
      -- [sqlgg] module=FooBar
      col_3 INT PRIMARY KEY,
      col_4 TEXT NOT NULL
    )
  |} [] [];

  tt "SELECT col_1, col_2 FROM table_37" [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "col_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt {|
    CREATE TABLE "table_39" (
      -- [sqlgg] module=HelloWorld
      "col_1" INT PRIMARY KEY,
      "col_2" INT NOT NULL
    )
  |} [] [];

  tt {|SELECT "col_1", "col_2" FROM "table_39"|} [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "col_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt "SELECT col_1 + 1 as col_1_with_plus_1, col_2 FROM table_37" [
    attr' ~meta:[] "col_1_with_plus_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt "SELECT col_1, col_2, col_3, col_4 FROM table_37 LEFT JOIN table_38 ON table_37.col_1 = table_38.col_3" [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "col_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
    attr' ~extra:[PrimaryKey;] ~meta:["module", "FooBar"] ~nullability:Nullable "col_3" Int;
    attr' ~extra:[NotNull;] ~nullability:Nullable "col_4" Text;
  ] [];

  tt {|
    SELECT 
      subquery.col_1 as from_subquery_col_1,
      subquery.col_2 as from_subquery_col_2
    FROM 
      (SELECT col_1, col_2 FROM table_37) as subquery
  |} [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "from_subquery_col_1" Int;
    attr' ~extra:[NotNull;] "from_subquery_col_2" Int;
  ] [];

  tt {|
    SELECT 
      outer_query.nested_col_1 as multi_level_col_1
    FROM 
      (
        SELECT 
          inner_query.col_1 as nested_col_1
        FROM 
          (SELECT col_1 FROM table_37) as inner_query
      ) as outer_query
  |} [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "multi_level_col_1" Int;
  ] [];

  tt {|
    SELECT 
      union_result.col_val as union_col_1
    FROM 
      (
        SELECT col_1 as col_val FROM table_37
        UNION
        SELECT col_1 as col_val FROM table_37
      ) as union_result
  |} [
    attr' ~extra:[PrimaryKey;] ~meta:["module", "HelloWorld"] "union_col_1" Int;
  ] [];

  tt {|
    WITH data_cte AS (
      SELECT col_1, col_2 FROM table_37
    )
    SELECT col_1, col_2 FROM data_cte
  |} [
    attr' ~meta:["module", "HelloWorld"] ~extra:[PrimaryKey;] "col_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt "SELECT MAX(col_1) as col_1_max, col_2 FROM table_37" [
    attr' ~extra:[] ~nullability:Nullable ~meta:["module", "HelloWorld"] "col_1_max" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt {|
    SELECT 
      (SELECT col_1 FROM table_37 LIMIT 1) as subquery_col_1,
      col_2 
    FROM table_37
  |} [
    attr' ~meta:["module", "HelloWorld"] ~nullability:Nullable "subquery_col_1" Int;
    attr' ~extra:[NotNull;] "col_2" Int;
  ] [];

  tt {|
    SELECT (SELECT MAX(col_1) FROM table_37) as col_plus_max
    FROM table_37
  |} [
    attr' ~meta:["module", "HelloWorld"] ~nullability:Nullable "col_plus_max" Int;
  ] [];

  tt {|
    SELECT 
      (
        SELECT MAX(
          (
            SELECT col_1 as col_val 
            FROM table_37 
            WHERE col_1 > (SELECT MIN(col_1) FROM table_37)
            LIMIT 1
          )
        )
      ) as deeply_nested_query
    FROM table_37
  |} [
    attr' ~meta:["module", "HelloWorld"] ~nullability:Nullable "deeply_nested_query" Int;
  ] [];

  tt {|
    SELECT 
      (
        SELECT MAX(x.col_val) 
        FROM (
          SELECT col_1 as col_val 
          FROM table_37 
          WHERE col_1 > (SELECT MIN(col_1) FROM table_37)
        ) as x
      ) as deeply_nested_query
    FROM table_37
  |} [
    attr' ~meta:["module", "HelloWorld"] ~nullability:Nullable "deeply_nested_query" Int;
  ] [];
]

let test_case_enum = [
  tt "CREATE TABLE test37 (id INT NOT NULL, status enum('A','B','C') NOT NULL)" [][];
  tt "CREATE TABLE test38 (id INT PRIMARY KEY)" [][];

  (* not exhausted (C not matched) then null *)
  tt "SELECT CASE status WHEN 'A' THEN 1 WHEN 'B' THEN 2 END `value` FROM test37" 
    [attr' ~nullability:Nullable "value" Int;] [];

  (* not exhausted, else branch is presented then not null *)
  tt "SELECT CASE status WHEN 'A' THEN 1 WHEN 'B' THEN 2 ELSE 0 END `value` FROM test37" 
    [attr' "value" Int;] [];

  (* exhausted, else isn't needed *)
  tt "SELECT CASE status WHEN 'A' THEN 1 WHEN 'B' THEN 2 WHEN 'C' THEN 0 END `value` FROM test37" 
    [attr' "value" Int;] [];

  (* not exhausted, else branch isn't presented then null *)
  tt "SELECT CASE WHEN 1 > 10 THEN 'High' END `value`" 
    [attr' "value" ~nullability:Nullable (StringLiteral "High");][];  

  (* not exhausted, else branch isn't presented, 'High' and 'Low' literals make Union *)
  tt "SELECT CASE WHEN 1 > 10 THEN 'High' WHEN FALSE THEN 'Low' END `value`" 
    [attr' "value" ~nullability:Nullable 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["High"; "Low"]); is_closed = false }));][]; 
      
  (* exhausted (else is presented), Int <: Float *)
  tt "SELECT CASE WHEN TRUE THEN 1 ELSE 0.2 END `value`" 
    [attr' "value" Float;][];

  (* nullable since no rows possible inisde THEN *)
  tt {| SELECT CASE WHEN FALSE THEN (SELECT id FROM test37 WHERE FALSE ) ELSE 1 END `value`|} 
    [attr' ~nullability:Nullable "value" Int;][];

  tt {| 
    SELECT CASE WHEN FALSE THEN (SELECT id FROM test37 WHERE FALSE ) ELSE 1 END `value`
  |} 
    [attr' ~nullability:Nullable "value" Int;][];

  (* If COUNT is presented at least in a one branch then the NO ROWS case isn't possible *)
  tt {|
    SELECT
    (SELECT
         CASE
             WHEN TRUE
             THEN 42-0
             WHEN TRUE
             THEN 2-1
             ELSE COUNT(1)
         END
     FROM test38
     WHERE FALSE
    ) AS value
  |} [attr' "value" Int;][];

  (* If COUNT is presented at least in a one branch then the NO ROWS case isn't possible, 
    but if at least on NULL is presented then NULLABLE
  *)
  tt {|
    SELECT
    (SELECT
         CASE
             WHEN TRUE
             THEN NULL
             WHEN TRUE
             THEN 2-1
             ELSE COUNT(1)
         END
     FROM test38
     WHERE FALSE
    ) AS value
  |} [attr' ~nullability:Nullable "value" Int;][];

   tt {|
    SELECT
    (SELECT
         CASE
             WHEN TRUE
             THEN MAX(NULL)
             WHEN TRUE
             THEN 2-1
             ELSE COUNT(1)
         END
     FROM test38
     WHERE FALSE
    ) AS value
  |} [attr' ~nullability:Nullable "value" Int;][];
]

let test_type_mapping_params _ = 
  do_test {| 
    CREATE TABLE test39 (
      -- [sqlgg] module=HelloWorld
      id INT PRIMARY KEY,
      txt TEXT NOT NULL
    )
  |} [] [];

  let stmt = parse {|SELECT id FROM test39 WHERE id = @id|} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [attr' ~extra:[PrimaryKey] ~meta:["module", "HelloWorld"] "id" Int] 
    (schema_to_attrs stmt.schema);
  assert_params_with_meta stmt [(named "id" Int, ["module", "HelloWorld"])];

  (* test in subqery *)
  let stmt = parse {|SELECT id FROM test39 WHERE txt = (SELECT txt FROM test39 WHERE id = @id)|} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [attr' ~extra:[PrimaryKey] ~meta:["module", "HelloWorld"] "id" Int] 
    (schema_to_attrs stmt.schema);
  assert_params_with_meta stmt [(named "id" Int, ["module", "HelloWorld"])];

  let stmt = parse {|SELECT id FROM test39 WHERE txt = (SELECT txt FROM test39 WHERE id = @id OR (txt = @txt OR TRUE) )|} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [
      attr' ~extra:[PrimaryKey] ~meta:["module", "HelloWorld"] "id" Int;
    ] 
    (schema_to_attrs stmt.schema);
  assert_params_with_meta stmt [(named "id" Int, ["module", "HelloWorld"]); (named "txt" Text, [])];

  do_test {| 
    CREATE TABLE test40 (
      -- [sqlgg] module=Txt_module_name
      txt2 TEXT NOT NULL
    )
  |} [] [];

  let stmt = parse {|
    SELECT id, txt2
    FROM test39
    JOIN test40 ON test39.txt = test40.txt2
    WHERE id = @id OR (txt2 = @txt2 OR TRUE)
  |} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [
      attr' ~extra:[PrimaryKey] ~meta:["module", "HelloWorld"] "id" Int;
      attr' ~extra:[NotNull] ~meta:["module", "Txt_module_name"] "txt2" Text;
    ] 
    (schema_to_attrs stmt.schema);
  assert_params_with_meta stmt [
    (named "id" Int, ["module", "HelloWorld"]); 
    (named "txt2" Text, ["module", "Txt_module_name"])
  ];

  let stmt = parse {|
    SELECT id, txt2
    FROM test39
    JOIN test40 ON test39.txt = test40.txt2 AND test40.txt2 = @txt2
  |} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [
      attr' ~extra:[PrimaryKey] ~meta:["module", "HelloWorld"] "id" Int;
      attr' ~extra:[NotNull] ~meta:["module", "Txt_module_name"] "txt2" Text;
    ] 
    (schema_to_attrs stmt.schema);
  assert_params_with_meta stmt [
    (named "txt2" Text, ["module", "Txt_module_name"])
  ];

  let stmt = parse {|
    SELECT txt2
    FROM test40
    WHERE txt2 IN @txt2
  |} in
  assert_params_with_meta stmt [
    (named "txt2" Text, ["module", "Txt_module_name"])
  ];
  
  let stmt = parse {|
    SELECT id, txt2
    FROM test39
    JOIN test40 ON (test39.txt, test40.txt2) IN @txt2
  |} in

  assert_equal 
    ~msg:"params with meta" 
    ~cmp:(fun p1 p2 -> match List.hd p1, List.hd p2 with
      | TupleList ({ value; _ }, Where_in { value = (l1, _); pos = _ }), TupleList ({ value = value2; _ }, Where_in { value = (l2, _); pos = _ }) -> 
        value = value2 && l1 = l2
      | _ -> false
    )
    ~printer:show_vars
    stmt.vars 
    [
      TupleList (make_located ~value:(Some "txt2") ~pos:(0, 0), Where_in (make_located ~value:([
        Type.strict Text, Meta.empty ();
        Type.strict Text, Meta.of_list ["module", "Txt_module_name"];
      ], `In) ~pos:(0,0)));
    ];
  let stmt = parse {|
    SELECT id = @id as booo
    FROM test39
    LIMIT 1
  |} in
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [
      attr' "booo" Bool;
    ] 
    (schema_to_attrs stmt.schema);

  (* not only in WHERE expr *)
  assert_params_with_meta stmt [
    (named "id" Int, ["module", "HelloWorld"])
  ];

  do_test {|
    CREATE TABLE test41 (
      -- [sqlgg] module=Module1
      col_1 INT PRIMARY KEY,
      
      -- [sqlgg] module=Module2  
      col_2 CHAR(36) NOT NULL,
      
      -- [sqlgg] module=Module3
      col_3 DECIMAL(10,2) NOT NULL,
      
      -- [sqlgg] module=Module4
      col_4 ENUM('status_1', 'status_2', 'status_3', 'status_4') NOT NULL,
      col_5 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )
  |} [] [];

  do_test {|
    CREATE TABLE test42 (
      -- [sqlgg] module=Module2
      col_2 CHAR(36) PRIMARY KEY,
      
      -- [sqlgg] module=Module5
      col_6 VARCHAR(255) NOT NULL UNIQUE,
      
      -- [sqlgg] module=Module3
      col_7 DECIMAL(10,2) NOT NULL DEFAULT 0.0,
      
      -- [sqlgg] module=Module6
      col_8 ENUM('status_a', 'status_b', 'status_c') NOT NULL,
      
      col_9 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )
  |} [] [];

  let stmt = parse {|
    WITH 
      subquery_1 AS (
        SELECT 
          col_2, 
          SUM(col_3) as computed_1,
          COUNT(*) as computed_2
        FROM test41
        WHERE col_5 > @param_1 AND col_3 > @param_2
        GROUP BY col_2
        HAVING SUM(col_3) > @param_3
      )
    SELECT 
      t2.col_2 as a, 
      t2.col_6 as b, 
      t2.col_8 as c,
      sq.computed_1 as d,
      sq.computed_2 as e
    FROM test42 t2
    JOIN subquery_1 sq ON t2.col_2 = sq.col_2
    WHERE t2.col_8 = @param_4
    ORDER BY sq.computed_1 DESC
    LIMIT @param_5
  |} in 
  assert_equal 
    ~msg:"schema" 
    ~printer:Sql.Schema.to_string 
    [
      attr' ~extra:[PrimaryKey] ~meta:["module", "Module2"] "a" Text;
      attr' ~extra:[NotNull; Unique] ~meta:["module", "Module5"] "b" Text;
      attr' ~extra:[NotNull] ~meta:["module", "Module6"] "c" 
        (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["status_a"; "status_b"; "status_c"]); is_closed = true }));
      attr' ~extra:[] ~meta:["module", "Module3"] "d" (Decimal { precision = Some 10; scale = Some 2; });
      attr' "e" Int;
    ] 
    (schema_to_attrs stmt.schema);

  assert_params_with_meta stmt [
    (named "param_1" Datetime, []);
    (named "param_2" (Decimal { precision = Some 10; scale = Some 2; }), ["module", "Module3"]);
    (named "param_3" (Decimal { precision = Some 10; scale = Some 2; }), ["module", "Module3"]);
    (named "param_4" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["status_a"; "status_b"; "status_c"]); is_closed = true })), 
      ["module", "Module6"]);
    (named "param_5" Int, []);
  ]

let test_meta_insert_update _ = 
  do_test {| 
    CREATE TABLE test43 (
      -- [sqlgg] module=Test43Id
      id INT PRIMARY KEY,
      -- [sqlgg] module=ImportantTxt
      txt TEXT NOT NULL,
      -- [sqlgg] module=Test43Status
      status ENUM('active', 'inactive') NOT NULL DEFAULT 'inactive'
    )
  |} [] [];

  let stmt = parse {|
    INSERT INTO test43 (id, txt, status) 
    VALUES (@param_1, @param_2, @param_3)
  |} in

  assert_params_with_meta stmt [
    (named "param_1" Int, ["module", "Test43Id"]);
    (named "param_2" Text, ["module", "ImportantTxt"]);
    (named "param_3" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "inactive"]); is_closed = true })), ["module", "Test43Status"]);
  ];

  let stmt = parse {|
    UPDATE test43 
    SET txt = @param_1, status = @param_2 
    WHERE id = @param_3
  |} in

  assert_params_with_meta stmt [
    (named "param_1" Text, ["module", "ImportantTxt"]);
    (named "param_2" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "inactive"]); is_closed = true })), ["module", "Test43Status"]);
    (named "param_3" Int, ["module", "Test43Id"]);
  ];

  let stmt = parse {|
    INSERT INTO test43 (id, txt, status)
    SELECT id, txt, status
    FROM test43 
    WHERE id = @param_1
  |} in

  assert_params_with_meta stmt [
    (named "param_1" Int, ["module", "Test43Id"]);
  ];

  do_test {| 
    CREATE TABLE test44 (
      -- [sqlgg] module=Module1
      col_1 INT PRIMARY KEY,
      -- [sqlgg] module=Module2
      col_2 VARCHAR(255) NOT NULL UNIQUE,
      -- [sqlgg] module=Module3
      col_3 ENUM('admin', 'user', 'moderator') NOT NULL DEFAULT 'user',
      -- [sqlgg] module=Module4
      col_4 DECIMAL(10,2) DEFAULT 0.0,
      col_5 TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  |} [] [];

  do_test {| 
    CREATE TABLE test45 (
      -- [sqlgg] module=Module1
      col_1 INT NOT NULL,
      -- [sqlgg] module=Module5
      col_2 ENUM('read', 'write', 'delete') NOT NULL,
      -- [sqlgg] module=Module4
      col_3 DECIMAL(10,2) NOT NULL DEFAULT 0.0,
      FOREIGN KEY (col_1) REFERENCES test44(col_1)
    )
  |} [] [];

  let stmt = parse {|
    INSERT INTO test44 (col_1, col_2, col_3, col_4, col_5) 
    VALUES (@param1, @param2, @param3, @param4, @param5)
  |} in
  assert_params_with_meta stmt [
    (named "param1" Int, ["module", "Module1"]);
    (named "param2" Text, ["module", "Module2"]);
    (named "param3" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named_nullable "param4" (Decimal { precision = Some 10; scale = Some 2; }), ["module", "Module4"]);
    (named_nullable "param5" Datetime, []);
  ];

  let stmt = parse {|
    UPDATE test44 t44
    JOIN test45 t45 ON t44.col_1 = t45.col_1
    SET t44.col_4 = t44.col_4 + t45.col_3 + @param1,
        t44.col_3 = @param2,
        t45.col_2 = @param3
    WHERE t44.col_1 = @param4
  |} in
  assert_params_with_meta stmt [
    (named_nullable "param1" (Decimal { precision = Some 10; scale = Some 2; }), []);
    (named "param2" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named "param3" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["read"; "write"; "delete"]); is_closed = true })), 
      ["module", "Module5"]);
    (named "param4" Int, ["module", "Module1"]);
  ];

  let stmt = parse {|
    INSERT INTO test44 (col_1, col_2, col_3) VALUES
    (@param1, @param2, @param3),
    (@param4, @param5, @param6),
    (@param7, @param8, @param9)
  |} in
  assert_params_with_meta stmt [
    (named "param1" Int, ["module", "Module1"]);
    (named "param2" Text, ["module", "Module2"]);
    (named "param3" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named "param4" Int, ["module", "Module1"]);
    (named "param5" Text, ["module", "Module2"]);
    (named "param6" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named "param7" Int, ["module", "Module1"]);
    (named "param8" Text, ["module", "Module2"]);
    (named "param9" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
  ];

  let stmt = parse {|
    INSERT INTO test44 (col_1, col_2, col_3, col_4)
    VALUES (@param1, @param2, @param3, @param4)
    ON DUPLICATE KEY UPDATE
      col_2 = @param5,
      col_3 = @param6,
      col_4 = col_4 + @param7
  |} in
  assert_params_with_meta stmt [
    (named "param1" Int, ["module", "Module1"]);
    (named "param2" Text, ["module", "Module2"]);
    (named "param3" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named_nullable "param4" (Decimal { precision = Some 10; scale = Some 2; }), ["module", "Module4"]);
    (named "param5" Text, ["module", "Module2"]);
    (named "param6" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["admin"; "user"; "moderator"]); is_closed = true })), 
      ["module", "Module3"]);
    (named_nullable "param7" (Decimal { precision = Some 10; scale = Some 2; }), []);
  ];

  let stmt = parse {|
    UPDATE test44 
    SET col_4 = (
      SELECT MAX(col_3) 
      FROM test45 
      WHERE col_1 = @param1
    )
    WHERE col_1 = @param2 AND col_2 = @param3
  |} in
  assert_params_with_meta stmt [
    (named "param1" Int, ["module", "Module1"]);
    (named "param2" Int, ["module", "Module1"]);
    (named "param3" Text, ["module", "Module2"]);
  ];

  let stmt = parse {|
    UPDATE test44 
    SET col_4 = col_4 + @param1,
        col_2 = CONCAT(@param2, col_2)
    WHERE col_1 = @param3
  |} in
  assert_params_with_meta stmt [
    (named_nullable "param1" (Decimal { precision = Some 10; scale = Some 2; }), []);  (* no meta from col_4 *)
    (named "param2" Text, []);  (* no meta from col_2 *)
    (named "param3" Int, ["module", "Module1"]);
  ]

let test_meta_loss_query =
  let open_enum_t = Type.(Union { ctors = (Enum_kind.Ctors.of_list ["one"; "two"; "three"]); is_closed = false }) in
  let closed_enum_t = Type.(Union { ctors = (Enum_kind.Ctors.of_list ["one"; "two"; "three"]); is_closed = true }) in
  [
  tt {|
    CREATE TABLE test51 (
      id INT PRIMARY KEY,
      parent_id INT NOT NULL,
      -- [sqlgg] module=T51Level
      col_a ENUM('one', 'two', 'three') NOT NULL,
      -- [sqlgg] module=T51Time
      col_b DATETIME NOT NULL
    )
  |} [] [];

  tt {|
    WITH t1 AS (
      SELECT
        IFNULL(LAG(col_a) OVER (PARTITION BY parent_id ORDER BY col_b, id), 'one') AS prev_a,
        col_a AS cur_a
      FROM test51
    )
    SELECT prev_a, cur_a FROM t1
  |} [
    attr' ~meta:["module", "T51Level"] "prev_a" closed_enum_t;
    attr' ~extra:[NotNull] ~meta:["module", "T51Level"] "cur_a" closed_enum_t;
  ] [];

  tt {|
    WITH RECURSIVE d AS (
      SELECT DATE(IFNULL(MIN(col_b), NOW())) AS date_ FROM test51
      UNION ALL
      SELECT DATE_ADD(date_, INTERVAL 1 DAY) FROM d WHERE date_ < DATE(NOW())
    ),
    t1 AS (
      SELECT
        IFNULL(LAG(col_a) OVER (PARTITION BY parent_id ORDER BY col_b, id), 'one') AS prev_a,
        col_a AS cur_a
      FROM test51
    ),
    t2 AS (
      SELECT prev_a, cur_a, COUNT(1) AS cnt FROM t1 GROUP BY prev_a, cur_a
    ),
    dim AS (SELECT 'one' AS lvl UNION ALL SELECT 'two' UNION ALL SELECT 'three')
    SELECT
      d.date_,
      d1.lvl AS from_a,
      d2.lvl AS to_a,
      IFNULL(t2.cnt, 0) AS cnt
    FROM d
    CROSS JOIN dim d1
    CROSS JOIN dim d2
    LEFT JOIN t2 ON d1.lvl = t2.prev_a AND d2.lvl = t2.cur_a
    WHERE d1.lvl != d2.lvl
  |} [
    attr' "date_" Datetime;
    attr' ~meta:["module", "T51Level"] "from_a" open_enum_t;
    attr' ~meta:["module", "T51Level"] "to_a" open_enum_t;
    attr' "cnt" Int;
  ] [];

  tt {|
    WITH
      t1 AS (SELECT col_a AS cur_a FROM test51),
      dim AS (SELECT 'one' AS lvl UNION ALL SELECT 'two' UNION ALL SELECT 'three')
    SELECT dim.lvl AS lvl
    FROM dim
    LEFT JOIN t1 ON dim.lvl = t1.cur_a OR t1.cur_a = 'one'
  |} [
    attr' "lvl" open_enum_t;
  ] [];
]

let semilattice_laws ~show ~eq ~op ?unit ?absorbing elements =
  let each f = List.iter f elements in
  let pairs f = each (fun a -> each (fun b -> f a b)) in
  let triples f = each (fun a -> each (fun b -> each (fun c -> f a b c))) in
  let why l = String.concat " " (List.map show l) in
  let law name f = name >:: (fun () -> f ()) in
  [
    law "commutative" (fun () -> pairs (fun a b -> assert_bool (why [a;b]) (eq (op a b) (op b a))));
    law "associative" (fun () -> triples (fun a b c ->
      assert_bool (why [a;b;c]) (eq (op (op a b) c) (op a (op b c)))));
    law "idempotent" (fun () -> each (fun a -> assert_bool (why [a]) (eq (op a a) a)));
    law "absorbs its own result" (fun () -> pairs (fun a b ->
      let m = op a b in assert_bool (why [a;b]) (eq (op m a) m && eq (op m b) m)));
  ]
  @ Option.map_default (fun u ->
      [ law "unit" (fun () -> each (fun a -> assert_bool (why [a]) (eq (op u a) a))) ]) [] unit
  @ Option.map_default (fun z ->
      [ law "absorbing" (fun () -> each (fun a -> assert_bool (why [a]) (eq (op z a) z))) ]) [] absorbing

let test_meta_lattice =
  let metas =
    let singles = List.concat_map (fun k -> List.map (fun v -> [k, v]) ["a"; "b"]) ["k1"; "k2"] in
    let pairs = List.concat_map (fun a -> List.filter_map (fun b ->
      if String.equal (fst (List.hd a)) (fst (List.hd b)) then None else Some (a @ b)) singles) singles in
    List.map Meta.of_list ([] :: singles @ pairs)
  in
  let elements = None :: List.map (fun m -> Some m) metas in
  let eq a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> Meta.equal a b
    | None, Some _ | Some _, None -> false
  in
  let show = function None -> "top" | Some m -> Format.asprintf "%a" Meta.pp m in
  semilattice_laws ~show ~eq ~op:Meta.common ~unit:None ~absorbing:(Some (Meta.empty ())) elements
  @ [
  "merge_right is associative" >:: (fun () -> List.iter (fun a -> List.iter (fun b -> List.iter (fun c ->
    assert_bool (sprintf "%s %s %s" (show (Some a)) (show (Some b)) (show (Some c)))
      (Meta.equal (Meta.merge_right (Meta.merge_right a b) c) (Meta.merge_right a (Meta.merge_right b c))))
      metas) metas) metas);

  "merge_right keeps the right side" >:: (fun () -> List.iter (fun a -> List.iter (fun b ->
    assert_bool (sprintf "%s %s" (show (Some a)) (show (Some b)))
      (Meta.equal (Meta.merge_right a (Meta.merge_right a b)) (Meta.merge_right a b)))
      metas) metas);

  "common_all folds common" >:: (fun () -> List.iter (fun a -> List.iter (fun b -> List.iter (fun c ->
    assert_bool (sprintf "%s %s %s" (show a) (show b) (show c))
      (eq (Meta.common_all [a; b; c]) (Meta.common a (Meta.common b c))))
      elements) elements) elements);

  "silence is not disagreement" >:: (fun () ->
    List.iter (fun a -> List.iter (fun b ->
      assert_bool "shared ignores empty"
        (Meta.equal (Meta.shared [Meta.empty (); a; b]) (Meta.shared [a; b]))) metas) metas);
]


let test_meta_equality =
  [
  tt {|
    CREATE TABLE branded (
      -- [sqlgg] module=Cid
      cid BIGINT NOT NULL,
      -- [sqlgg] module=Cid
      also_cid BIGINT NOT NULL,
      -- [sqlgg] module=Money
      amount DECIMAL(10,2) NOT NULL,
      -- [sqlgg] module=Cid
      -- [sqlgg] non_nullifiable=true
      guarded BIGINT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE elsewhere (
      -- [sqlgg] module=Other
      other BIGINT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE plain (
      cid BIGINT NOT NULL,
      amount BIGINT NOT NULL,
      n BIGINT NOT NULL
    )
  |} [] [];

  tt "SELECT plain.cid FROM plain JOIN branded ON branded.cid = plain.cid"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain, branded WHERE branded.cid = plain.cid"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain, branded WHERE branded.cid = plain.cid AND plain.n > 0"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain LEFT JOIN branded ON plain.n = branded.cid WHERE branded.cid = plain.cid"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain JOIN branded USING (cid)"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain NATURAL JOIN branded"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain, branded WHERE branded.cid = plain.cid OR plain.n = 0"
    [attr' ~extra:[NotNull] "cid" Int] [];

  tt "SELECT plain.cid FROM plain JOIN branded ON branded.cid = plain.cid OR plain.cid = 0"
    [attr' ~extra:[NotNull] "cid" Int] [];

  tt "SELECT plain.amount FROM plain JOIN branded ON branded.amount = plain.amount"
    [attr' ~extra:[NotNull] "amount" Int] [];

  tt "SELECT plain.cid FROM branded LEFT JOIN plain ON branded.cid = plain.cid"
    [attr' ~extra:[NotNull] ~nullability:Nullable ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.cid FROM plain LEFT JOIN branded ON branded.cid = plain.cid"
    [attr' ~extra:[NotNull] "cid" Int] [];

  tt "SELECT plain.cid FROM plain LEFT JOIN branded USING (cid)"
    [attr' ~extra:[NotNull] "cid" Int] [];

  tt "SELECT plain.cid FROM plain JOIN branded ON branded.guarded = plain.cid"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  tt "SELECT plain.* FROM plain JOIN branded ON branded.cid = plain.cid"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int;
     attr' ~extra:[NotNull] "amount" Int;
     attr' ~extra:[NotNull] "n" Int] [];

  tt "SELECT p.cid FROM branded p LEFT JOIN branded q ON p.cid = q.amount"
    [attr' ~extra:[NotNull] ~meta:["module", "Cid"] "cid" Int] [];

  check "each param follows the column it stands next to"
    "SELECT plain.n FROM plain JOIN branded ON branded.cid = plain.cid WHERE branded.amount = @money AND plain.n = @loose"
    [ (named "money" (Decimal { precision = Some 10; scale = Some 2 }), ["module", "Money"]);
      (named "loose" Int, []) ];

  check "a param reaches the domain through the joined column"
    "SELECT plain.n FROM plain JOIN branded ON branded.cid = plain.cid WHERE plain.cid = @p"
    [ (named "p" Int, ["module", "Cid"]) ];

  check "a param reaches it through an equality in where"
    "SELECT plain.n FROM plain, branded WHERE branded.cid = plain.cid AND plain.cid = @p"
    [ (named "p" Int, ["module", "Cid"]) ];

  check "an outer join keeps the param silent"
    "SELECT plain.n FROM plain LEFT JOIN branded ON branded.cid = plain.cid WHERE plain.cid = @p"
    [ (named "p" Int, []) ];

  check "each occurrence of one name is annotated where it stands"
    "SELECT branded.cid FROM branded, elsewhere WHERE branded.cid = @p AND elsewhere.other = @p"
    [ (named "p" Int, ["module", "Cid"]); (named "p" Int, ["module", "Other"]) ];

  check "one domain reached twice stays itself"
    "SELECT branded.cid FROM branded WHERE branded.cid = @p AND branded.also_cid = @p"
    [ (named "p" Int, ["module", "Cid"]); (named "p" Int, ["module", "Cid"]) ];

  check "an occurrence next to an unbranded column stays silent"
    "SELECT plain.n FROM plain, branded WHERE branded.cid = @p AND plain.cid = @p"
    [ (named "p" Int, ["module", "Cid"]); (named "p" Int, []) ];

  check "a cast is about the type and leaves the domain alone"
    "SELECT branded.cid FROM branded WHERE branded.cid = @p :: Int"
    [ (named "p" Int, ["module", "Cid"]) ];
]

let test_meta_functions =
  [
  tt {|
    CREATE TABLE rows_ (
      id BIGINT NOT NULL,
      -- [sqlgg] module=Cid
      cid BIGINT NOT NULL,
      -- [sqlgg] module=Status
      status TEXT NOT NULL,
      plain TEXT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE unbranded (
      ref BIGINT NOT NULL,
      cid BIGINT NOT NULL
    )
  |} [] [];

  tt "SELECT COALESCE(rows_.cid, unbranded.cid) AS c FROM rows_ JOIN unbranded ON rows_.id = unbranded.ref"
    [attr' ~meta:["module", "Cid"] "c" Int] [];

  tt "SELECT COALESCE(unbranded.cid, rows_.cid) AS c FROM rows_ JOIN unbranded ON rows_.id = unbranded.ref"
    [attr' ~meta:["module", "Cid"] "c" Int] [];

  tt "SELECT GREATEST(cid, 0) AS c FROM rows_" [attr' ~meta:["module", "Cid"] "c" Int] [];

  tt "SELECT LEAST(0, cid) AS c FROM rows_" [attr' ~meta:["module", "Cid"] "c" Int] [];

  tt "SELECT NULLIF(status, 'active') AS s FROM rows_"
    [attr' ~nullability:Nullable ~meta:["module", "Status"] "s" (Type.StringLiteral "active")] [];

  tt "SELECT CONCAT(status, plain) AS s FROM rows_" [attr' "s" Text] [];

  tt "SELECT GROUP_CONCAT(plain ORDER BY status) AS s FROM rows_"
    [attr' ~nullability:Nullable "s" Text] [];

  tt "SELECT (WITH c AS (SELECT cid FROM rows_) SELECT cid FROM c LIMIT 1) AS c FROM rows_"
    [attr' ~nullability:Nullable ~meta:["module", "Cid"] "c" Int] [];

  check "both ends of a comparison through a null handling call"
    "SELECT id FROM rows_ WHERE IFNULL(@a, status) = @b"
    [ (named "a" Text, ["module", "Status"]); (named "b" Text, ["module", "Status"]) ];

  check "params nested in coalesce chains"
    "SELECT COALESCE(COALESCE(cid, @a), @b) AS c FROM rows_"
    [ (named "a" Int, ["module", "Cid"]); (named "b" Int, ["module", "Cid"]) ];

  check "nullif shares one variable"
    "SELECT NULLIF(status, @p) AS s FROM rows_"
    [ (named "p" Text, ["module", "Status"]) ];

  check "a variadic function shares one variable"
    "SELECT id FROM rows_ WHERE cid = LEAST(@p, 0)"
    [ (named "p" Int, ["module", "Cid"]) ];

  check "a transforming function breaks the chain"
    "SELECT id FROM rows_ WHERE LOWER(status) = @p"
    [ (named "p" Text, []) ];

  check "arithmetic breaks the chain"
    "SELECT id FROM rows_ WHERE cid = @p + 1"
    [ (named "p" Int, []) ];

  check "an unbranded column leaves the param silent"
    "SELECT id FROM rows_ WHERE plain = @p"
    [ (named "p" Text, []) ];

  tt "SELECT COALESCE(status, LOWER(status), @p) AS c FROM rows_" [attr' "c" Text]
    [ named "p" Text ];

  tt "SELECT MAX(cid) AS c FROM rows_"
    [attr' ~nullability:Nullable ~meta:["module", "Cid"] "c" Int] [];

  tt "SELECT SUM(cid) AS c FROM rows_"
    [attr' ~nullability:Nullable ~meta:["module", "Cid"] "c" Int] [];

  check "a param in a case branch reaches the domain of its sibling branch"
    "SELECT CASE WHEN id = 1 THEN status ELSE @p END AS s FROM rows_"
    [ (named "p" Text, ["module", "Status"]) ];

  check "an opaque sibling withdraws the domain from the whole class"
    "SELECT COALESCE(status, LOWER(status), @p) AS c FROM rows_"
    [ (named "p" Text, []) ];

  check "a param inside a function in an assignment"
    "UPDATE rows_ SET status = IFNULL(@status, status) WHERE id = @id"
    [ (named "status" Text, ["module", "Status"]); (named "id" Int, []) ];

  check "both ends of a between"
    "SELECT id FROM rows_ WHERE status BETWEEN @low AND @high"
    [ (named "low" Text, ["module", "Status"]); (named "high" Text, ["module", "Status"]) ];

  tt "SELECT CASE WHEN id = 1 THEN status ELSE status END AS s FROM rows_"
    [attr' ~meta:["module", "Status"] "s" Text] [];
]

let test_meta_foreign_key =
  [
  tt {|
    CREATE TABLE owners (
      -- [sqlgg] module=Owner_id
      id BIGINT NOT NULL PRIMARY KEY,
      plain BIGINT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE owned (
      owner_ref BIGINT NOT NULL,
      plain_ref BIGINT NOT NULL,
      loose BIGINT NOT NULL,
      FOREIGN KEY (owner_ref) REFERENCES owners(id),
      FOREIGN KEY (plain_ref) REFERENCES owners(plain)
    )
  |} [] [];

  tt "SELECT owner_ref FROM owned"
    [attr' ~extra:[NotNull] ~meta:["module", "Owner_id"] "owner_ref" Int] [];

  tt {|
    CREATE TABLE owned_annotated (
      -- [sqlgg] module=Its_own
      declared_ref BIGINT NOT NULL,
      -- [sqlgg] non_nullifiable=true
      partial_ref BIGINT NOT NULL,
      FOREIGN KEY (declared_ref) REFERENCES owners(id),
      FOREIGN KEY (partial_ref) REFERENCES owners(id)
    )
  |} [] [];

  tt "SELECT declared_ref, partial_ref FROM owned_annotated"
    [attr' ~extra:[NotNull] ~meta:["module", "Its_own"] "declared_ref" Int;
     attr' ~extra:[NotNull] ~meta:["module", "Owner_id"; "non_nullifiable", "true"] "partial_ref" Int] [];

  tt "SELECT loose FROM owned" [attr' ~extra:[NotNull] "loose" Int] [];

  tt "SELECT plain_ref FROM owned" [attr' ~extra:[NotNull] "plain_ref" Int] [];

  tt "SELECT owned.owner_ref FROM owned LEFT JOIN owners ON owners.id = owned.owner_ref"
    [attr' ~extra:[NotNull] ~meta:["module", "Owner_id"] "owner_ref" Int] [];

  check "param against a declared foreign key"
    "SELECT loose FROM owned WHERE owner_ref = @p"
    [ (named "p" Int, ["module", "Owner_id"]) ];

  tt {|
    CREATE TABLE other_owners (
      -- [sqlgg] module=Other_id
      id BIGINT NOT NULL PRIMARY KEY,
      -- [sqlgg] module=Owner_id
      agreeing BIGINT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE two_ways (
      contested BIGINT NOT NULL,
      agreed BIGINT NOT NULL,
      FOREIGN KEY (contested) REFERENCES owners(id),
      FOREIGN KEY (contested) REFERENCES other_owners(id),
      FOREIGN KEY (agreed) REFERENCES owners(id),
      FOREIGN KEY (agreed) REFERENCES other_owners(agreeing)
    )
  |} [] [];

  tt "SELECT contested, agreed FROM two_ways"
    [attr' ~extra:[NotNull] "contested" Int;
     attr' ~extra:[NotNull] ~meta:["module", "Owner_id"] "agreed" Int] [];
]

let test_meta_union_null_placeholder = [
  tt {|
    CREATE TABLE left_rows (
      -- [sqlgg] module=Left_id
      id BIGINT NOT NULL,
      -- [sqlgg] module=Owner_id
      owner_id BIGINT NULL,
      -- [sqlgg] module=Payload
      payload JSON NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE right_rows (
      -- [sqlgg] module=Right_id
      id BIGINT NOT NULL
    )
  |} [] [];

  tt {|
    SELECT l.id AS left_id, l.owner_id, l.payload, NULL AS right_id FROM left_rows l
    UNION ALL
    SELECT NULL AS left_id, NULL AS owner_id, NULL AS payload, r.id AS right_id FROM right_rows r
  |} [
    attr' ~nullability:Nullable ~extra:[NotNull] ~meta:["module", "Left_id"] "left_id" Int;
    attr' ~nullability:Nullable ~extra:[Null] ~meta:["module", "Owner_id"] "owner_id" Int;
    attr' ~nullability:Nullable ~extra:[Null] ~meta:["module", "Payload"] "payload" Json;
    attr' ~nullability:Nullable ~meta:["module", "Right_id"] "right_id" Int;
  ] [];

  tt "SELECT id AS x FROM left_rows UNION ALL SELECT id AS x FROM right_rows"
    [ attr' ~extra:[NotNull] "x" Int ] [];

  tt "SELECT CASE WHEN id = 1 THEN owner_id ELSE NULL END AS s FROM left_rows"
    [ attr' ~nullability:Nullable ~meta:["module", "Owner_id"] "s" Int ] [];
]

let test_meta_union_enum_literal =
  let status_t = Type.(Union { ctors = (Enum_kind.Ctors.of_list ["draft"; "published"; "failed"]); is_closed = true }) in
  [
  tt {|
    CREATE TABLE rows_with_status (
      -- [sqlgg] module=Row_status
      status ENUM('draft', 'published', 'failed') NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE rows_text (
      label TEXT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE rows_status_a (
      -- [sqlgg] module=Status_a
      status ENUM('draft', 'published') NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE rows_status_b (
      -- [sqlgg] module=Status_b
      status ENUM('draft', 'published') NOT NULL
    )
  |} [] [];

  tt {|
    SELECT status AS row_status FROM rows_with_status
    UNION ALL
    SELECT 'published' AS row_status
  |} [ attr' ~extra:[NotNull] ~meta:["module", "Row_status"] "row_status" status_t ] [];

  tt {|
    SELECT 'published' AS row_status
    UNION ALL
    SELECT status AS row_status FROM rows_with_status
  |} [ attr' ~meta:["module", "Row_status"] "row_status" status_t ] [];

  tt {|
    SELECT status AS x FROM rows_with_status
    UNION ALL
    SELECT label AS x FROM rows_text
  |} [ attr' ~extra:[NotNull] ~meta:["module", "Row_status"] "x" Type.Text ] [];

  tt {|
    SELECT label AS x FROM rows_text
    UNION ALL
    SELECT status AS x FROM rows_with_status
  |} [ attr' ~extra:[NotNull] ~meta:["module", "Row_status"] "x" Type.Text ] [];

  tt {|
    SELECT status FROM rows_status_a
    UNION ALL
    SELECT status FROM rows_status_b
  |} [ attr' ~extra:[NotNull] "status" Type.(Union { ctors = (Enum_kind.Ctors.of_list ["draft"; "published"]); is_closed = true }) ] [];
]

let test_operand_order =
  let narrow_t = Type.(Union { ctors = Enum_kind.Ctors.of_list ["draft"; "published"]; is_closed = true }) in
  let price_t = Type.(Decimal { precision = Some 10; scale = Some 2 }) in
  [
  tt "CREATE TABLE narrow (s ENUM('draft','published') NOT NULL)" [] [];
  tt "CREATE TABLE wide (s ENUM('draft','published','failed') NOT NULL)" [] [];
  tt "CREATE TABLE txt (label TEXT NOT NULL)" [] [];
  tt "CREATE TABLE prices (price DECIMAL(10,2) NOT NULL)" [] [];

  (* TODO asymmetric on purpose: PostgreSQL enums are nominal and this join is an error there.
     See the TODO in order_kind. *)
  wrong "SELECT narrow.s AS x FROM narrow JOIN wide ON narrow.s = wide.s";
  tt "SELECT narrow.s AS x FROM narrow JOIN wide ON wide.s = narrow.s" [ attr' ~extra:[NotNull] "x" narrow_t ] [];

  tt "SELECT COALESCE(narrow.s, txt.label) AS x FROM narrow, txt" [ attr' "x" narrow_t ] [];
  tt "SELECT COALESCE(txt.label, narrow.s) AS x FROM narrow, txt" [ attr' "x" narrow_t ] [];

  tt "SELECT COALESCE(narrow.s, 'draft') AS x FROM narrow" [ attr' "x" narrow_t ] [];
  tt "SELECT COALESCE('draft', narrow.s) AS x FROM narrow" [ attr' "x" narrow_t ] [];

  tt "SELECT COALESCE(price, 1.5) AS x FROM prices" [ attr' "x" price_t ] [];
  tt "SELECT COALESCE(1.5, price) AS x FROM prices" [ attr' "x" price_t ] [];

  wrong "SELECT 1 AS x FROM narrow WHERE COALESCE(narrow.s, CASE WHEN 1=1 THEN 'draft' ELSE 'published' END) = 'failed'";
  wrong "SELECT 1 AS x FROM narrow WHERE COALESCE(CASE WHEN 1=1 THEN 'draft' ELSE 'published' END, narrow.s) = 'failed'";
]

let test_multi_functions = [
  tt "CREATE TABLE test_multi (id INT, txt1 TEXT, txt2 TEXT NULL, txt3 TEXT NOT NULL)" [] [];
  
  tt "SELECT CONCAT(txt1, txt2) as result FROM test_multi" 
    [attr' ~nullability:Nullable "result" Text] [];
    
  tt "SELECT CONCAT('hello', 'world') as result" 
    [attr' "result" Text] [];
    
  tt "SELECT CONCAT('hello', txt2) as result FROM test_multi" 
    [attr' ~nullability:Nullable "result" Text] [];
    
  tt "SELECT CONCAT(txt1, @param) as result FROM test_multi" 
    [attr' ~nullability:Nullable "result" Text] 
    [named "param" Text];

  tt "SELECT CONCAT(txt3, @param) as result FROM test_multi" 
    [attr' "result" Text]
    [named "param" Text];
    
  tt "SELECT STRFTIME('%Y-%m-%d', txt1) as result FROM test_multi"
    [attr' ~nullability:Nullable "result" Text] [];
    
  tt "SELECT CONCAT_WS(',', txt1, txt2, 'static') as result FROM test_multi"
    [attr' ~nullability:Nullable "result" Text] [];
    
  tt "SELECT CONCAT(txt1, CONCAT_WS('-', txt2, 'suffix')) as result FROM test_multi"
    [attr' ~nullability:Nullable "result" Text] [];
    
  tt "SELECT id FROM test_multi WHERE CONCAT(txt1, txt2) = @search"
    [attr' ~nullability:Nullable "id" Int]
    [named "search" Text];
    
  tt "SELECT CONCAT('prefix:', (SELECT txt1 FROM test_multi LIMIT 1)) as result"
    [attr' ~nullability:Nullable "result" Text] [];
]


let test_on_conflict_do_update = [
  tt {|
    CREATE TABLE table_20250801 (
      col_1 INTEGER PRIMARY KEY,
      col_2 TEXT NOT NULL,
      col_3 INTEGER
    )
  |} [] [];
  tt {|
    INSERT INTO table_20250801 (col_1, col_2, col_3)
    VALUES (1, 'value_1', 30)
    ON CONFLICT(col_1) DO UPDATE SET
      col_2 = excluded.col_2,
      col_3 = col_3
  |} [] [];
  (* Schema Error: ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint column *)
  wrong {|
    INSERT INTO table_20250801 (col_1, col_2, col_3)
    VALUES (1, 'value_1', 30)
    ON CONFLICT(col_3) DO UPDATE SET
      col_2 = excluded.col_2,
      col_3 = col_3
  |};
  (* Schema Error: ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint column *)
  wrong {|
    INSERT INTO table_20250801 (col_1, col_2, col_3)
    VALUES (1, 'value_1', 30)
    ON CONFLICT(col_1, col_3) DO UPDATE SET
      col_2 = excluded.col_2,
      col_3 = col_3
  |};
]

let test_enum_with_in_and_between = [
  tt {|
    CREATE TABLE table_20250807 (
      col_0 INT PRIMARY KEY AUTO_INCREMENT,
      col_1 ENUM('todo', 'in_progress', 'review', 'done', 'cancelled') NOT NULL,
      col_2 ENUM('low', 'medium', 'high') DEFAULT 'medium'
    )
  |} [][];
  tt {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 IN ('todo', 'in_progress', 'review')
  |} [ attr' ~extra:[PrimaryKey;Autoincrement] ~meta:[] "col_0" Int;][];
  wrong {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 IN ('todo', 'in_progress', 'review', 'non_existent')
  |};
  wrong {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 IN ('todo1', 'in_progress', 'review')
  |};
  tt {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 BETWEEN 'todo' AND 'review'
  |} [ attr' ~extra:[PrimaryKey;Autoincrement] ~meta:[] "col_0" Int;][];
  wrong {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 BETWEEN 'todo' AND 'non_existent'
  |};
  tt {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 >= 'todo' AND col_1 <= 'review'
  |} [ attr' ~extra:[PrimaryKey;Autoincrement] ~meta:[] "col_0" Int;][];
  wrong {|
    SELECT col_0 FROM table_20250807
    WHERE col_1 >= 'todo' AND col_1 <= 'non_existent'
  |};
  wrong {|
    SELECT col_1 IN ('todo', 'in_progress', 'review', 'nonono')
    FROM table_20250807
  |};
  tt {|
    SELECT col_1 IN ('todo', 'in_progress', 'review') AS is_valid
    FROM table_20250807
  |} [attr' "is_valid" Bool;][];
  tt {|
    SELECT col_1 IN ('todo', 'in_progress', 'review') AS is_valid
    FROM table_20250807
    WHERE col_1 IN ('todo', 'in_progress', 'review')
  |} [attr' "is_valid" Bool;][];
  tt {|
    SELECT IF (col_1 IN ('todo', 'in_progress', 'review'), 'valid', 'invalid') AS status
    FROM table_20250807
  |} [attr' "status" (Type.(Union { ctors = 
    (Enum_kind.Ctors.of_list ["valid"; "invalid"]); is_closed = false }));
  ][]
]

let test_datefns = [
  tt "SELECT DAY(CURRENT_DATE) AS day_" [attr' ~nullability:Strict "day_" Int] [];
  tt "SELECT EXTRACT(DAY FROM CURRENT_DATE) AS day_extract" [attr' ~nullability:Strict "day_extract" Int] [];
  tt "SELECT CURRENT_TIMESTAMP() + INTERVAL 1 DAY AS ts_plus_interval" [attr' ~nullability:Strict "ts_plus_interval" Datetime] [];
  tt "SELECT DATE_SUB(CURRENT_DATE, INTERVAL 5 WEEK) AS date_sub_weeks" [attr' ~nullability:Strict "date_sub_weeks" Datetime] [];
  tt "SELECT TIMESTAMPDIFF(MONTH,'2003-02-01','2003-05-01') AS tsdiff1" [attr' ~nullability:Strict "tsdiff1" Int] [];
  tt "SELECT TIMESTAMPDIFF(MONTH, '2019-11-12', CURRENT_TIMESTAMP()) AS tsdiff2" [attr' ~nullability:Strict "tsdiff2" Int] [];
  tt "SELECT CURRENT_DATE + INTERVAL 3 MONTH AS date_plus_months" [attr' ~nullability:Strict "date_plus_months" Datetime] [];
  tt "SELECT DATE_ADD(CURRENT_DATE, INTERVAL 2 DAY) AS date_add_days" [attr' ~nullability:Strict "date_add_days" Datetime] [];
  tt "SELECT LAST_DAY(CURRENT_DATE) AS last_day" [attr' ~nullability:Strict "last_day" Datetime] [];
  tt "SELECT YEAR(CURRENT_DATE) AS year_, MONTH(CURRENT_DATE) AS month_, WEEK(CURRENT_DATE) AS week_, QUARTER(CURRENT_DATE) AS quarter_"
    [ attr' ~nullability:Strict "year_" Int; attr' ~nullability:Strict "month_" Int; attr' ~nullability:Strict "week_" Int; attr' ~nullability:Strict "quarter_" Int ] [];
  tt "SELECT TIMESTAMPDIFF(MONTH, '2002-05-01', @dt) AS tsdiff_param" [attr' ~nullability:Strict "tsdiff_param" Int] [named "dt" Datetime];
  tt "SELECT DATE_ADD(@dt, INTERVAL 7 DAY) AS date_add_param" [attr' ~nullability:Strict "date_add_param" Datetime] [named "dt" Datetime];
  tt "SELECT @dt + INTERVAL @n DAY AS dt_plus_param" [attr' ~nullability:Strict "dt_plus_param" Datetime] [named "dt" Datetime; named "n" Int];

  tt "SELECT TIME(CURRENT_TIMESTAMP()) AS time_" [attr' ~nullability:Strict "time_" Text] [];
  tt "SELECT DATE('2020-01-01 12:34:56') AS date_cast" [attr' ~nullability:Strict "date_cast" Datetime] [];
  tt "SELECT FROM_UNIXTIME(946684800) AS from_unix_dt" [attr' ~nullability:Strict "from_unix_dt" Datetime] [];
  tt "SELECT FROM_UNIXTIME(946684800, '%Y-%m-%d') AS from_unix_str" [attr' ~nullability:Strict "from_unix_str" Text] [];
  tt "SELECT UNIX_TIMESTAMP() AS now_unix" [attr' ~nullability:Strict "now_unix" Int] [];
  tt "SELECT UNIX_TIMESTAMP(CURRENT_TIMESTAMP()) AS ts_unix" [attr' ~nullability:Strict "ts_unix" Int] [];
  tt "SELECT DATE_FORMAT(CURRENT_DATE, '%Y-%m') AS date_fmt" [attr' ~nullability:Strict "date_fmt" Text] [];
  tt "SELECT TIME_FORMAT(CURRENT_TIME, '%H:%i') AS time_fmt" [attr' ~nullability:Strict "time_fmt" Text] [];
  tt "SELECT DAYOFMONTH(CURRENT_DATE) AS dom" [attr' ~nullability:Strict "dom" Int] [];
  tt "SELECT DAYOFWEEK(CURRENT_DATE) AS dow" [attr' ~nullability:Strict "dow" Int] [];
  tt "SELECT DAYOFYEAR(CURRENT_DATE) AS doy" [attr' ~nullability:Strict "doy" Int] [];
  tt "SELECT HOUR(CURRENT_TIMESTAMP()) AS hour_" [attr' ~nullability:Strict "hour_" Int] [];
  tt "SELECT MINUTE(CURRENT_TIMESTAMP()) AS minute_" [attr' ~nullability:Strict "minute_" Int] [];
  tt "SELECT SECOND(CURRENT_TIMESTAMP()) AS second_" [attr' ~nullability:Strict "second_" Int] [];
  tt "SELECT MICROSECOND(CURRENT_TIMESTAMP()) AS microsecond_" [attr' ~nullability:Strict "microsecond_" Int] [];
  tt "SELECT EXTRACT(YEAR FROM CURRENT_DATE) AS year_extracted" [attr' ~nullability:Strict "year_extracted" Int] [];
]

let test_json_and_fixed_then_pairs_fn_kind  = [
  tt "CREATE TABLE test46 ( id INT AUTO_INCREMENT PRIMARY KEY, data JSON)" [][];
  tt "UPDATE test46 SET data = JSON_ARRAY_APPEND(data, '$', '\"new_val\"') WHERE id = 3" [] [];
  tt "UPDATE test46 SET data = JSON_ARRAY_APPEND(data, '$[0][1][2].three.four.five', 'false') WHERE id = 3" [] [];
  tt {| SELECT JSON_ARRAY_APPEND(
       data, 
       '$[0].items',     123,          
       '$[1].props',     '"hello"',       
       '$[2].flags',     true,          
       '$[3].meta',      null,         
       '$[4].nested',    JSON_OBJECT('x', 'y')
     ) as result FROM test46 WHERE id = 3 
    |} [ attr' ~nullability:Nullable "result" Json ] [];
  wrong "UPDATE test46 SET data = JSON_ARRAY_APPEND('NOT_A_VALID_JSON', '$[0][1][2].three.four.five', 'this is a string') WHERE id = 3";
  tt {| UPDATE test46 SET data = JSON_ARRAY_APPEND(data, @path, @data :: Text) WHERE id = 3 |} [] [
    named "path" Json_path;
    named "data" Text;
  ];
  tt "SELECT JSON_REMOVE(@json, '$[1]') as result" [ attr' "result" Json ][ named "json" Json;];
  wrong "SELECT JSON_REMOVE(@json, 'invalid path') as result";
  tt "SELECT JSON_REMOVE(@json, @path) as result" [ attr' "result" Json ][ named "json" Json; named "path" Json_path;];
  
  tt "SELECT JSON_REMOVE(@json, '$.field1', '$.field2', '$.nested.prop') as result" 
    [ attr' "result" Json ] [ named "json" Json ];
  tt "UPDATE test46 SET data = JSON_REMOVE(data, '$.old_field') WHERE id = 1" [] [];
  tt "UPDATE test46 SET data = JSON_SET(data, '$.name', 'John') WHERE id = 1" [] [];
  tt {| UPDATE test46 SET data = JSON_SET(
        data, 
        '$.name',     'Alice',
        '$.age',      25,
        '$.active',   true,
        '$.balance',  null
      ) WHERE id = 2 
    |} [] [];
  tt {| SELECT JSON_SET(
        data,
        '$.user.name',    'Bob',
        '$.user.props',   JSON_OBJECT('theme', 'dark'),
        '$.user.count',   42
      ) as result FROM test46 WHERE id = 1
    |} [ attr' ~nullability:Nullable "result" Json ] [];
  tt {| UPDATE test46 SET data = JSON_SET(data, @path, @value :: Text, '$.timestamp', @time :: Int) WHERE id = 3 |} 
  [] [
    named "path" Json_path;
    named "value" Text;
    named "time" Int;
  ];
  wrong "UPDATE test46 SET data = JSON_SET('INVALID_JSON', '$.field', 'value') WHERE id = 1";
  tt "SELECT JSON_OBJECT() as result" [ attr' "result" Json ] [];
  tt "SELECT JSON_OBJECT('name', 'John') as result" [ attr' "result" Json ] [];
  tt "SELECT JSON_OBJECT('name', 'Alice', 'age', 25, 'active', true) as result" 
    [ attr' "result" Json ] [];
  tt "UPDATE test46 SET data = JSON_OBJECT('user', JSON_OBJECT('id', 1, 'name', 'Bob')) WHERE id = 1" [] [];
  tt "SELECT JSON_OBJECT(@key, @value :: Text) as result" 
    [ attr' "result" Json ] [ named "key" Text; named "value" Text ];
  tt "SELECT JSON_OBJECT('meta', JSON_EXTRACT(data, '$.info')) as result FROM test46" 
    [ attr' "result" Json ] [];
  tt "SELECT JSON_ARRAY() as result" [ attr' "result" Json ] [];
  tt "SELECT JSON_ARRAY(1, 'hello', true, null) as result" [ attr' "result" Json ] [];
  tt "UPDATE test46 SET data = JSON_ARRAY(JSON_OBJECT('id', 1), JSON_OBJECT('id', 2)) WHERE id = 1" [] [];
  tt "SELECT JSON_ARRAY(@val1 :: Int, @val2 :: Text, @val3 :: Bool) as result" 
    [ attr' "result" Json ] [ 
      named "val1" Int; 
      named "val2" Text; 
      named "val3" Bool 
    ];
  tt "SELECT JSON_CONTAINS(@json :: Json, @search) as result" 
    [ attr' ~nullability:Nullable  "result" Bool ] [ named "json" Json; named "search" Json ];
  tt {| SELECT JSON_CONTAINS(data, '"target_value"') as found FROM test46 |}
    [ attr' ~nullability:Nullable "found" Bool ] [];
  wrong "SELECT JSON_CONTAINS(@json, @search :: Int, @path) as result";
  
  tt "SELECT JSON_CONTAINS(data, JSON_OBJECT('key', 'value'), '$.objects') as found FROM test46" 
    [ attr' ~nullability:Nullable "found" Bool ] [];
  wrong "SELECT JSON_CONTAINS('INVALID_JSON', 'search') as result";
  wrong "SELECT JSON_CONTAINS('{\"a\": 2}', 'INVALID') as result";
  (* tt "SELECT JSON_CONTAINS('{\"a\": 2}', NULL) as result" [][]; *)
  tt "SELECT JSON_UNQUOTE(@json_val) as result" 
    [ attr' "result" Text ] [ named "json_val" Json ];
  tt "SELECT JSON_UNQUOTE(JSON_EXTRACT(data, '$.name')) as name FROM test46" 
    [ attr' ~nullability:Nullable "name" Text ] [];
  wrong "SELECT JSON_UNQUOTE('not a json value') as result";
  tt "SELECT JSON_SEARCH(@json, 'one', @pattern) as result" 
    [ attr' ~nullability:Nullable "result" Json ] [ 
      named "json" Json; 
      named "pattern" Text 
    ];
  tt "SELECT JSON_SEARCH(data, 'all', 'search%', '\\\\', '$.users') as paths FROM test46" 
    [ attr' ~nullability:Nullable "paths" Json ] [];
  tt "SELECT JSON_SEARCH(@json, 'one', @pattern, @escape, @path1, @path2) as result" 
    [ attr' ~nullability:Nullable "result" Json ] [ 
      named "json" Json; 
      named "pattern" Text;
      named "escape" Text;
      named "path1" Json_path;
      named "path2" Json_path;
    ];
  tt {| UPDATE test46 SET data = JSON_SET(
        data,
        '$.processed', JSON_ARRAY(
          JSON_OBJECT('id', 1, 'status', 'active'),
          JSON_OBJECT('id', 2, 'status', 'inactive')
        ),
        '$.meta', JSON_OBJECT('version', 2, 'updated', true)
      ) WHERE id = 1 |} [] [];
  tt {| SELECT 
        JSON_UNQUOTE(JSON_EXTRACT(data, '$.name')) as name,
        JSON_CONTAINS(data, '"admin"', '$.roles') as is_admin,
        JSON_SEARCH(data, 'one', 'test%') as test_path
      FROM test46 WHERE id = 1 |} [
        attr' ~nullability:Nullable "name" Text;
        attr' ~nullability:Nullable "is_admin" Bool; 
        attr' ~nullability:Nullable "test_path" Json;
      ] [];

  tt "SELECT JSON_DEPTH(@json) as depth" [ attr' ~nullability:Strict "depth" Int ] [ named "json" Json ];
  tt "SELECT JSON_DEPTH(@json_nullable :: Json Null) as depth_n" [ attr' ~nullability:Nullable "depth_n" Int ] [ named_nullable "json_nullable" Json ];
  
  
  tt "SELECT JSON_REMOVE(@json, '$.field1', '$.field2', '$.nested.prop') as result" 
    [ attr' "result" Json ] [ named "json" Json ];
]

let test_json_arrow_ops = [
  tt "CREATE TABLE table20250814 ( id INT AUTO_INCREMENT PRIMARY KEY, data JSON)" [] [];
  tt "SELECT data -> '$.name' as js_name FROM table20250814" [attr' ~nullability:Nullable "js_name" Json] [];
  tt "SELECT data ->> '$.name' as name FROM table20250814" [attr' ~nullability:Nullable "name" Text] [];
  tt "SELECT data -> @name_path as js_name FROM table20250814" [attr' ~nullability:Nullable "js_name" Json] [named "name_path" Json_path];
  tt "SELECT data ->> @email_path as email FROM table20250814" [attr' ~nullability:Nullable "email" Text] [named "email_path" Json_path];
  tt "SELECT id FROM table20250814 WHERE data ->> '$.address.city' = 'Paris'" [attr' ~extra:[Autoincrement;PrimaryKey] "id" Int] [];
  tt "SELECT data -> '$.user' ->> '$.name' as user_name FROM table20250814" [attr' ~nullability:Nullable "user_name" Text] [];
]

let test_json_additional_functions = [
  tt "CREATE TABLE table20250814_2 ( id INT AUTO_INCREMENT PRIMARY KEY, data JSON)" [] [];

  tt "SELECT JSON_ARRAY_INSERT(data, '$[0]', 'x') as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];
  tt "SELECT JSON_CONTAINS_PATH(data, 'one', '$.a', '$.b') as has_path FROM table20250814_2" [attr' ~nullability:Nullable "has_path" Bool] [];
  tt "SELECT JSON_DEPTH(data) as depth FROM table20250814_2" [attr' ~nullability:Nullable "depth" Int] [];
  tt "SELECT JSON_INSERT(data, '$.a', 1, '$.b', true) as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];

  tt "SELECT JSON_INSERT(@json, '$.a', 1) as r" [attr' ~nullability:Strict "r" Json] [named "json" Json];
  tt "SELECT JSON_INSERT(@json, @p, NULL) as r" [attr' ~nullability:Nullable "r" Json] [named "json" Json; named "p" Json_path];
  tt "SELECT JSON_INSERT(@json, '$.a', @v :: Int Null) as r" [attr' ~nullability:Nullable "r" Json] [named "json" Json; named_nullable "v" Int];
  tt "SELECT JSON_INSERT(@json_nullable :: Json Null, '$.a', 1) as r" [attr' ~nullability:Nullable "r" Json] [named_nullable "json_nullable" Json];
  tt "SELECT JSON_INSERT(@json, '$.a', 1, NULL, 2) as r" [attr' ~nullability:Nullable "r" Json] [named "json" Json; ];

  tt "SELECT JSON_KEYS(data) as keys FROM table20250814_2" [attr' ~nullability:Nullable "keys" Json] [];
  tt "SELECT JSON_KEYS(data, '$') as keys2 FROM table20250814_2" [attr' ~nullability:Nullable "keys2" Json] [];
  tt "SELECT JSON_LENGTH(data) as len FROM table20250814_2" [attr' ~nullability:Strict "len" Int] [];
  tt "SELECT JSON_LENGTH(data, '$.a') as len FROM table20250814_2" [attr' ~nullability:Strict "len" Int] [];

  tt "SELECT JSON_MERGE(data, JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];
  tt "SELECT JSON_MERGE(JSON_OBJECT('y',1), JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' "result" Json] [];

  tt "SELECT JSON_MERGE_PATCH(data, JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];
  tt "SELECT JSON_MERGE_PATCH(JSON_OBJECT('y',1), JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' "result" Json] [];

  tt "SELECT JSON_MERGE_PRESERVE(data, JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];
  tt "SELECT JSON_MERGE_PRESERVE(JSON_OBJECT('y',1), JSON_OBJECT('x',1)) as result FROM table20250814_2" [attr' "result" Json] [];

  tt "SELECT JSON_PRETTY(data) as pretty FROM table20250814_2" [attr' ~nullability:Nullable "pretty" Text] [];
  tt "SELECT JSON_PRETTY('[1, 3, 10]') as pretty FROM table20250814_2" [attr' "pretty" Text] [];

  tt "SELECT JSON_QUOTE('hello') as quoted" [attr' "quoted" Text] [];
  tt "SELECT JSON_REPLACE(data, '$.a', 2) as result FROM table20250814_2" [attr' ~nullability:Nullable "result" Json] [];
  tt "SELECT JSON_REPLACE(JSON_OBJECT('x',1), '$.a', 2) as result FROM table20250814_2" [attr' "result" Json] [];
  tt "SELECT JSON_STORAGE_SIZE(data) as sz FROM table20250814_2" [attr' ~nullability:Nullable "sz" Int] [];
  tt "SELECT JSON_TYPE(data) as t FROM table20250814_2" [attr' ~nullability:Nullable "t" Text] [];
  tt "SELECT JSON_VALID('{\"a\":1}') as v1" [attr' "v1" Bool] [];
  tt "SELECT JSON_VALID('{oops}') as v2" [attr' "v2" Bool] [];
  tt "SELECT JSON_CONTAINS_PATH(data, 'all', @p1, @p2) as has_all FROM table20250814_2" [attr' ~nullability:Nullable "has_all" Bool] [named "p1" Json_path; named "p2" Json_path];

  tt "CREATE TABLE table20250814_3 ( id INT AUTO_INCREMENT PRIMARY KEY, data JSON NOT NULL)" [] [];
  tt "SELECT JSON_CONTAINS_PATH(data, 'one', '$.a') as has_strict FROM table20250814_3" [attr' ~nullability:Nullable "has_strict" Bool] [];
  tt "SELECT JSON_CONTAINS_PATH(data, @mode, '$.a') as has_mode FROM table20250814_2" [attr' ~nullability:Nullable "has_mode" Bool] [named "mode" One_or_all];
  tt "SELECT JSON_CONTAINS_PATH(data, 'one', @p) as has_p FROM table20250814_2" [attr' ~nullability:Nullable "has_p" Bool] [named "p" Json_path];
  tt "SELECT JSON_CONTAINS_PATH(data, 'all', '$.a', @p2) as has_p2 FROM table20250814_2" [attr' ~nullability:Nullable "has_p2" Bool] [named "p2" Json_path];
]

let test_cardinality =
  let x = [attr' ~nullability:Strict "x" ~extra:[PrimaryKey] Int] in
  let y = [attr' ~nullability:Nullable "y" ~extra:[Unique] Int] in
  let z = [attr' ~nullability:Nullable "z" ~extra:[Constraint.make_composite_unique ["z"; "a"]] Int] in
  let a = [attr' ~nullability:Nullable "a" ~extra:[Constraint.make_composite_unique ["z"; "a"]] Int] in
  let b = [attr' ~nullability:Nullable "b" Int] in
  let refined = List.map (fun a -> Sql.{ a with domain = Type.make_strict a.domain }) in
  [
  tt "CREATE TABLE test_cardinality (x INT PRIMARY KEY, y INT, z INT, a INT, b INT, UNIQUE(y), UNIQUE(z, a))" [] [];
  tt "select x from test_cardinality where true" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where false" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = @x" x [named "x" Int] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x != 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x <> 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where not x = 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = x" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where false and x = 1" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 and false" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where true and x = 1" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 and false" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where true or x = 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1 or true" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where false or x = 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1 or false" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x < 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x > 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x <= 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x >= 1" x [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1 and x = 2" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 and x = 2 and x = 3" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 and x = 2 and x = 3 and x = 4" x [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 and x = 2 and x = 3 and x = 4 and x = 5" x [] ~kind:(Select `Zero_one);
  tt "select y from test_cardinality where y = 1" (refined y) [] ~kind:(Select `Zero_one);
  tt "select y from test_cardinality where y != 1" (refined y) [] ~kind:(Select `Nat);
  tt "select z from test_cardinality where z = 1" (refined z) [] ~kind:(Select `Nat);
  tt "select z from test_cardinality where z != 1" (refined z) [] ~kind:(Select `Nat);
  tt "select x from test_cardinality where x = 1 limit 1" x [] ~kind:(Select `Zero_one);
  tt "select y from test_cardinality where y = 1 limit 1" (refined y) [] ~kind:(Select `Zero_one);
  tt "select z from test_cardinality where z = 1 limit 1" (refined z) [] ~kind:(Select `Zero_one);
  tt "select x from test_cardinality where x = 1 limit 2" x [] ~kind:(Select `Zero_one);
  tt "select y from test_cardinality where y = 1 limit 2" (refined y) [] ~kind:(Select `Zero_one);
  tt "select z from test_cardinality where z = 1 limit 2" (refined z) [] ~kind:(Select `Nat);
  tt "select x,y from test_cardinality where x = 1" (x @ y) [] ~kind:(Select `Zero_one);
  tt "select x,y from test_cardinality where x = 1 limit 1" (x @ y) [] ~kind:(Select `Zero_one);
  tt "select x,y from test_cardinality where x = 1 limit 2" (x @ y) [] ~kind:(Select `Zero_one);
  tt "select x,z from test_cardinality where x = 1" (x @ z) [] ~kind:(Select `Zero_one);
  tt "select x,z from test_cardinality where x = 1 limit 1" (x @ z) [] ~kind:(Select `Zero_one);
  tt "select x,z from test_cardinality where x = 1 limit 2" (x @ z) [] ~kind:(Select `Zero_one);
  tt "select x,z from test_cardinality where z = 1" (x @ refined z) [] ~kind:(Select `Nat);
  tt "select x,z from test_cardinality where z = 1 limit 1" (x @ refined z) [] ~kind:(Select `Zero_one);
  tt "select x,z from test_cardinality where z = 1 limit 2" (x @ refined z) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where z = 1 and a = 1" (refined z @ refined a) [] ~kind:(Select `Zero_one);
  tt "select z,a from test_cardinality where z = 1 and not (a = 1)" (refined z @ refined a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where not (z = 1 and a = 1)" (z @ a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where not (z = 1) and a = 1" (refined z @ refined a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where not not (a = 1)" (z @ refined a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where z = 1 and a != 1" (refined z @ refined a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where z != 1 and a = 1" (refined z @ refined a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where (z = 1) and (a = 1)" (refined z @ refined a) [] ~kind:(Select `Zero_one);
  tt "select z,a from test_cardinality where z = 1 and a = 1 limit 1" (refined z @ refined a) [] ~kind:(Select `Zero_one);
  tt "select z,a from test_cardinality where z = 1 and a = 1 limit 2" (refined z @ refined a) [] ~kind:(Select `Zero_one);
  tt "select z,a,b from test_cardinality where z = 1 and a = 1 and b = 1" (refined z @ refined a @ refined b) [] ~kind:(Select `Zero_one);
  tt "select z,a,b from test_cardinality where z = 1 and a = 1 and b = 1 limit 1" (refined z @ refined a @ refined b) [] ~kind:(Select `Zero_one);
  tt "select z,a,b from test_cardinality where z = 1 and a = 1 and b = 1 limit 2" (refined z @ refined a @ refined b) [] ~kind:(Select `Zero_one);
  tt "select z,a,b from test_cardinality where z = 1 and a = 1 and not b = 1" (refined z @ refined a @ refined b) [] ~kind:(Select `Zero_one);
  tt "select z,a from test_cardinality where z = 1" (refined z @ a) [] ~kind:(Select `Nat);
  tt "select z,a from test_cardinality where z = 1 limit 1" (refined z @ a) [] ~kind:(Select `Zero_one);
  tt "select z,a from test_cardinality where z = 1 limit 2" (refined z @ a) [] ~kind:(Select `Nat);
  tt "select a,b from test_cardinality where a = 1 and b = 1" (refined a @ refined b) [] ~kind:(Select `Nat);
]

let test_cardinality_optimization_validity = 
  let x = [attr' ~nullability:Strict "x" ~extra:[PrimaryKey] Int] in
  let id = [attr' ~nullability:Nullable "id" Int] in
  let one_x = [attr' ~nullability:Strict "one_x" Int] in
  [
  tt "CREATE TABLE tc2_1 (x INT PRIMARY KEY)" [] [];
  tt "CREATE TABLE tc2_2 (id INT, one_x INT, FOREIGN KEY (one_x) REFERENCES tc2_1(x))" [] [];
  tt "select * from tc2_1 where x = 1" x [] ~kind:(Select `Zero_one);
  tt "select * from tc2_2 where one_x = 1" (id @ one_x) [] ~kind:(Select `Nat);
  (* below should return one row -- tc2_1.x is a primary key, but tc2_2.one_x is not *)
  tt "select * from tc2_2 join tc2_1 on tc2_2.one_x = tc2_1.x where tc2_1.x = 1" (id @ one_x @ x) [] ~kind:(Select `Nat);
  (* below should return multiple rows -- tc2_1.x is a primary key, but joining on it allows multiple rows with it to be returned *)
  tt "select * from tc2_1 join tc2_2 on tc2_1.x = tc2_2.one_x where tc2_1.x = 1" (x @ id @ one_x) [] ~kind:(Select `Nat);
]

let test_nullability_narrowing =
  let n name = attr' ~nullability:Nullable name Int in
  let s name = attr' ~nullability:Strict name Int in
  [
  tt "CREATE TABLE narrow1 (a INT, b INT, c TEXT)" [] [];
  tt "CREATE TABLE narrow2 (d INT, e INT)" [] [];

  tt "SELECT a FROM narrow1 WHERE a = 5" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a > 5" [s "a"] [];
  tt "SELECT a, b FROM narrow1 WHERE a > b" [s "a"; s "b"] [];
  tt "SELECT a FROM narrow1 WHERE a <> 5" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE NOT (a = 5)" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE NOT NOT (a = 5)" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a = a" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a" [s "a"] [];

  tt "SELECT a + 1 AS r FROM narrow1 WHERE a IS NOT NULL" [s "r"] [];
  tt "SELECT a + b AS r FROM narrow1 WHERE a > 0 AND b > 0" [s "r"] [];
  tt "SELECT a FROM narrow1 WHERE a + 1 = 5" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE (a + 1) IS NOT NULL" [s "a"] [];

  tt "SELECT a FROM narrow1 WHERE CAST(a AS DECIMAL) = 1" [n "a"] [];
  tt "SELECT c FROM narrow1 WHERE CONCAT(c, c) = 'x'" [attr' ~nullability:Nullable "c" Text] [];
  tt "SELECT c FROM narrow1 WHERE CONCAT_WS(',', c, c) = 'x'" [attr' ~nullability:Nullable "c" Text] [];
  tt "SELECT a, b FROM narrow1 WHERE IF(a = 1, b = 2, b = 2)" [n "a"; n "b"] [];

  tt "SELECT a, b FROM narrow1 WHERE a = 1 AND b = 2" [s "a"; s "b"] [];
  tt "SELECT a, b FROM narrow1 WHERE a = 1 OR b = 2" [n "a"; n "b"] [];
  tt "SELECT a FROM narrow1 WHERE a = 1 OR a = 2" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a IS NULL OR a > 3" [n "a"] [];
  tt "SELECT a, b FROM narrow1 WHERE NOT (a = 1 AND b = 2)" [n "a"; n "b"] [];

  tt "SELECT a FROM narrow1 WHERE a <=> 5" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE COALESCE(a, 0) = 1" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE COALESCE(a, a) = 1" [s "a"] [];

  tt "SELECT a FROM narrow1 WHERE a IN (1, 2)" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a NOT IN (1, 2)" [s "a"] [];
  (* an empty subquery makes IN false and NOT IN true even for a NULL left operand, and the
     grammar drops the NOT, so membership in a subquery must not narrow at all *)
  tt "SELECT a FROM narrow1 WHERE a IN (SELECT d FROM narrow2)" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE a NOT IN (SELECT d FROM narrow2)" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE NOT (a IN (SELECT d FROM narrow2))" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE a = ANY (SELECT d FROM narrow2)" [s "a"] [];
  tt "SELECT a FROM narrow1 WHERE a > ANY (SELECT d FROM narrow2)" [s "a"] [];
  (* <=> is null-safe, so it narrows nothing even under ANY *)
  tt "SELECT a FROM narrow1 WHERE a <=> ANY (SELECT d FROM narrow2)" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE a = ALL (SELECT d FROM narrow2)" [n "a"] [];
  tt "SELECT a FROM narrow1 WHERE a BETWEEN 1 AND 2" [s "a"] [];
  tt "SELECT a, b FROM narrow1 WHERE a IN (b, 1)" [s "a"; n "b"] [];
  tt "SELECT a, b FROM narrow1 WHERE a BETWEEN b AND 1" [s "a"; n "b"] [];

  tt "SELECT a, b FROM narrow1 WHERE CASE WHEN a = 1 THEN b = 2 END" [s "a"; s "b"] [];
  tt "SELECT a, b FROM narrow1 WHERE CASE WHEN a = 1 THEN b = 2 ELSE b = 3 END" [n "a"; s "b"] [];
  tt "SELECT a, b FROM narrow1 WHERE CASE WHEN a = 1 THEN b = 2 ELSE a = 3 END" [s "a"; n "b"] [];

  tt "SELECT a, d FROM narrow1 JOIN narrow2 ON a = d" [s "a"; s "d"] [];
  tt "SELECT a, d FROM narrow1 LEFT JOIN narrow2 ON a = d" [n "a"; n "d"] [];
  tt "SELECT a, d FROM narrow1 RIGHT JOIN narrow2 ON a = d" [n "a"; n "d"] [];
  tt "SELECT a FROM narrow1 JOIN narrow2 ON a = d RIGHT JOIN narrow2 x2 ON a = x2.e" [n "a"] [];

  tt "SELECT a, b FROM narrow1 GROUP BY a HAVING a > 1" [s "a"; n "b"] [];
  tt "SELECT a, b FROM narrow1 GROUP BY a HAVING b > 1" [n "a"; n "b"] [];
]

let test_nullability_rules = [
  tt "CREATE TABLE test20250819 (a INT, b INT NOT NULL, c TEXT)" [] [];

  tt "SELECT a <=> b AS r FROM test20250819" [attr' "r" Bool] [];
  tt "SELECT NULL <=> NULL AS r" [attr' "r" Bool] [];
  tt "SELECT 1 IS NOT DISTINCT FROM NULL AS r" [attr' "r" Bool] [];
  tt "SELECT NULLIF(NULL, 1) AS r" [attr' ~nullability:Nullable "r" Int] [];
  tt "SELECT NULLIF(1, a) AS r FROM test20250819" [attr' ~nullability:Nullable "r" Int] [];
  tt "SELECT NULLIF(1, 1) AS r" [attr' ~nullability:Nullable "r" Int] [];
  tt "SELECT IFNULL(NULL, 1) AS r" [attr' "r" Int][];
  tt "SELECT IFNULL(1, a) AS r FROM test20250819" [attr' "r" Int][];
  tt "SELECT IFNULL(a, a) AS r FROM test20250819" [attr' ~nullability:Nullable "r" Int][];
  tt "SELECT IFNULL(NULL, a) AS r FROM test20250819" [attr' ~nullability:Nullable "r" Int][];
  tt "SELECT 1 as r FROM test20250819 WHERE a < @param" [attr' "r" Int][ named "param" Int ]
]

let test_fn_group_by_arg = [
  tt {|
    CREATE TABLE table_1_2025_09_26 (
      id INT PRIMARY KEY AUTO_INCREMENT,
      date_1 DATE,
      table_no INT,
      date1_strict INT NOT NULL
    )
  |} [] [];

  tt {|
    CREATE TABLE table_2_2025_09_26 (
        id INT PRIMARY KEY AUTO_INCREMENT,
        date_2 DATE,
        table_no INT
    )
  |} [] [];

  tt {|
    SELECT 
      t1.table_no,
      GROUP_CONCAT(t1.date_1 ORDER BY t1.date_1 DESC) AS dates_from_t1,
      GROUP_CONCAT(t1.date1_strict ORDER BY t1.date1_strict DESC) AS dates_from_t1_strict,
      GROUP_CONCAT(t2.date_2 ORDER BY t2.date_2 DESC) AS dates_from_t2
    FROM table_1_2025_09_26 t1
    JOIN table_2_2025_09_26 t2 ON t1.table_no = t2.table_no
    GROUP BY t1.table_no
    ORDER BY dates_from_t1; 
  |} [
    attr' ~nullability:Strict "table_no" Int;
    attr' ~nullability:Nullable "dates_from_t1" Text;
    attr' ~nullability:Strict "dates_from_t1_strict" Text;
    attr' ~nullability:Nullable "dates_from_t2" Text;
  ] [];

  tt {|
    SELECT 
      t1.table_no,
      GROUP_CONCAT(
          t1.date_1 
          ORDER BY YEAR(t1.date_1) * 10000 + MONTH(t1.date_1) * 100 + DAY(t1.date_1) DESC
      ) AS dates_from_t1,
      GROUP_CONCAT(
          t2.date_2 
          ORDER BY DAYOFYEAR(t2.date_2) ASC
      ) AS dates_from_t2
  FROM table_1_2025_09_26 t1
  JOIN table_2_2025_09_26 t2 ON t1.table_no = t2.table_no
  GROUP BY t1.table_no
  ORDER BY dates_from_t1;
  |} [
    attr' ~nullability:Strict "table_no" Int;
    attr' ~nullability:Nullable "dates_from_t1" Text;
    attr' ~nullability:Nullable "dates_from_t2" Text;
  ] [];

  tt {|
    SELECT 
      t1.table_no,
      GROUP_CONCAT(t1.date_1 ORDER BY YEAR(t1.date_1) + @delta) AS dates_from_t1
    FROM table_1_2025_09_26 t1
    GROUP BY t1.table_no
  |} [
    attr' ~nullability:Nullable "table_no" Int;
    attr' ~nullability:Nullable "dates_from_t1" Text;
  ] [
    named "delta" Int;
  ];

  tt {|
    SELECT 
      t1.table_no,
      GROUP_CONCAT(t1.date_1 ORDER BY YEAR(t1.date_1) + @delta) AS dates_from_t1
    FROM table_1_2025_09_26 t1
    WHERE t1.table_no > @delta
    GROUP BY t1.table_no
  |} [
    attr' ~nullability:Strict "table_no" Int;
    attr' ~nullability:Nullable "dates_from_t1" Text;
  ] [
    named "delta" Int;
    named "delta" Int;
  ];
]

let test_join_hole_whitespace =
  let join_var sql sub =
    let j1 = String.find sql sub in
    Sql.DynamicSelectJoin {
      pid = { value = Some "col"; pos = (0,0) };
      pos = (j1, j1 + String.length sub);
      source = { table = make_table_name "b"; alias = None };
    }
  in
  let rec show_piece = function
    | Gen.Static s -> sprintf "Static %S" s
    | Gen.Cond (_, body) -> sprintf "Cond [%s]" (String.concat "; " (List.map show_piece body))
    | _ -> "Other"
  in
  let check name sql joins expected =
    name >:: (fun () ->
      assert_equal
        ~cmp:(fun a b -> List.map show_piece a = List.map show_piece b)
        ~printer:(fun l -> String.concat "; " (List.map show_piece l))
        expected
        (Gen.substitute_vars sql (List.map (join_var sql) joins) None))
  in
  let join text = Gen.Cond (Gen.Dep_selected ({ value = Some "col"; pos = (0,0) }, 0), [Gen.Static text]) in
  [
    check "no holes"
      "SELECT x\nFROM a\nWHERE y = 1" []
      [Gen.Static "SELECT x\nFROM a\nWHERE y = 1"];
    check "newline before hole absorbed"
      "SELECT x FROM a\nLEFT JOIN b ON b.a = a.id\nWHERE y = 1"
      ["LEFT JOIN b ON b.a = a.id"]
      [Gen.Static "SELECT x FROM a"; join " LEFT JOIN b ON b.a = a.id"; Gen.Static "\nWHERE y = 1"];
    check "space before hole absorbed"
      "FROM a LEFT JOIN b ON b.a = a.id WHERE y = 1"
      ["LEFT JOIN b ON b.a = a.id"]
      [Gen.Static "FROM a"; join " LEFT JOIN b ON b.a = a.id"; Gen.Static " WHERE y = 1"];
    check "hole at end of query"
      "FROM a\nLEFT JOIN b ON b.a = a.id"
      ["LEFT JOIN b ON b.a = a.id"]
      [Gen.Static "FROM a"; join " LEFT JOIN b ON b.a = a.id"];
    check "adjacent holes leave no gap"
      "FROM a\nLEFT JOIN b ON b.a = a.id\nLEFT JOIN c ON c.a = a.id\nWHERE y = 1"
      ["LEFT JOIN b ON b.a = a.id"; "LEFT JOIN c ON c.a = a.id"]
      [Gen.Static "FROM a"; join " LEFT JOIN b ON b.a = a.id"; join " LEFT JOIN c ON c.a = a.id"; Gen.Static "\nWHERE y = 1"];
    check "static join between holes"
      "FROM a\nLEFT JOIN b ON b.a = a.id\nJOIN o USING (x)\nLEFT JOIN c ON c.a = a.id"
      ["LEFT JOIN b ON b.a = a.id"; "LEFT JOIN c ON c.a = a.id"]
      [Gen.Static "FROM a"; join " LEFT JOIN b ON b.a = a.id"; Gen.Static "\nJOIN o USING (x)"; join " LEFT JOIN c ON c.a = a.id"];
    check "string literal whitespace preserved"
      "FROM a\nLEFT JOIN b ON b.a = a.id\nWHERE note = '  two  spaces  '"
      ["LEFT JOIN b ON b.a = a.id"]
      [Gen.Static "FROM a"; join " LEFT JOIN b ON b.a = a.id"; Gen.Static "\nWHERE note = '  two  spaces  '"];
  ]

let test_migration_name =
  let module Name = Migration_id.Name in
  let head = Name.words "alter_blog_sync_jobs" in
  let actions =
    [ Name.action (Name.words "add_col") (Name.words "redirects_synced");
      Name.action (Name.words "add_col") (Name.words "redirects_skipped");
      Name.action (Name.words "add_pk") [] ]
  in
  let render limit expected =
    assert_equal ~printer:(fun s -> s) expected (Name.render limit (Name.make head actions))
  in
  let uncapped expected = "uncapped" >:: (fun () -> render None expected) in
  let cut n expected = sprintf "cap %d" n >:: (fun () -> render (Some n) expected) in
  let flat limit before after =
    sprintf "fit %S" before >:: (fun () ->
      assert_equal ~printer:(fun s -> s) after (Name.fit limit before))
  in
  let whole = "alter_blog_sync_jobs_add_col_redirects_synced_add_col_redirects_skipped_add_pk" in
  [
    uncapped whole;
    cut 78 whole;
    cut 77 "alter_blog_sync_jobs_add_col_redirects_synced_add_col_redirects_skipped";
    cut 60 "alter_blog_sync_jobs_add_col_redirects_synced_add_col";
    cut 53 "alter_blog_sync_jobs_add_col_redirects_synced_add_col";
    cut 52 "alter_blog_sync_jobs_add_col_redirects_synced";
    cut 27 "alter_blog_sync_jobs";
    cut 19 "alter_blog_sync";
    cut 6 "alter";

    flat None "whatever_it_is"            "whatever_it_is";
    flat (Some 15)  "alter_users_add_col_email" "alter_users_add";
    flat (Some 11)  "alter_users_add_col_email" "alter_users";
  ]

let run () =
  Gen.params_mode := Some Named;
  let tests =
  [
    "simple" >::: test;
    "multi-table UPDATE" >::: test2;
    "gotchas" >::: test3;
    "single-row SELECT" >::: test4;
    "parsing" >::: test_parsing;
    "JOIN result columns" >:: test_join_result_cols;
    "enum" >::: test_enum;
    "manual_param" >::: test_manual_param;
    "test_left_join" >::: test_left_join;
    "test_coalesce" >::: test_coalesce;
    "test_primary_strict" >::: test_primary_strict;
    "test_not_null_default_field" >::: test_not_null_default_field;
    "test_update_join" >::: test_update_join;
    "test_param_not_null_by_default" >::: test_param_not_null_by_default;
    "test_in_clause_with_tuple_sets" >:: test_in_clause_with_tuple_sets;
    "test_agg_nullable" >::: test_agg_nullable;
    "cte_possible_rec_non_shared_select_only" >::: cte_possible_rec_non_shared_select_only;
    "test_ambiguous" >::: test_ambiguous;
    "test_subquery_nullability" >::: test_subquery_nullability;
    "test_values_row" >::: test_values_row;
    "test_select_exposed_alias" >::: test_select_exposed_alias;
    "test_enum_as_variant" >::: test_enum_as_variant;
    "test_enum_literal" >:: test_enum_literal;
    "test_add_with_window_function" >::: test_add_with_window_function;
    "test_meta_propagation" >::: test_meta_propagation;
    "test_case_enum" >::: test_case_enum;
    "test_type_mapping_params" >:: test_type_mapping_params;
    "test_meta_insert_update" >:: test_meta_insert_update;
    "test_meta_loss_query" >::: test_meta_loss_query;
    "test_meta_lattice" >::: test_meta_lattice;
    "test_meta_equality" >::: test_meta_equality;
    "test_meta_functions" >::: test_meta_functions;
    "test_meta_foreign_key" >::: test_meta_foreign_key;
    "test_meta_union_null_placeholder" >::: test_meta_union_null_placeholder;
    "test_meta_union_enum_literal" >::: test_meta_union_enum_literal;
    "test_operand_order" >::: test_operand_order;
    "test_multi_functions" >::: test_multi_functions;
    "test_on_conflict_do_update" >::: test_on_conflict_do_update;
    "test_enum_with_in_and_between" >::: test_enum_with_in_and_between;
    "test_datefns" >::: test_datefns;
    "test_json_and_fixed_then_pairs_fn_kind" >::: test_json_and_fixed_then_pairs_fn_kind;
    "test_json_arrow_ops" >::: test_json_arrow_ops;
    "test_json_additional_functions" >::: test_json_additional_functions;
    "test_cardinality" >::: test_cardinality;
    "test_cardinality_optimization_validity" >::: test_cardinality_optimization_validity;
    "test_nullability_rules" >::: test_nullability_rules;
    "test_nullability_narrowing" >::: test_nullability_narrowing;
    "test_fn_group_by_arg" >::: test_fn_group_by_arg;
    "test_join_hole_whitespace" >::: test_join_hole_whitespace;
    "migration name" >::: test_migration_name;
  ]
  in
let test_suite = "main" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0

