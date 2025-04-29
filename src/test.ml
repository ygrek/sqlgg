open Printf
open ExtLib
open OUnit
open Sqlgg
open Sql
(* open Sql.Type *)
open Stmt

let cmp_params p1 p2 =
  try
    List.for_all2 (fun p1 p2 ->
      p1.id.label = p2.id.label && Type.equal p1.typ p2.typ && p1.id.pos = (0,0) && snd p2.id.pos > fst p2.id.pos)
    p1 p2
  with
    _ -> false

let parse sql =
  match Main.parse_one (sql,[]) with
  | exception exn -> assert_failure @@ sprintf "failed : %s : %s" (Printexc.to_string exn) sql
  | None -> assert_failure @@ sprintf "Failed to parse : %s" sql
  | Some stmt -> stmt

let do_test sql ?kind schema params =
  let stmt = parse sql in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string schema stmt.schema;
  assert_equal ~msg:"params" ~cmp:cmp_params ~printer:Sql.show_params params
  (List.map (function Single p -> p | SharedVarsGroup _ | OptionActionChoice _ | SingleIn _ | Choice _ | ChoiceIn _ | TupleList _ -> assert false) stmt.vars);
  match kind with
  | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
  | None -> ()

let tt sql ?kind schema params =
  let test () = do_test sql ?kind schema params in
  sql >:: test

let wrong sql =
  sql >:: (fun () -> ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one' (sql,[])); false with _ -> true))

let attr ?(extra=[]) n d = make_attribute n (Some d) (Constraints.of_list extra)
let attr' ?(extra=[]) ?(nullability=Type.Strict) name kind =
  let domain: Type.t = { t = kind; nullability; } in
  {name;domain;extra=Constraints.of_list extra }

let named s t = new_param { label = Some s; pos = (0,0) } (Type.strict t)
let named_nullable s t = new_param { label = Some s; pos = (0,0) } (Type.nullable t)
let param_nullable t = new_param { label = None; pos = (0,0) } (Type.nullable t)
let param t = new_param { label = None; pos = (0,0) } (Type.strict t)

let test = Type.[
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)" [] [];
  tt "SELECT str FROM test WHERE id=?"
     [attr' ~nullability:(Nullable) "str" Text]
     [param Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr' "x" Text; attr' ~nullability:(Nullable) "z" Int]
     [param_nullable Int; named "id" Int; named "lim" Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [  attr' ~nullability:(Nullable) "name" Text;
        attr' ~nullability:(Nullable) "other_name" Text
     ]
     [named_nullable "delta" Int];
  tt "select test.name from test where test.id + @x = ? or test.id - @x = ?"
     [attr' ~nullability:(Nullable) "name" Text;]
     [named_nullable "x" Int; param Int; named_nullable "x" Int; param Int;];
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
    [attr' ~nullability:(Nullable) "str" Text; attr' ~nullability:(Nullable) "q" (StringLiteral "qqq")]
    [named_nullable "id" Int; named_nullable "id" Int; named_nullable "def" (StringLiteral "qqq")];
   wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr' ~nullability:(Nullable) "str" Text;]
    [named "id" Int; named_nullable "x" Int; named "id" Int];
  tt "select 3/5"
    [attr' ~nullability:(Strict) "" Float;]
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
  tt "select 1" [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10)"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `One);
  tt "select greatest(1+2,10) where 1 = 2"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Zero_one);
  tt "select 1 from test4"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test"
    [attr' ~nullability:(Strict)  "" Int; attr' ~nullability:(Strict) "" Text] [] ~kind:(Select `Nat);
  tt "select greatest(10,x) from test4" [attr' ~nullability:(Nullable) "" Int] [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y"  [attr' ~nullability:(Strict) "" Int] [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr' ~nullability:(Nullable) "q" Int] [named_nullable "n" Int] ~kind:(Select `One);
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr' ~nullability:(Strict) "q" Int] [named_nullable "n" Int] ~kind:(Select `One); 
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
    if String.ends_with name ~suffix:"?" then
      Sql.{ name = String.slice ~last:(-1) name; domain = Type.(nullable Int); extra = Constraints.empty; }
    else
      attr name Int)
  in
  do_test "CREATE TABLE t1 (i INT, j INT)" [] [];
  do_test "CREATE TABLE t2 (k INT, j INT)" [] [];
  do_test "SELECT * FROM t1 JOIN t2 ON t1.j=t2.j" (ints ["i";"j";"k";"j"]) [];
  do_test "SELECT * FROM t1 LEFT JOIN t2 ON t1.j=t2.j" (ints ["i";"j";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 RIGHT JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k";"j"]) [];
  do_test "SELECT * FROM t1 FULL JOIN t2 ON t1.j=t2.j" (ints ["i?";"j?";"k?";"j?"]) [];
  do_test "SELECT * FROM t1 NATURAL JOIN t2" (ints ["j";"i";"k"]) [];
  do_test "SELECT * FROM t1 JOIN t2 USING (j)" (ints ["j";"i";"k"]) [];
(*   NATURAL JOIN with common column in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE j > @x"
    (ints ["j";"i";"k"])
    [named"x" Int];
(*   NATURAL JOIN with common column qualified in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x"
    (ints ["j";"i";"k"])
    [named "x" Int];
  ()

let test_enum = [
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["true"; "false"]); is_closed = true })) ~extra:[NotNull;]; attr ~extra:[WithDefault;] "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["true"; "false"]); is_closed = true })) ~extra:[NotNull;]; attr "" Int] [];
]

let test_manual_param = [
  tt "CREATE TABLE test7 (x INT NULL DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test7 WHERE x = @x_arg" [attr "x" Int ~extra:[Null; WithDefault];] [
    named "x_arg" Int
  ];
  tt "SELECT * FROM test7 WHERE x = @x_arg::Int" [attr "x" Int ~extra:[Null; WithDefault];] [
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
  {name="type_name"; domain=Type.nullable Text; extra=(Constraints.of_list [Constraint.NotNull]);}] [];
]

let test_coalesce = [
  tt "CREATE TABLE test8 (x integer unsigned null)" [] [];
  tt "SELECT COALESCE(x, null, null) as x FROM test8" [attr' ~nullability:(Nullable) "x" Int;] [];
  tt "SELECT COALESCE(x, coalesce(null, null, 75, null), null) as x FROM test8" [attr' ~nullability:Strict "x" Int;] [];
]

let test_primary_strict = [
  tt "CREATE TABLE test9 (x BIGINT UNSIGNED PRIMARY KEY)" [] [];
  tt "SELECT x FROM test9 WHERE x > 100" [attr' ~extra:[PrimaryKey] ~nullability:(Strict) "x" Int;] [];
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
  |} [attr "a" Int ~extra:[];] [
    named "a" Int;
    named "ab" Int;
    named_nullable "x" Int;
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
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [attr' ~nullability:(Nullable) "a" Int] stmt.schema;
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
  |} [attr' ~nullability:(Nullable) "avg_value" Float] [];
  tt {| 
    SELECT MAX(value) as max_value FROM test18
  |} [attr' ~nullability:(Nullable) "max_value" Int] [];
  tt {| 
    SELECT MAX(value) as max_value FROM test18 GROUP BY id
  |} [attr' "max_value" Int] [];
  tt {| 
    SELECT MAX(value) as max_value, MAX(id) as max_id
    FROM test18 GROUP BY id
  |} [attr' "max_value" Int; attr' "max_id" ~nullability:(Nullable) Int] [];
  tt {| 
    SELECT AVG(value) as avg_value, AVG(id) as avg_id
    FROM test18
  |} [
    attr' "avg_value" ~nullability:(Nullable) Float; 
    attr' "avg_id" ~nullability:(Nullable) Float
  ] [];
  tt {| 
    SELECT MAX((SELECT value FROM test18 WHERE value = 100)) AS result
    FROM test18
    GROUP BY value
  |} [
  attr' "result" ~nullability:(Nullable) Int; 
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
  attr' "result" ~nullability:(Nullable) Int; 
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
  |} [ attr' ~nullability:(Nullable) "result" Int; ][];
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
  |} [ attr' ~nullability:(Nullable) "num" Int;][];
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
    attr' ~nullability:Nullable "col_value" Decimal;
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
    attr' ~nullability:Nullable "avg_value" Float;
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
    attr' ~nullability:(Nullable) "id" Int;
  ] [];
  (* Wrong parses and asserts fail *)
  wrong "select test23.id from test23 join test24 on test23.id = test24.id where id > 2 order by id";
  tt "select test23.id from test23 join test24 on test23.id = test24.id group by id" [
    attr' ~nullability:(Nullable) "id" Int;
  ] [];
  tt "select test23.id as test from test23 join test24 on test23.id = test24.id group by column_a" [
    attr' ~nullability:(Nullable) "test" Int;
  ][];
  tt "select test23.id, test24.id from test23 join test24 on test23.id = test24.id" [
    attr' ~nullability:(Nullable) "id" Int;
    attr' ~nullability:(Nullable) "id" Int;
  ] [];
  (* Wrong parses and asserts fail *)
  wrong "select id, id from test23 join test24 on test23.id = test24.id group by id";
  wrong "select id as id1, id as id2 from test23 join test24 on test23.id = test24.id group by id";
  wrong "select test23.id, test24.id from test23 join test24 on test23.id = test24.id group by id";
  tt "select test23.id from test23 join test24 on test23.id = test24.id group by id, column_a" [
    attr' ~nullability:(Nullable) "id" Int;
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
    attr' ~nullability:(Nullable) "id1" Int;
    attr' ~nullability:(Nullable) "id2" Int;
  ] [];
  tt "SELECT test23.id, test24.id FROM test23 JOIN test24 ON test23.id = test24.id GROUP BY test23.id" [
    attr' ~nullability:(Nullable) "id" Int;
    attr' ~nullability:(Nullable) "id" Int;
  ][];
  wrong "SELECT COUNT(id) FROM test23 JOIN test24 ON test23.id = test24.id";
  wrong "SELECT COUNT(id) as id FROM test23 JOIN test24 ON test23.id = test24.id";
  wrong "SELECT id FROM test23 JOIN test24 ON test23.id = test24.id WHERE id > 2";
  tt "SELECT test23.id AS test_id, test24.id AS other_id FROM test23 JOIN test24 ON test23.id = test24.id" [
    attr' ~nullability:(Nullable) "test_id" Int;
    attr' ~nullability:(Nullable) "other_id" Int;
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
    attr' ~nullability:(Nullable) "id_from_test27" Int;
    attr' ~nullability:(Nullable) "value_from_test28" Int;
    attr' ~nullability:(Nullable) "value_from_test29" Int;
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
    attr' ~nullability:Nullable "c" Decimal;
    attr' ~nullability:Nullable "d" Decimal;
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
    attr' "price" Float;
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
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] stmt.schema;

  let stmt2 = parse {|INSERT INTO test36 VALUES('active')|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] stmt2.schema;
  
  let stmt3 = parse {|INSERT INTO test36 VALUES('deleted')|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] stmt3.schema;

  let stmt4 = parse {|SELECT * FROM test36 WHERE status = 'active'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string 
    [attr' ~extra:[NotNull; WithDefault] "status" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"] ); is_closed = true }))]
    stmt4.schema;

  let stmt5 = parse {|UPDATE test36 SET status = 'deleted' WHERE status = 'pending'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] stmt5.schema;

  let stmt6 = parse {|
    SELECT * FROM test36 
    WHERE status IN ('active', 'pending') 
    AND status != 'deleted'
  |} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string
    [attr' ~extra:[NotNull; WithDefault] "status" 
      (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["active"; "pending"; "deleted"]); is_closed = true }))]
    stmt6.schema;

  ignore @@ wrong {|INSERT INTO test36 VALUES('deleteddd')|} ;
  ignore @@ wrong {|INSERT INTO test36 VALUES((IF(TRUE, 'a', 'b')))|} ;

  let stmt7 = parse {|INSERT INTO test36 VALUES((IF(TRUE, 'pending', 'active')))|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string [] stmt7.schema;

  ignore @@ wrong {|INSERT INTO test36 VALUES((IF(TRUE, 'pending', 'b')))|};
  ignore @@ wrong {|INSERT INTO test36 VALUES(CONCAT(''))|};

  ignore @@ wrong {|SELECT * FROM test36 WHERE status = 'activee'|};

  let stmt8 = parse {|SELECT CONCAT(status, 'test') AS named FROM test36 WHERE status = 'active'|} in
  assert_equal ~msg:"schema" ~printer:Sql.Schema.to_string 
    [attr' ~extra:[] "named" Text]
    stmt8.schema

let test_add_with_window_function = [
  (* Most aggregate functions also can be used as window functions *)
  tt {| SELECT SUM(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT COUNT(*) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT AVG(1) OVER() WHERE FALSE |} [attr' ~nullability:(Nullable) "" Float;] [];
  tt {| SELECT MIN(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT MAX(1) OVER() WHERE FALSE |} [attr' "" Int;] [];
  tt {| SELECT MAX(NULL) OVER() |} [attr' ~nullability:(Nullable) "" Any;] [];

  (* Same but with PARTITION BY and ORDER BY *)
  tt {| SELECT SUM(1) OVER(PARTITION BY COALESCE(NULL, 'a')) |} [attr' "" Int;] [];
  tt {| SELECT SUM(1) OVER(ORDER BY 1 + 1) |} [attr' "" Int;] [];
  tt {| SELECT SUM(1) OVER(PARTITION BY CONCAT('a') ORDER BY 1 - 0) |} [attr' "" Int;] [];

  tt {| SELECT 1 + (SELECT COUNT(1) OVER() WHERE FALSE) |} [attr' ~nullability:(Nullable) "" Int;] [];

  tt {| SELECT CASE WHEN SUM(2) OVER() > 100 THEN 'High' ELSE 'Low' END |} 
    [attr' "" (Type.(Union { ctors = (Enum_kind.Ctors.of_list ["High"; "Low"]); is_closed = false }));] [];

  tt {| SELECT (NULL - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) |} [attr' ~nullability:(Nullable) "" Float;] [];  
  tt {| SELECT (0 - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) |} [attr' "" Float;] [];  

  tt {| SELECT 1 + (SELECT (0 - MIN(2.0) OVER()) / (MAX(3) OVER() - MIN(4) OVER()) ) |} [attr' ~nullability:(Nullable) "" Float;] [];

  (* Non window, non agregate can't be  used *)
  wrong "SELECT IF(TRUE, 1, 2) OVER() WHERE FALSE" ;
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
  ]
  in
  let test_suite = "main" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
