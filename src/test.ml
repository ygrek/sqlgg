open Printf
open OUnit
open Sql
open Sql.Type
open Stmt

let named s = (Some s,(0,0))
let param = (None,(0,0))
let p name t = (named name, t)

let cmp_params p1 p2 =
  try
    List.for_all2 (fun ((name1,pos1),t1) ((name2,pos2),t2) ->
      name1 = name2 && t1 = t2 && pos1 = (0,0) && snd pos2 > fst pos2)
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
  assert_equal ~msg:"params" ~cmp:cmp_params ~printer:Sql.params_to_string params stmt.params;
  match kind with
  | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
  | None -> ()

let tt sql ?kind schema params =
  let test () = do_test sql ?kind schema params in
  sql >:: test

let wrong sql =
  sql >:: (fun () -> ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one_exn (sql,[])); false with _ -> true))

let test = [
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)" [] [];
  tt "SELECT str FROM test WHERE id=?"
     [attr "str" Text]
     [param, Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr "x" Text; attr "z" Int]
     [param,Int; named "id", Int; named "lim",Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [attr "name" Text; attr "other_name" Text]
     [named "delta", Int];
  tt "select test.name from test where test.id + @x = ? or test.id - @x = ?"
     [attr "name" Text;]
     [named "x", Int; param, Int; named "x", Int; param, Int;];
  tt "insert into test values"
     []
     [p "id" Int; p "str" Text; p "name" Text];
  tt "insert into test (str,name) values"
     []
     [p "str" Text; p "name" Text];
  tt "insert into test values (2,'hello' || ' world',@name)"
     []
     [p "name" Text];
  tt "insert or replace into test values (2,?,?)" [] [param,Text; param,Text;];
  tt "replace into test values (2,?,?)" [] [param,Text; param,Text;];
  tt "select str, case when id > @id then name when id < @id then 'qqq' else @def end as q from test"
    [attr "str" Text; attr "q" Text]
    [p "id" Int; p "id" Int; p "def" Text];
  wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr "str" Text;]
    [p "id" Int; p "x" Int; p "id" Int];
]

let test2 = [
  tt "CREATE TABLE test2 (id INT, str TEXT)" [] [];
  tt    "update test, (select * from test2) as x set str = x.str where test.id=x.id" [] [];
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
(*   tt "select max( * ) from test4" a [] ~kind:(Select `One); *)
  tt "select max(x) as q from test4" [attr "q" Int] [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x+y) from test4 limit 1" a [] ~kind:(Select `One);
  tt "select max(y) from test4 limit 2" a [] ~kind:(Select `One);
(* TODO sqlite treats multi-arg max to non-grouping
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4 limit 1" a [] ~kind:(Select `Zero_one);
  tt "select max(x,y) from test4 limit 2" a [] ~kind:(Select `Nat);
*)
  tt "select 1" a [] ~kind:(Select `One);
  tt "select greatest(1+2,10)" a [] ~kind:(Select `One);
  tt "select greatest(1+2,10) where 1 = 2" a [] ~kind:(Select `Zero_one);
  tt "select 1 from test4" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4" a [] ~kind:(Select `Nat);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test"
    [attr "" Int; attr "" Text] [] ~kind:(Select `Nat);
  tt "select greatest(10,x) from test4" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y" a [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr "q" Int] [named "n", Int] ~kind:(Select `One);
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr "q" Int] [named "n", Int] ~kind:(Select `One);
]

let test_parsing = [
  tt "CREATE TABLE test5_1 (x INT NOT NULL, y INT DEFAULT -1) ENGINE=MEMORY" [] [];
  tt "SELECT 2+3, 2+-3, -10 FROM test5_1" [attr "" Int; attr "" Int; attr "" Int] [];
]

(*
  see MySQL 5.4 refman -- 12.2.8.1. JOIN Syntax
  see SQL:2008 -- 7.7 <joined table>
*)
let test_join_result_cols () =
  Tables.reset ();
  let ints = List.map (fun name -> attr name Int) in
  do_test "CREATE TABLE t1 (i INT, j INT)" [] [];
  do_test "CREATE TABLE t2 (k INT, j INT)" [] [];
  do_test "SELECT * FROM t1 JOIN t2 ON i=t1.j" (ints ["i";"j";"k";"j"]) [];
  do_test "SELECT * FROM t1 NATURAL JOIN t2" (ints ["j";"i";"k"]) [];
  do_test "SELECT * FROM t1 JOIN t2 USING (j)" (ints ["j";"i";"k"]) [];
(*   NATURAL JOIN with common column in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE j > @x"
    (ints ["j";"i";"k"])
    [named "x",Int];
(*   NATURAL JOIN with common column qualified in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x"
    (ints ["j";"i";"k"])
    [named "x",Int];
  ()

let test_misc () =
  let test =
    let printer = [%derive.show: int list] in
    fun x y z -> assert_equal ~printer (Schema.natural_ x y) z
  in
  test [1;2;3;4] [1;2;5;6] [1;2;3;4;5;6];
  test [1;2;3;4] [4;3;2;1] [1;2;3;4];
  test [1;2;3;4] [5;4;3;7;5;7] [3;4;1;2;5;7;5;7];
  test [1;2;3;4] [5;2;2] [2;1;3;4;5]; (* ?! *)
  ()

let test_enum = [
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" Text; attr "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" Text; attr "" Int] [];
]

let run () =
  let tests =
  [
    "simple" >::: test;
    "multi-table UPDATE" >::: test2;
    "gotchas" >::: test3;
    "single-row SELECT" >::: test4;
    "parsing" >::: test_parsing;
    "JOIN result columns" >:: test_join_result_cols;
    "misc" >:: test_misc;
    "enum" >::: test_enum;
  ]
  in
  let test_suite = "main" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
