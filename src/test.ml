
open OUnit
open RA
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

let tt ?msg sql ?kind schema params =
  let msg = Option.default sql msg in
  match Main.parse_one (sql,[]) with
  | None -> assert_failure "Failed to parse"
  | Some stmt ->
      assert_equal ~msg ~printer:RA.Schema.to_string schema stmt.schema;
      assert_equal ~msg ~cmp:cmp_params ~printer:Stmt.params_to_string params stmt.params;
      match kind with 
      | Some k -> assert_equal ~msg ~printer:Show.show<Stmt.kind> k stmt.kind
      | None -> ()

let wrong sql =
  ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one_exn (sql,[])); false with _ -> true)

(* let wrong sql = assert_equal None (Main.parse_one (sql,[])) *)

let test () =
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
    [attr "str" Text; attr "q" Any] (* FIXME `Func *)
    [p "id" Int; p "id" Int; p "def" Any];
  wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr "str" Text;]
    [p "id" Int; p "x" Int; p "id" Int];
  ()

let test2 () =
  tt "CREATE TABLE test2 (id INT, str TEXT)" [] [];
  tt    "update test, (select * from test2) as x set str = x.str where test.id=x.id" [] [];
  tt    "update test, (select * from test2) as x set name = x.str where test.id=x.id" [] [];
  tt    "update test, (select * from test2) as x set test.str = x.str where test.id=x.id" [] [];
  wrong "update test, (select * from test2) as x set test.name = x.name where test.id=x.id";
  wrong "update test, (select * from test2) as x set test.str = str where test.id=x.id";
  ()

let test3 () =
  tt "SELECT id FROM test WHERE str IN ( SELECT str FROM test2 )" [attr "id" Int] [];
  todo "tuples";
  (* from http://stackoverflow.com/questions/1063866/sql-portability-gotchas/1063946#1063946 *)
  tt "SELECT id FROM test WHERE (id, str) IN ( SELECT id, str FROM test2)" [attr "id" Int] [];
  ()

let test4  () =
  let a = [attr "" Int] in
  tt "CREATE TABLE test4 (x INT, y INT)" [] [];
  tt "select max(*) from test4" a [] ~kind:(Select `One);
  tt "select max(x) as q from test4" [attr "q" Int] [] ~kind:(Select `One);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4 limit 1" a [] ~kind:(Select `Zero_one);
  tt "select max(x,y) from test4 limit 2" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4" a [] ~kind:(Select `Zero_one);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test" 
    [attr "" Int; attr "" Text] [] ~kind:(Select `Zero_one);
  tt "select greatest(10,x) from test4" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y" a [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr "q" Int] [named "n", Int] ~kind:(Select `One);
  todo "single row";
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr "q" Int] [named "n", Int] ~kind:(Select `One);
  ()

let test_parsing () =
  tt "CREATE TABLE test5_1 (x INT NOT NULL, y INT DEFAULT -1) ENGINE=MEMORY" [] [];
  tt "SELECT 2+3, 2+-3, -10 FROM test5_1" [attr "" Int; attr "" Int; attr "" Int] [];
  ()

(*
  see MySQL 5.4 refman -- 12.2.8.1. JOIN Syntax
  see SQL:2008 -- 7.7 <joined table>
*)
let test_join_result_cols () =
  Tables.reset ();
  let ints = List.map (fun name -> attr name Int) in
  tt "CREATE TABLE t1 (i INT, j INT)" [] [];
  tt "CREATE TABLE t2 (k INT, j INT)" [] [];
  tt "SELECT * FROM t1 JOIN t2 ON i=t1.j" (ints ["i";"j";"k";"j"]) [];
  tt ~msg:"NATURAL JOIN"
    "SELECT * FROM t1 NATURAL JOIN t2" (ints ["j";"i";"k"]) [];
  tt ~msg:"JOIN USING"
    "SELECT * FROM t1 JOIN t2 USING (j)" (ints ["j";"i";"k"]) [];
  tt ~msg:"NATURAL JOIN with common column in WHERE"
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE j > @x" 
    (ints ["j";"i";"k"]) 
    [named "x",Int];
  tt ~msg:"NATURAL JOIN with common column qualified in WHERE"
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x" 
    (ints ["j";"i";"k"]) 
    [named "x",Int];
  ()

let test_misc () =
  let test =
    let printer = Show.show<int list> in
    fun x y z -> assert_equal ~printer (RA.Schema.natural_ x y) z
  in
  test [1;2;3;4] [1;2;5;6] [1;2;3;4;5;6];
  test [1;2;3;4] [4;3;2;1] [1;2;3;4];
  test [1;2;3;4] [5;4;3;7;5;7] [3;4;1;2;5;7;5;7];
  test [1;2;3;4] [5;2;2] [2;1;3;4;5]; (* ?! *)
  ()

let test_enum () =
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" Text; attr "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" Text; attr "" Int] [];
  ()

let run () =
  let tests =
  [
    "simple" >:: test;
    "multi-table UPDATE" >:: test2;
    "gotchas" >:: test3;
    "single-row SELECT" >:: test4;
    "parsing" >:: test_parsing;
    "JOIN result columns" >:: test_join_result_cols;
    "misc" >:: test_misc;
    "enum" >:: test_enum;
  ]
  in
  let test_suite = "main" >::: tests in
  ignore (run_test_tt test_suite)
