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
    (List.map (function Single p -> p | SingleIn _ | Choice _ | ChoiceIn _ | TupleList _ -> assert false) stmt.vars);
  match kind with
  | Some k -> assert_equal ~msg:"kind" ~printer:[%derive.show: Stmt.kind] k stmt.kind
  | None -> ()

let tt sql ?kind schema params =
  let test () = do_test sql ?kind schema params in
  sql >:: test

let wrong sql =
  sql >:: (fun () -> ("Expected error in : " ^ sql) @? (try ignore (Main.parse_one' (sql,[])); false with _ -> true))

let attr ?(extra=[]) n d = make_attribute n (Some d) (Constraints.of_list extra)
let named s t = new_param { label = Some s; pos = (0,0) } (Type.strict t)
let named_nullable s t = new_param { label = Some s; pos = (0,0) } (Type.nullable t)
let param t = new_param { label = None; pos = (0,0) } (Type.strict t)

let test = Type.[
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)" [] [];
  tt "SELECT str FROM test WHERE id=?"
     [attr "str" Text]
     [param  Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr "x" Text; attr "z" Int]
     [param Int; named "id" Int; named "lim" Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [attr "name" Text; attr "other_name" Text]
     [named "delta" Int];
  tt "select test.name from test where test.id + @x = ? or test.id - @x = ?"
     [attr "name" Text;]
     [named "x" Int; param Int; named "x" Int; param Int;];
  tt "insert into test values"
     []
     [named "id" Int; named "str" Text; named "name" Text];
  tt "insert into test (str,name) values"
     []
     [named "str" Text; named "name" Text];
  tt "insert into test values (2,'hello' || ' world',@name)"
     []
     [named "name" Text];
  tt "insert or replace into test values (2,?,?)" [] [param Text; param Text;];
  tt "replace into test values (2,?,?)" [] [param Text; param Text;];
  tt "select str, case when id > @id then name when id < @id then 'qqq' else @def end as q from test"
    [attr "str" Text; attr "q" Text]
    [named "id" Int; named "id" Int; named "def" Text];
  wrong "insert into test values (1,2)";
  wrong "insert into test (str,name) values (1,'str','name')";
  (* check precedence of boolean and arithmetic operators *)
  tt "select str from test where id>=@id and id-@x<@id"
    [attr "str" Text;]
    [named "id" Int; named "x" Int; named "id" Int];
  tt "select 3/5"
    [attr "" Float;]
    [];
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
  tt "select max(x) as q from test4" [attr "q" Int] [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x) from test4" a [] ~kind:(Select `One);
  tt "select max(x+y) from test4 limit 1" a [] ~kind:(Select `One);
  tt "select max(y) from test4 limit 2" a [] ~kind:(Select `One);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4" a [] ~kind:(Select `Nat);
  tt "select max(x,y) from test4 limit 1" a [] ~kind:(Select `Zero_one);
  tt "select max(x,y) from test4 limit 2" a [] ~kind:(Select `Nat);
  tt "select 1" a [] ~kind:(Select `One);
  tt "select greatest(1+2,10)" a [] ~kind:(Select `One);
  tt "select greatest(1+2,10) where 1 = 2" a [] ~kind:(Select `Zero_one);
  tt "select 1 from test4" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4" a [] ~kind:(Select `Nat);
  tt "select least(10+unix_timestamp(),random()), concat('test',upper('qqqq')) from test"
    [attr "" Int; attr "" Text] [] ~kind:(Select `Nat);
  tt "select greatest(10,x) from test4" a [] ~kind:(Select `Nat);
  tt "select 1+2 from test4 where x=y" a [] ~kind:(Select `Nat);
  tt "select max(x) as q from test4 where y = x + @n" [attr "q" Int] [named "n" Int] ~kind:(Select `One);
  tt "select coalesce(max(x),0) as q from test4 where y = x + @n" [attr "q" Int] [named "n" Int] ~kind:(Select `One);
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
  let ints = List.map (fun name ->
    if String.ends_with name "?" then
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
    [named "x" Int];
(*   NATURAL JOIN with common column qualified in WHERE *)
  do_test
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x"
    (ints ["j";"i";"k"])
    [named "x" Int];
  ()

let test_enum = [
  tt "CREATE TABLE test6 (x enum('true','false') COLLATE utf8_bin NOT NULL, y INT DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test6" [attr "x" Text ~extra:[NotNull]; attr "y" Int] [];
  tt "SELECT x, y+10 FROM test6" [attr "x" Text ~extra:[NotNull]; attr "" Int] [];
]

let test_manual_param = [
  tt "CREATE TABLE test7 (x INT NULL DEFAULT 0) ENGINE=MyISAM DEFAULT CHARSET=utf8" [] [];
  tt "SELECT * FROM test7 WHERE x = @x_arg" [attr "x" Int ~extra:[Null];] [
    named_nullable "x_arg" Int
  ];
  tt "SELECT * FROM test7 WHERE x = @x_arg::Int" [attr "x" Int ~extra:[Null];] [
    named "x_arg" Int
  ];
  tt "INSERT INTO test7 VALUES (@x_arg)" [] [
    named_nullable "x_arg" Int
  ];
  tt "UPDATE test7 SET x = @x_arg WHERE x = @x_arg_2" [] [
    named_nullable "x_arg" Int;
    named_nullable "x_arg_2" Int
  ];
  tt "UPDATE test7 SET x = @x_arg ::Int WHERE x = @x_arg_2 :: Int" [] [
    named "x_arg" Int;
    named "x_arg_2" Int
  ];
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
  ]
  in
  let test_suite = "main" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
