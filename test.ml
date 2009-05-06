
open OUnit
open RA
open Sql.Type
open Stmt

(*
CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
-- [sql2cpp] name=select_all
SELECT * FROM test;
SELECT name,descr FROM test;
*)

let tt stmt scheme params =
  let print_scheme = RA.Scheme.to_string in
  let print_params = Stmt.params_to_string in
  let (s1,p1,_) =
    try
      Parser.parse_stmt stmt
    with
    | _ -> assert_failure "parsing failed"
  in
  assert_equal ~printer:print_scheme scheme s1;
  assert_equal ~printer:print_params params p1
      
let test () =
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)"
     []
     [];
  tt "SELECT str FROM test WHERE id=?"
     [attr "str" Text]
     [Next,Some Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+y LIMIT @lim"
     [attr "x" Text; attr "z" Int]
     [Next,Some Int; Named "id", Some Int; Named "lim",Some Int; ];
  tt "select test.name,other.name from test, test as other where test.id=other.id + @delta"
     [attr "name" Text; attr "name" Text]
     [Named "delta", Some Int];
  ()

let run () =
  let tests = 
  [
    "test" >:: test ;
  ]
  in
  let test_suite = "main" >::: tests in
  ignore (run_test_tt test_suite)
