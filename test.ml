
open OUnit
open RA
open Sql.Type
open Stmt.Raw

(*
CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
-- [sql2cpp] name=select_all
SELECT * FROM test;
SELECT name,descr FROM test;
*)

let tt stmt scheme params =
  let print_scheme = RA.Scheme.to_string in
  let print_params = Stmt.Raw.to_string in
  let (s1,p1,_) =
    try
      Parser.parse_stmt stmt
    with
    | _ -> assert_failure "parsing failed"
  in
  assert_equal ~printer:print_scheme scheme s1;
  assert_equal ~printer:print_params params p1
      
let test () =
  tt "CREATE TABLE q (id INT, str TEXT, name TEXT)"
     []
     [];
  tt "SELECT str FROM q WHERE id=?"
     [attr "str" Text]
     [Next,None];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM q WHERE id=@id*2) ORDER BY x,x+y LIMIT @lim"
     [attr "x" Text; attr "z" Text]
     [Next,None; Named "id", None; Named "lim",Some Int; ]

let run () =
  let tests = 
  [
    "test" >:: test ;
  ]
  in
  let test_suite = "main" >::: tests in
  ignore (run_test_tt test_suite)
