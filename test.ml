
open OUnit
open RA.Scheme
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
  let (s1,p1) =
    try
      Parser.parse_stmt stmt
    with
    | _ -> assert_failure "parsing failed"
  in
  assert_equal ~printer:print_scheme scheme s1;
  assert_equal ~printer:print_params params p1
      
let test () =
  tt "CREATE TABLE q (id INT, str TEXT)"
     []
     [];
  tt "SELECT str FROM q WHERE id=?"
     [attr "str" Text]
     [Next,None];
  assert_equal 1 1

let run () =
  let tests = 
  [
    "test" >:: test ;
  ]
  in
  let test_suite = "main" >::: tests in
  ignore (run_test_tt test_suite)
