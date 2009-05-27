
open OUnit
open RA
open Sql.Type
open Stmt

let tt ?msg stmt scheme params =
  let print_scheme = RA.Scheme.to_string in
  let print_params = Stmt.params_to_string in
  let (s1,p1,_) =
    try
      Parser.parse_stmt stmt
    with
    | _ -> assert_failure "tt failed"
  in
  assert_equal ?msg ~printer:print_scheme scheme s1;
  assert_equal ?msg ~printer:print_params params p1

let test () =
  tt "CREATE TABLE test (id INT, str TEXT, name TEXT)"
     []
     [];
  tt "SELECT str FROM test WHERE id=?"
     [attr "str" Text]
     [Next,Some Int];
  tt "SELECT x,y+? AS z FROM (SELECT id AS y,CONCAT(str,name) AS x FROM test WHERE id=@id*2) ORDER BY x,x+z LIMIT @lim"
     [attr "x" Text; attr "z" Int]
     [Next,Some Int; Named "id", Some Int; Named "lim",Some Int; ];
  tt "select test.name,other.name as other_name from test, test as other where test.id=other.id + @delta"
     [attr "name" Text; attr "other_name" Text]
     [Named "delta", Some Int];
  tt "select test.name from test where test.id = @x + ? or test.id = @x - ?"
     [attr "name" Text;]
     [Named "x", Some Int; Next, Some Int; Named "x", Some Int; Next, Some Int;];
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
    [Named "x",Some Int];
  tt ~msg:"NATURAL JOIN with common column qualified in WHERE"
    "SELECT * FROM t1 NATURAL JOIN t2 WHERE t2.j > @x" 
    (ints ["j";"i";"k"]) 
    [Named "x",Some Int];
  ()

let test_misc () =
  let test =
    let printer = Show.show<int list> in
    fun x y z -> assert_equal ~printer (RA.Scheme.natural_ x y) z
  in
  test [1;2;3;4] [1;2;5;6] [1;2;3;4;5;6];
  test [1;2;3;4] [4;3;2;1] [1;2;3;4];
  test [1;2;3;4] [5;4;3;7;5;7] [3;4;1;2;5;7;5;7];
  test [1;2;3;4] [5;2;2] [2;1;3;4;5]; (* ?! *)
  ()

let run () =
  let tests =
  [
    "simple" >:: test;
    "JOIN result columns" >:: test_join_result_cols;
    "misc" >:: test_misc;
  ]
  in
  let test_suite = "main" >::: tests in
  ignore (run_test_tt test_suite)
