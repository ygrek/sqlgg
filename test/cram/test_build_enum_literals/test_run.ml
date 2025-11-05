(* test_run.ml - Test that Enum.to_literal adds quotes *)

open Printf

module M (T: Sqlgg_traits.M with 
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct
  
  module Sql = Output.Sqlgg(T)
  
  let test_where_in connection =
    printf "[TEST 1] Testing Enum in WHERE IN clause\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Failed"]
    ];
    
    let result = Sql.test1 ~status_list:[`Failed; `Skipped] connection (fun ~status:_ ->
      printf "  -> Callback executed\n"
    ) in
    printf "[TEST 1] Completed\n\n";
    result
  
  let test_insert_values connection =
    printf "[TEST 2] Testing Enum in INSERT VALUES\n";
    
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:2L ~insert_id:(Some 1L) ();
    
    let result = Sql.test2 ~rows:[(1L, `Active); (2L, `Pending)] connection in
    printf "[TEST 2] Completed: affected_rows=%Ld\n\n" result.affected_rows;
    result
  
  let run_all_tests connection =
    printf "=== Starting Enum.to_literal Tests ===\n\n";
    
    try
      let _ = test_where_in connection in
      let _ = test_insert_values connection in
      
      printf "=== All Tests Completed Successfully ===\n";
      ()
    with
    | exn ->
      printf "\n=== Test Failed: %s ===\n" (Printexc.to_string exn);
      raise exn
end

module Test = M(Print_ocaml_impl)

let () = 
  let con = () in
  
  printf "Starting Enum Literal Tests\n";
  printf "%s\n" (String.make 60 '=');
  
  Test.run_all_tests con;
  
  printf "\n%s\n" (String.make 60 '=');
  printf "All tests executed successfully!\n"

