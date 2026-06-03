(* test_run.ml - show the final, resolved SQL string for a choice branch,
   demonstrating that the branch body is wrapped in parentheses. *)

open Printf

module M (T: Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)

  let run connection =
    (* No rows: FALSE AND (FALSE OR TRUE) is FALSE, but we only care about the
       resolved SQL string printed by the mock, not the result. *)
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response None;
    let _ = Sql.poc_sqlgg_no_parens ~b:`Some connection in

    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response None;
    let _ = Sql.poc_sqlgg_with_parens ~b:`Some connection in
    ()
end

module Test = M(Print_ocaml_impl)

let () = Test.run ()
