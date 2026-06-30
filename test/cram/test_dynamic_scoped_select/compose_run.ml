open Print_ocaml_impl
module Db = Output.Sqlgg(Print_ocaml_impl)
open Db

type who = { id : int64; name : string option } [@@deriving sqlgg]
type labeling = { category : string option } [@@deriving sqlgg]

let () =
  clear_mock_responses ();
  setup_select_one_response
    (Some (make_mock_row [ mock_int 7L; mock_text "widget"; mock_text "tools" ]));
  let frag =
    Dscope_q1.(
      let+ w = who_of_cols cols
      and+ l = labeling_of_cols cols in
      (w, l))
  in
  match Dscope_q1.select () frag ~id:7L with
  | Some (w, l) ->
    Printf.printf "COMPOSED id=%Ld name=%s category=%s\n"
      w.id
      (Option.value ~default:"-" w.name)
      (Option.value ~default:"-" l.category)
  | None -> print_endline "none"
