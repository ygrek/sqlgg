open Print_ocaml_impl
module Db = Output.Sqlgg(Print_ocaml_impl)
open Db

type tinted = { id : int64; color : Color.t } [@@deriving sqlgg]
type named = { name : string option } [@@deriving sqlgg]

let show_tinted tag t =
  Printf.printf "%s id=%Ld color=%s\n" tag t.id (Color.to_string t.color)

let () =
  clear_mock_responses ();
  setup_select_one_response
    (Some (make_mock_row [ mock_int 7L; mock_text "green"; mock_text "gadget" ]));
  let frag =
    Tinted_q1.(
      let+ t = tinted_of_cols cols
      and+ n = named_of_cols cols in
      (t, n))
  in
  begin match Tinted_q1.select () frag ~id:7L with
  | Some (t, n) ->
    show_tinted "Q1" t;
    Printf.printf "Q1 name=%s\n" (Option.value ~default:"-" n.name)
  | None -> print_endline "Q1 none"
  end;
  setup_select_response [ make_mock_row [ mock_int 8L; mock_text "blue" ] ];
  Tinted_q2.(select () (tinted_of_cols cols) ~min_id:1L)
    (show_tinted "Q2")
