open Print_ocaml_impl
module Db = Output.Sqlgg(Print_ocaml_impl)
open Db

type item = { id : int64; color : Color.t } [@@deriving sqlgg]

let () =
  clear_mock_responses ();
  setup_select_response
    [ make_mock_row [ mock_int 7L; mock_text "green" ];
      make_mock_row [ mock_int 8L; mock_text "blue" ] ];
  Db.Items_scope.(select () (item_of_cols cols))
    (fun it -> Printf.printf "DECODED id=%Ld color=%s\n" it.id (Color.to_string it.color))
