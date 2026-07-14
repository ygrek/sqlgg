open Print_ocaml_impl
module Db = Output.Sqlgg(Print_ocaml_impl)
open Db

type who = { id : User_id.t; name : string } [@@deriving sqlgg]

type ident = { id : User_id.t } [@@deriving sqlgg]
type naming = { name : string } [@@deriving sqlgg]

let ident_and_naming cols =
  let open Sqlgg_scope in
  let+ i = ident_of_cols cols
  and+ n = naming_of_cols cols in
  (i, n)

let show tag (w : who) =
  Printf.printf "%s who: id=%Ld name=%s\n" tag (User_id.to_int64 w.id) w.name

let show_pair tag ((i : ident), (n : naming)) =
  Printf.printf "%s pair: id=%Ld name=%s\n" tag (User_id.to_int64 i.id) n.name

let () =
  clear_mock_responses ();
  setup_select_one_response
    (Some (make_mock_row [ mock_int 1L; mock_text "alice" ]));
  begin match Get_user.(select () (who_of_cols cols)
          ~id:(User_id.get_column 1L)) with
  | Some w -> show "Q1" w
  | None -> print_endline "Q1 none"
  end;
  setup_select_response
    [ make_mock_row [ mock_int 7L; mock_text "bob" ];
      make_mock_row [ mock_int 8L; mock_text "carol" ] ];
  List_admins.(select () (who_of_cols cols) (show "Q2"));
  setup_select_one_response
    (Some (make_mock_row [ mock_int 3L; mock_text "dave" ]));
  begin match Get_user.(select () (ident_and_naming cols) ~id:(User_id.get_column 3L)) with
  | Some p -> show_pair "Q3" p
  | None -> print_endline "Q3 none"
  end;
  setup_select_response [ make_mock_row [ mock_int 9L; mock_text "erin" ] ];
  List_admins.(select () (ident_and_naming cols) (show_pair "Q4"))
