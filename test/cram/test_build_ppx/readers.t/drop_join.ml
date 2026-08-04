type bare = { iid : int64; tag : string option } [@@deriving sqlgg]

module Db = Shop.Sqlgg (Print_impl)

let () =
  let open Print_impl in
  let open Db.Wide in
  clear_mock_responses ();
  setup_select_response [ make_mock_row [ mock_int 1L; mock_text "red" ] ];
  Stdlib.List.iter
    (fun (r : bare) ->
      Printf.printf "iid=%Ld tag=%s\n" r.iid (match r.tag with None -> "none" | Some s -> s))
    (List.select () (bare_of_cols cols) ~min_id:0L)
