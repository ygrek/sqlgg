type stock = {
  sid : int64;
  place : string;
  hits : int64; [@sqlgg.default 0L]
}
[@@deriving sqlgg ~nullable_cols]

type listing = {
  iid : int64;
  stock : stock option; [@sqlgg.nested]
  tag : string option;
}
[@@deriving sqlgg]

type listing_or_none = {
  iid : int64;
  stock : stock option; [@sqlgg.nested default_none]
  tag : string option;
}
[@@deriving sqlgg]

module Db = Shop.Sqlgg (Print_impl)

let row ~iid ~sid ~place ~hits ~tag =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response
    [ Print_impl.make_mock_row [ iid; sid; place; hits; tag ] ]

let shown = function
  | None -> "none"
  | Some s -> Printf.sprintf "%Ld/%s/%Ld" s.sid s.place s.hits

let read one = try shown (one ()) with Failure w -> "raises " ^ w

let () =
  let open Print_impl in
  let open Db.Wide in
  let plain () =
    (Stdlib.List.hd (List.select () (listing_of_cols cols) ~min_id:0L)).stock
  in
  let soft () =
    (Stdlib.List.hd (List.select () (listing_or_none_of_cols cols) ~min_id:0L)).stock
  in
  let line = Printf.printf "| %-22s | %-34s | %s\n%!" in
  let case name cells =
    cells ();
    let p = read plain in
    cells ();
    let s = read soft in
    line name p s
  in
  line "row" "[@sqlgg.nested]" "default_none";
  case "matched" (fun () ->
    row ~iid:(mock_int 1L) ~sid:(mock_int 7L) ~place:(mock_text "shelf")
      ~hits:(mock_int 3L) ~tag:(mock_text "red"));
  case "no match" (fun () ->
    row ~iid:(mock_int 2L) ~sid:mock_null ~place:mock_null ~hits:mock_null
      ~tag:mock_null);
  case "half a relation" (fun () ->
    row ~iid:(mock_int 3L) ~sid:(mock_int 7L) ~place:mock_null ~hits:(mock_int 5L)
      ~tag:(mock_text "blue"));
  case "defaulted column NULL" (fun () ->
    row ~iid:(mock_int 4L) ~sid:(mock_int 8L) ~place:(mock_text "bin") ~hits:mock_null
      ~tag:(mock_text "x"))
