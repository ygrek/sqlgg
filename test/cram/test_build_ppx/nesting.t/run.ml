type cc = { cid : int64; cname : string } [@@deriving sqlgg ~nullable_cols]

type deep = {
  bid : int64;
  bname : string;
  cc : cc option; [@sqlgg.nested]
}
[@@deriving sqlgg ~nullable_cols]

type chained = { aid : int64; deep : deep option [@sqlgg.nested] }
[@@deriving sqlgg]

type lax_chain = { aid : int64; deep : deep option [@sqlgg.nested default_none] }
[@@deriving sqlgg]

type inner_nested = { bid : int64; cc : cc [@sqlgg.nested] }
[@@deriving sqlgg ~nullable_cols]

type nested_under_option = {
  aid : int64;
  b : inner_nested option; [@sqlgg.nested]
}
[@@deriving sqlgg]

type nested_under_option_lax = {
  aid : int64;
  b : inner_nested option; [@sqlgg.nested default_none]
}
[@@deriving sqlgg]

module Db = Joins.Sqlgg (Print_impl)

let row ~aid ~bid ~bname ~cid ~cname =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response
    [ Print_impl.make_mock_row [ aid; bid; bname; cid; cname ] ]

let leaf = function None -> "none" | Some c -> Printf.sprintf "%Ld/%s" c.cid c.cname

let shown : deep option -> string = function
  | None -> "none"
  | Some b -> Printf.sprintf "%Ld/%s c=%s" b.bid b.bname (leaf b.cc)

let read one = try shown (one ()) with Failure w -> "raises " ^ w

let () =
  let open Print_impl in
  let open Db.Chain in
  let strict () =
    (Stdlib.List.hd (List.select () (chained_of_cols cols) ~min_id:0L)).deep
  in
  let lax () =
    (Stdlib.List.hd (List.select () (lax_chain_of_cols cols) ~min_id:0L)).deep
  in
  let pair name a b = Printf.printf "| %-19s | %-30s | %s\n%!" name a b in
  let case2 name cells read one two =
    cells ();
    let a = read one in
    cells ();
    let b = read two in
    pair name a b
  in
  let case name cells = case2 name cells read strict lax in
  pair "row" "[@sqlgg.nested]" "default_none";
  case "all present" (fun () ->
    row ~aid:(mock_int 1L) ~bid:(mock_int 2L) ~bname:(mock_text "bb")
      ~cid:(mock_int 3L) ~cname:(mock_text "cc"));
  case "leaf absent" (fun () ->
    row ~aid:(mock_int 1L) ~bid:(mock_int 2L) ~bname:(mock_text "bb") ~cid:mock_null
      ~cname:mock_null);
  case "middle absent" (fun () ->
    row ~aid:(mock_int 1L) ~bid:mock_null ~bname:mock_null ~cid:mock_null
      ~cname:mock_null);
  case "half a leaf" (fun () ->
    row ~aid:(mock_int 1L) ~bid:(mock_int 2L) ~bname:(mock_text "bb")
      ~cid:(mock_int 3L) ~cname:mock_null);
  let plain () =
    (Stdlib.List.hd (List.select () (nested_under_option_of_cols cols) ~min_id:0L)).b
  in
  let soft () =
    (Stdlib.List.hd (List.select () (nested_under_option_lax_of_cols cols) ~min_id:0L)).b
  in
  let inner : inner_nested option -> string = function
    | None -> "none"
    | Some b -> Printf.sprintf "%Ld c=%Ld" b.bid b.cc.cid
  in
  let read one = try inner (one ()) with Failure w -> "raises " ^ w in
  case2 "plain child, absent" (fun () ->
    row ~aid:(mock_int 1L) ~bid:mock_null ~bname:mock_null ~cid:mock_null
      ~cname:mock_null)
    read plain soft
