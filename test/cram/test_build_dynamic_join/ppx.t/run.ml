type item = { id : int64; name : string option } [@@deriving sqlgg]
type described = { descr : string option } [@@deriving sqlgg]
type stat = { sold : int64 option } [@@deriving sqlgg]

module Db = Ppx_je.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

let () =
  let open Db.Wide in
  run "wide: item record -> both joins dropped" (fun () ->
    ignore (List.select () (item_of_cols cols) ~min_id:0L));
  run "wide: item + stat -> only stats join" (fun () ->
    ignore (List.select ()
      (let+ i = item_of_cols cols
       and+ s = stat_of_cols cols in
       (i, s))
      ~min_id:0L));
  run "wide: described -> only details join" (fun () ->
    ignore (List.select () (described_of_cols cols) ~min_id:0L));
  run "wide: item + described + stat -> both joins" (fun () ->
    ignore (List.select ()
      (let+ i = item_of_cols cols
       and+ d = described_of_cols cols
       and+ s = stat_of_cols cols in
       (i, d, s))
      ~min_id:0L))

let () =
  let open Db.Narrow in
  run "narrow: same item record, different query" (fun () ->
    ignore (List.select () (item_of_cols cols) ~min_id:0L))
