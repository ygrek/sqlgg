let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Whitespace = Whitespace.Sqlgg(Print_impl)

let () =
  let open Whitespace.Ws in
  run "ws: pick id -> both joins dropped, no gaps" (fun () ->
    ignore (List.select () id ~uid:1L));
  run "ws: pick id + bio -> profiles kept, billing gap collapsed" (fun () ->
    ignore (List.select () (let+ i = id and+ b = bio in (i, b)) ~uid:1L));
  run "ws: pick id + plan -> profiles gap collapsed, billing kept" (fun () ->
    ignore (List.select () (let+ i = id and+ p = plan in (i, p)) ~uid:1L));
  run "ws: pick all -> both joins, single spaces" (fun () ->
    ignore (List.select ()
      (let+ i = id and+ b = bio and+ p = plan in (i, b, p))
      ~uid:1L))
