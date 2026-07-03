let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Subquery_sources = Subquery_sources.Sqlgg(Print_impl)

let () =
  let open Subquery_sources.Join_subq_source_col in
  run "subquery_sources/plain: pick id -> join kept (subquery source)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_join_dup_col in
  run "subquery_sources/cross_dup: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_union_dup_col in
  run "subquery_sources/union_dup: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_base_join_col in
  run "subquery_sources/subq_base: pick id -> table join dropped" (fun () -> ignore (List.select () id (fun x -> x)));
  run "subquery_sources/subq_base: pick bio -> table join present" (fun () -> ignore (List.select () bio (fun x -> x)))
