let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Outside_refs = Outside_refs.Sqlgg(Print_impl)

let () =
  let open Outside_refs.Ref_in_group_col in
  run "outside_refs/group: pick id -> join kept (GROUP BY)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Ref_in_order_col in
  run "outside_refs/order: pick id -> join kept (ORDER BY)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Ref_in_having_col in
  run "outside_refs/having: pick id -> join kept (HAVING)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Complex_proj_col in
  run "outside_refs/complex_proj: pick id -> join kept (complex expr)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Outside_refs.Subq_in_where_col in
  run "outside_refs/subq_in_where: pick id -> join kept (subquery in WHERE)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Unqualified_where_col in
  run "outside_refs/unqualified: pick id -> join kept (unqualified ref)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Join_unreferenced_col in
  run "outside_refs/unreferenced: pick id -> join rendered statically" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))
