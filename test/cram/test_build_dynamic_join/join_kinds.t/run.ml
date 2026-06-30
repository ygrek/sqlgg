let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Join_kinds = Join_kinds.Sqlgg(Print_impl)

let () =
  let open Join_kinds.Inner_join in
  run "join_kinds/inner: pick id -> join kept (INNER)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Join_using in
  run "join_kinds/using: pick id -> join kept (USING)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Join_natural in
  run "join_kinds/natural: pick id -> join kept (NATURAL)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Using_after_candidate in
  run "join_kinds/using_after: pick id -> candidate kept (later USING)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Natural_after_candidate in
  run "join_kinds/natural_after: pick id -> candidate kept (later NATURAL)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Right_join in
  run "join_kinds/right_join: pick id -> join kept (RIGHT)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Comma_join in
  run "join_kinds/comma_join: pick id -> join kept (comma join)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Implicit_before_candidate in
  run "join_kinds/implicit_before: pick id -> join dropped (USING before candidate)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "join_kinds/implicit_before: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))
