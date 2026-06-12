let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Key_shapes = Key_shapes.Sqlgg(Print_impl)

let () =
  let open Key_shapes.Unique_key_col in
  run "key_shapes/unique: pick id -> join dropped (UNIQUE key)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "key_shapes/unique: pick label -> join present" (fun () -> ignore (List.select () label ~uid:1L (fun x -> x)))

let () =
  let open Key_shapes.Composite_partial_col in
  run "key_shapes/composite_partial: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Key_shapes.Composite_full_col in
  run "key_shapes/composite_full: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "key_shapes/composite_full: pick title -> join present" (fun () -> ignore (List.select () title ~uid:1L (fun x -> x)))
