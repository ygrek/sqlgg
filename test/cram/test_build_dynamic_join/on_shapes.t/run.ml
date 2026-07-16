let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module On_shapes = On_shapes.Sqlgg(Print_impl)

let () =
  let open On_shapes.Param_in_on in
  run "on_shapes/param_in_on: pick id -> join kept (param in ON)" (fun () -> ignore (List.select () id ~b:"x" ~uid:1L))

let () =
  let open On_shapes.Extra_const_on in
  run "on_shapes/extra_const_on: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L));
  run "on_shapes/extra_const_on: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L))

let () =
  let open On_shapes.On_inequality in
  run "on_shapes/inequality: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L))

let () =
  let open On_shapes.No_alias in
  run "on_shapes/no_alias: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L));
  run "on_shapes/no_alias: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L))

let () =
  let open On_shapes.Flipped_on in
  run "on_shapes/flipped_on: pick id -> join dropped (operand order does not matter)" (fun () -> ignore (List.select () id ~uid:1L));
  run "on_shapes/flipped_on: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L))

let () =
  let open On_shapes.Const_key_on in
  run "on_shapes/const_key_on: pick id -> join dropped (key equated to a constant)" (fun () -> ignore (List.select () id ~uid:1L));
  run "on_shapes/const_key_on: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L))

let () =
  let open On_shapes.Or_in_on in
  run "on_shapes/or_in_on: pick id -> join kept (OR can match many rows)" (fun () -> ignore (List.select () id ~uid:1L))

let () =
  let open On_shapes.Subq_own_on in
  run "on_shapes/subq_own_on: pick id -> join kept (subquery in own ON)" (fun () -> ignore (List.select () id ~uid:1L))
