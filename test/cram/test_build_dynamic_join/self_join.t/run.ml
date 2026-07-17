let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Self_join = Self_join.Sqlgg(Print_impl)

let () =
  let open Self_join.Bad in
  run "self_join/bad: pick id -> join kept (non-unique self key)" (fun () -> ignore (List.select () id))

let () =
  let open Self_join.Good in
  run "self_join/good: pick id -> join dropped (PK self key)" (fun () -> ignore (List.select () id));
  run "self_join/good: pick name -> join present" (fun () -> ignore (List.select () name))
