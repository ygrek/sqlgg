let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Basic = Basic.Sqlgg(Print_impl)

let () =
  let open Basic.Ok in
  run "basic/ok: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "basic/ok: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))

let () =
  let open Basic.Nonuniq in
  run "basic/nonuniq: pick id -> join kept (non-unique key)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Basic.Ref_in_where in
  run "basic/ref_in_where: pick id -> join kept (WHERE reference)" (fun () -> ignore (List.select () id ~b:"x" (fun x -> x)))
