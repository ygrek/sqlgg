module S = Multi.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

let () =
  let open S.Two_indep in
  run "two_indep: pick id (no joins)" (fun () -> ignore (List.select () id ~uid:1L));
  run "two_indep: pick bio (profiles only)" (fun () -> ignore (List.select () bio ~uid:1L));
  run "two_indep: pick url (avatars only)" (fun () -> ignore (List.select () url ~uid:1L));
  run "two_indep: pick bio+url (both)" (fun () ->
    ignore (List.select () (let+ b = bio and+ u = url in (b, u)) ~uid:1L))

let () =
  let open S.Subq_in_on in
  run "subq_in_on: pick id -> everything kept (subquery in another join's ON)" (fun () -> ignore (List.select () id ~uid:1L))

let () =
  let open S.Same_twice in
  run "same_twice: pick bio1 (p1 only)" (fun () -> ignore (List.select () bio1 ~uid:1L));
  run "same_twice: pick bio2 (p2 only)" (fun () -> ignore (List.select () bio2 ~uid:1L));
  run "same_twice: pick both" (fun () ->
    ignore (List.select () (let+ a = bio1 and+ b = bio2 in (a, b)) ~uid:1L))
