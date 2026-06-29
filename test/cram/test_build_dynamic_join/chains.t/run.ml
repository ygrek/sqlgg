module S = Chains.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

let () =
  let open S.Chain_col in
  run "chain: pick id" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "chain: pick bio" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)));
  run "chain: pick url (pulls profiles transitively)" (fun () -> ignore (List.select () url ~uid:1L (fun x -> x)));
  run "chain: pick all" (fun () ->
    ignore (List.select () (let+ i = id and+ b = bio and+ u = url in (i, b, u)) ~uid:1L (fun x -> x)))

let () =
  let open S.Chain3_col in
  run "chain3: pick label (pulls the whole ancestor chain)" (fun () ->
    ignore (List.select () label ~uid:1L (fun x -> x)));
  run "chain3: pick url (badges not pulled)" (fun () ->
    ignore (List.select () url ~uid:1L (fun x -> x)))

let () =
  let open S.Diamond_col in
  run "diamond: pick url (one branch)" (fun () ->
    ignore (List.select () url ~uid:1L (fun x -> x)));
  run "diamond: pick label (other branch)" (fun () ->
    ignore (List.select () label ~uid:1L (fun x -> x)));
  run "diamond: pick both (parent emitted once)" (fun () ->
    ignore (List.select () (let+ u = url and+ l = label in (u, l)) ~uid:1L (fun x -> x)))

let () =
  let open S.Chain_pinned_col in
  run "chain_pinned: pick id (WHERE pins the whole chain, no joins dropped)" (fun () ->
    ignore (List.select () id ~label:"x" (fun x -> x)))
