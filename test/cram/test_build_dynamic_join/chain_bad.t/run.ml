let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Chain_bad = Chain_bad.Sqlgg(Print_impl)

let () =
  let open Chain_bad.Chain_bad_col in
  run "chain_bad: pick id -> both joins kept (child pins parent)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))
