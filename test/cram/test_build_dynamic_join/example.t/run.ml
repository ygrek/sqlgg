let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module Example = Example.Sqlgg(Print_impl)

let () =
  let open Example.User_info in
  run "brief: name + email -> no joins" (fun () ->
    ignore (List.select () (let+ n = name and+ e = email in (n, e)) ~org:1L));
  run "card: name + bio + avatar_url -> profiles only" (fun () ->
    ignore (List.select () (let+ n = name and+ b = bio and+ a = avatar_url in (n, b, a)) ~org:1L));
  run "brief + billing, no profile: name + plan -> billing only" (fun () ->
    ignore (List.select () (let+ n = name and+ pl = plan in (n, pl)) ~org:1L));
  run "admin: everything -> both joins" (fun () ->
    ignore (List.select ()
      (let+ n = name and+ b = bio and+ pl = plan and+ bal = balance in (n, b, pl, bal))
      ~org:1L))
