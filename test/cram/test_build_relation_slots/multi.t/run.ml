module Q = Multi.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  f ()

let () =
  run "pair users teams" (fun () ->
    Print_impl.setup_select_one_response (Some (Print_impl.make_mock_row [ Print_impl.mock_int 2L ]));
    ignore (Q.Pair.select () ~left_src:Q.Pair.Left_src.table_users ~right_src:Q.Pair.Right_src.table_teams));
  run "tree over tags" (fun () ->
    Print_impl.setup_select_response [];
    Q.Tree.select () ~src:Q.Tree.Src.table_tags (fun ~id -> ignore (id : string)));
  run "correlated EXISTS" (fun () ->
    Print_impl.setup_select_response [];
    Q.Active.select () ~ev:Q.Active.Ev.table_events (fun ~id:_ -> ()))
