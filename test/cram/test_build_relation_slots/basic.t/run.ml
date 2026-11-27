module Q = Basic.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  f ()

let one n = Print_impl.setup_select_one_response (Some (Print_impl.make_mock_row [ Print_impl.mock_int n ]))

let () =
  run "count_from users" (fun () -> one 3L; ignore (Q.Count_from.select () ~source:Q.Count_from.Source.table_users));
  run "count_from logs" (fun () -> one 4L; ignore (Q.Count_from.select () ~source:Q.Count_from.Source.table_logs));
  run "select_id drafts: nullable id" (fun () ->
    Print_impl.setup_select_response [];
    Q.Select_id.select () ~source:Q.Select_id.Source.table_drafts (fun ~id -> ignore (id : int64 option)));
  run "select_id external: text id" (fun () ->
    Print_impl.setup_select_response [];
    ignore (Q.Select_id.List.select () ~source:Q.Select_id.Source.table_external (fun ~id -> (id : string))));
  run "count_by_id users" (fun () -> one 1L; ignore (Q.Count_by_id.select () ~source:Q.Count_by_id.Source.table_users ~id:7L));
  run "count_by_id external" (fun () -> one 1L; ignore (Q.Count_by_id.select () ~source:Q.Count_by_id.Source.table_external ~id:"x"))
