module Q = Reusable.Sqlgg(Print_impl)
open Q.Pick

let run label src =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  select () ~before:"b" ~src ~after:5L (fun ~before:_ ~id:_ -> ())

let () =
  run "table" Src.table_archived;
  run "reusable query with one parameter" (Src.query_active_users ~status:"on");
  run "reusable query with two parameters" (Src.query_recent_users ~min_id:10L ~excluded:"off")
