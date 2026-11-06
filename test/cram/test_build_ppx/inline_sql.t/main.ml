module Inline = Queries.Make (Print_impl)
module Cli = Cli_queries.Sqlgg (Print_impl)

let scenario ~find_user ~add_user ~rename_user ~titles_by ~publish =
  let open Print_impl in
  let show_user = function
    | None -> print_endline "find_user: no row"
    | Some (id, None) -> Printf.printf "find_user: %Ld, name is NULL\n" id
    | Some (id, Some name) -> Printf.printf "find_user: %Ld, name %s\n" id name
  in
  let show_affected label (r : execute_response) =
    Printf.printf "%s: %Ld row(s)\n" label r.affected_rows
  in
  clear_mock_responses ();
  setup_select_one_response (Some (make_mock_row [ mock_int 1L; mock_null ]));
  show_user (find_user () ~id:1L);
  setup_select_one_response (Some (make_mock_row [ mock_int 2L; mock_text "Ann" ]));
  show_user (find_user () ~id:2L);
  setup_execute_response ~affected_rows:1L ();
  show_affected "add_user" (add_user () ~id:3L ~name:None ~email:"bob@example.com");
  setup_execute_response ~affected_rows:1L ();
  show_affected "rename_user"
    (rename_user () ~name:(Some "Robert") ~nickname:None ~id:3L);
  setup_select_response
    [ make_mock_row [ mock_text "Hello"; mock_null ]
    ; make_mock_row [ mock_text "Again"; mock_text "bob" ]
    ];
  titles_by () ~author:3L ~state:`Published (fun ~title ~nickname ->
    Printf.printf "titles_by: %s by %s\n" title
      (Option.value nickname ~default:"<no nickname>"));
  setup_execute_response ~affected_rows:1L ();
  show_affected "publish" (publish () ~state:`Published ~id:7L)

let () =
  match Sys.argv with
  | [| _; "inline" |] ->
    scenario ~find_user:Inline.find_user ~add_user:Inline.add_user
      ~rename_user:Inline.rename_user ~titles_by:Inline.titles_by
      ~publish:Inline.publish
  | [| _; "cli" |] ->
    scenario ~find_user:Cli.find_user ~add_user:Cli.add_user
      ~rename_user:Cli.rename_user ~titles_by:Cli.titles_by
      ~publish:Cli.publish
  | _ -> prerr_endline "usage: main.exe (inline|cli)"
