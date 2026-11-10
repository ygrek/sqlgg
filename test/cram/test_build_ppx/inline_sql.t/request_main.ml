
module Via_mock = Sqlgg_request.Make (Print_impl)
module Via_custom = Sqlgg_request.Make (Custom_driver)

let run_select request =
  Print_impl.setup_select_response [];
  Via_mock.iter () request ignore

let () =
  print_endline "-- Print_impl: Int = int64, Text = string, IO = identity";
  let open Print_impl in
  clear_mock_responses ();
  setup_select_one_response (Some (make_mock_row [ mock_int 1L; mock_null ]));
  (match Via_mock.find_opt () (Requests.find_user ~id:1L) with
   | None -> print_endline "find_user: no row"
   | Some (id, None) -> Printf.printf "find_user: %Ld, name is NULL\n" id
   | Some (id, Some name) -> Printf.printf "find_user: %Ld, name %s\n" id name);
  setup_execute_response ~affected_rows:1L ();
  let response = Via_mock.execute () (Requests.rename_user ~name:(Some "Ann") ~id:1L) in
  Printf.printf "rename_user: %Ld row(s)\n" response.affected_rows

let () =
  print_endline "-- Custom_driver: Int = int, Text = Bytes.t, IO = result";
  let db = ref [ [| "2"; "Bob" |] ] in
  (match Via_custom.find_opt db (Requests.find_user ~id:2) with
   | Ok (Some (id, Some name)) -> Printf.printf "find_user: %d, name %s\n" id (Bytes.to_string name)
   | Ok (Some (id, None)) -> Printf.printf "find_user: %d, name is NULL\n" id
   | Ok None -> print_endline "find_user: no row"
   | Error message -> print_endline ("find_user failed: " ^ message));
  match Via_custom.execute db (Requests.rename_user ~name:(Some (Bytes.of_string "Robert")) ~id:2) with
  | Ok response -> Printf.printf "rename_user: %Ld row(s)\n" response.affected_rows
  | Error message -> print_endline ("rename_user failed: " ^ message)

let () =
  print_endline "-- Print_impl: 12 columns, 12 parameters";
  let open Print_impl in
  clear_mock_responses ();
  setup_execute_response ~affected_rows:1L ();
  ignore (Via_mock.execute () (Requests.insert_wide ~c1:1L ~c2:2L ~c3:3L ~c4:4L ~c5:5L ~c6:6L ~c7:7L ~c8:8L ~c9:9L ~c10:10L ~c11:11L ~c12:12L));
  setup_select_one_response
    (Some (make_mock_row (List.init 12 (fun index -> mock_int (Int64.of_int (index + 1))))));
  match Via_mock.find_opt () (Requests.find_wide ~c1:1L) with
  | None -> print_endline "find_wide: no row"
  | Some (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) ->
    Printf.printf "find_wide: %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld\n" c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12

let () =
  print_endline "-- Custom_driver: enum";
  let db = ref [ [| "draft" |] ] in
  (match Via_custom.find_opt db (Requests.post_state ~id:1) with
   | Ok (Some `Draft) -> print_endline "post_state: draft"
   | Ok (Some `Published) -> print_endline "post_state: published"
   | Ok None -> print_endline "post_state: no row"
   | Error message -> print_endline ("post_state failed: " ^ message));
  match Via_custom.execute db (Requests.publish ~state:`Published ~id:1) with
  | Ok response -> Printf.printf "publish: %Ld row(s)\n" response.affected_rows
  | Error message -> print_endline ("publish failed: " ^ message)

let () =
  print_endline "-- Print_impl: lists are spliced into the SQL as literals";
  let open Print_impl in
  clear_mock_responses ();
  run_select (Requests.users_by_ids ~ids:[ 1L; 2L; 3L ]);
  run_select (Requests.users_by_ids ~ids:[]);
  setup_execute_response ~affected_rows:2L ();
  let response = Via_mock.execute () (Requests.insert_users ~rows:[ 1L, Some "Ann", "ann@x.org"; 2L, None, "bob@x.org" ]) in
  Printf.printf "insert_users: %Ld row(s)\n" response.affected_rows;
  let response = Via_mock.execute () (Requests.insert_users ~rows:[]) in
  Printf.printf "insert_users, no rows: %Ld row(s)\n" response.affected_rows

let () =
  print_endline "-- Custom_driver: the same lists, quoted by this driver";
  let db = ref [] in
  ignore (Via_custom.iter db (Requests.users_by_ids ~ids:[ 4; 5 ]) ignore);
  let rows =
    [ 4, Some (Bytes.of_string "O'Neil"), Bytes.of_string "o@x.org"; 5, None, Bytes.of_string "n@x.org" ]
  in
  match Via_custom.execute db (Requests.insert_users ~rows) with
  | Ok response -> Printf.printf "insert_users: %Ld row(s)\n" response.affected_rows
  | Error message -> print_endline ("insert_users failed: " ^ message)

let () =
  print_endline "-- Print_impl: nested (CTE, IN-subquery)";
  let open Print_impl in
  clear_mock_responses ();
  setup_select_one_response (Some (make_mock_row [ mock_int 1L; mock_null ]));
  (match Via_mock.find_opt () (Requests.users_cte ~id:1L) with
   | None -> print_endline "users_cte: no row"
   | Some (id, None) -> Printf.printf "users_cte: %Ld, name is NULL\n" id
   | Some (id, Some name) -> Printf.printf "users_cte: %Ld, name %s\n" id name);
  run_select (Requests.users_in_sub ~id:1L)

let () =
  print_endline "-- Custom_driver: nested (CTE, IN-subquery)";
  let db = ref [ [| "2"; "Bob" |] ] in
  (match Via_custom.find_opt db (Requests.users_cte ~id:2) with
   | Ok (Some (id, Some name)) -> Printf.printf "users_cte: %d, name %s\n" id (Bytes.to_string name)
   | Ok (Some (id, None)) -> Printf.printf "users_cte: %d, name is NULL\n" id
   | Ok None -> print_endline "users_cte: no row"
   | Error message -> print_endline ("users_cte failed: " ^ message));
  ignore (Via_custom.iter db (Requests.users_in_sub ~id:2) ignore)

let () =
  print_endline "-- Print_impl: only the chosen branch binds its parameters";
  let open Print_impl in
  clear_mock_responses ();
  run_select (Requests.find_by ~filter:(`By_id 7L));
  run_select (Requests.find_by ~filter:(`By_name "Ann"));
  run_select (Requests.find_after ~min:1L ~name_filter:`None);
  run_select (Requests.find_after ~min:1L ~name_filter:(`Some "Bob"))

let () =
  print_endline "-- Any: untyped column and parameter go through the driver's Types.Any";
  let open Print_impl in
  clear_mock_responses ();
  setup_select_response [ make_mock_row [ mock_int 1L; mock_null ] ];
  Via_mock.iter () (Requests.untyped ~cond:"id > 0") (fun (id, extra) ->
    Printf.printf "untyped: %Ld, extra %s\n" id (Option.value ~default:"NULL" extra));
  let db = ref [ [| "2"; "raw" |] ] in
  ignore (Via_custom.iter db (Requests.untyped ~cond:"id > 1") (fun (id, extra) ->
    Printf.printf "untyped: %d, extra %s\n" id (Option.value ~default:"NULL" extra)))

let () =
  print_endline "-- module= codecs wrap canonical values under either driver";
  let open Print_impl in
  let id value = Requests.Wrapped_id.Id value in
  clear_mock_responses ();
  setup_select_one_response (Some (make_mock_row [ mock_int 10L; mock_null ]));
  (match Via_mock.find_opt () (Requests.wrapped_user ~id:(id 10L)) with
   | Some (Id found, None) -> Printf.printf "wrapped_user: %Ld, parent NULL\n" found
   | Some (_, Some _) | None -> print_endline "wrapped_user: unexpected");
  run_select (Requests.wrapped_by_ids ~ids:[ id 10L; id 11L ]);
  setup_execute_response ~affected_rows:2L ();
  let response =
    Via_mock.execute ()
      (Requests.insert_wrapped ~rows:[ id 12L, None; id 13L, Some (id 10L) ])
  in
  Printf.printf "insert_wrapped: %Ld row(s)\n" response.affected_rows;
  let db = ref [ [| "20"; "10" |] ] in
  (match Via_custom.find_opt db (Requests.wrapped_user ~id:(id 20L)) with
   | Ok (Some (Id found, Some (Id parent))) ->
     Printf.printf "wrapped_user custom: %Ld, parent %Ld\n" found parent
   | Ok (Some (_, None)) | Ok None | Error _ ->
     print_endline "wrapped_user custom: unexpected");
  ignore (Via_custom.iter db (Requests.wrapped_by_ids ~ids:[ id 20L; id 21L ]) ignore);
  match Via_custom.execute db
          (Requests.insert_wrapped ~rows:[ id 22L, None; id 23L, Some (id 20L) ]) with
  | Ok response -> Printf.printf "insert_wrapped custom: %Ld row(s)\n" response.affected_rows
  | Error message -> print_endline ("insert_wrapped custom failed: " ^ message)
