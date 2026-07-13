open Printf

module C = Compose.Sqlgg(Print_ocaml_impl)
module W = Cte.Sqlgg(Print_ocaml_impl)

let go name f =
  printf "-- %s\n" name;
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_select_response [];
  f ();
  printf "\n"

let () =
  let db = () in

  go "in_list: empty ids" (fun () ->
    C.In_list.select db C.In_list.name ~ids:[] ~nm:"x" (fun _ -> ()));
  go "in_list: two ids" (fun () ->
    C.In_list.select db C.In_list.name ~ids:[1L; 2L] ~nm:"x" (fun _ -> ()));

  go "tuple_list: empty pairs" (fun () ->
    C.Tuple_list.select db C.Tuple_list.id ~pairs:[] (fun _ -> ()));
  go "tuple_list: two pairs with null" (fun () ->
    C.Tuple_list.select db C.Tuple_list.id ~pairs:[(Some 1L, Some "a"); (None, Some "b")] (fun _ -> ()));

  go "opt_filter: none" (fun () ->
    C.Opt_filter.select db C.Opt_filter.name ~v:None (fun _ -> ()));
  go "opt_filter: some" (fun () ->
    C.Opt_filter.select db C.Opt_filter.name ~v:(Some 5L) (fun _ -> ()));

  go "order_choice: by id" (fun () ->
    C.Order_choice.select db C.Order_choice.name ~sort:`I (fun _ -> ()));
  go "order_choice: by name" (fun () ->
    C.Order_choice.select db C.Order_choice.name ~sort:`N (fun _ -> ()));

  go "order_comma_choice: comma before choice" (fun () ->
    C.Order_comma_choice.select db C.Order_comma_choice.name ~sort:`N (fun _ -> ()));

  go "where_choice: all" (fun () ->
    C.Where_choice.select db C.Where_choice.name ~f:`All (fun _ -> ()));
  go "where_choice: by id" (fun () ->
    C.Where_choice.select db C.Where_choice.name ~f:(`ById 7L) (fun _ -> ()));

  go "shared_choice: all" (fun () ->
    C.Shared_choice.select db C.Shared_choice.name ~f:`All (fun _ -> ()));
  go "shared_choice: by id (both occurrences)" (fun () ->
    C.Shared_choice.select db C.Shared_choice.name ~f:(`ById 7L) (fun _ -> ()));

  go "kitchen_sink: everything on" (fun () ->
    C.Kitchen_sink.select db C.Kitchen_sink.name
      ~ids:[1L; 2L] ~pairs:[(Some 1L, Some "a")] ~nm:(Some "x") ~f:(`ById 5L) ~sort:`N
      (fun _ -> ()));
  go "kitchen_sink: everything off" (fun () ->
    C.Kitchen_sink.select db C.Kitchen_sink.name
      ~ids:[] ~pairs:[] ~nm:None ~f:`All ~sort:`I
      (fun _ -> ()));

  go "cte_plain" (fun () ->
    W.Cte_plain.select db ~st:1L W.Cte_plain.name ~lo:10L (fun _ -> ()));

  go "cte_in_list: empty ids" (fun () ->
    W.Cte_in_list.select db ~ids:[] ~nm:"x" W.Cte_in_list.name ~lo:0L (fun _ -> ()));
  go "cte_in_list: two ids" (fun () ->
    W.Cte_in_list.select db ~ids:[1L; 2L] ~nm:"x" W.Cte_in_list.name ~lo:0L (fun _ -> ()));

  go "cte_choice: all, by id" (fun () ->
    W.Cte_choice.select db ~f:`All W.Cte_choice.name ~sort:`I (fun _ -> ()));
  go "cte_choice: by status, by name" (fun () ->
    W.Cte_choice.select db ~f:(`ByStatus 2L) W.Cte_choice.name ~sort:`N (fun _ -> ()));

  go "cte_opt_filter: none, empty pairs" (fun () ->
    W.Cte_opt_filter.select db ~st:None W.Cte_opt_filter.name ~pairs:[] (fun _ -> ()));
  go "cte_opt_filter: some, pairs" (fun () ->
    W.Cte_opt_filter.select db ~st:(Some 1L) W.Cte_opt_filter.name ~pairs:[(Some 1L, Some "a")] (fun _ -> ()));

  go "cte_shared_choice: all" (fun () ->
    W.Cte_shared_choice.select db W.Cte_shared_choice.name ~f:`All (fun _ -> ()));
  go "cte_shared_choice: by id (cte + main)" (fun () ->
    W.Cte_shared_choice.select db W.Cte_shared_choice.name ~f:(`ById 9L) (fun _ -> ()));

  go "cte_param_col: plain column" (fun () ->
    W.Cte_param_col.select db ~st:1L W.Cte_param_col.id (fun _ -> ()));
  go "cte_param_col: param inside picked column binds after cte param" (fun () ->
    W.Cte_param_col.select db ~st:1L (W.Cte_param_col.is_new (Some 100L)) (fun _ -> ()));

  go "where_subquery: empty names" (fun () ->
    W.Where_subquery.select db W.Where_subquery.name ~st:1L ~names:[] (fun _ -> ()));
  go "where_subquery: two names" (fun () ->
    W.Where_subquery.select db W.Where_subquery.name ~st:1L ~names:["a"; "b"] (fun _ -> ()))
