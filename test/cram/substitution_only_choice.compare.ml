module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_subst_items db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE subst_items (\n\
  id INT NOT NULL,\n\
  group_id INT NOT NULL DEFAULT 0,\n\
  score INT NOT NULL DEFAULT 0\n\
)") ~name:"create_subst_items" ~kind:Sqlgg_traits.Query.(Create "subst_items") ())

  let list_first db ~f =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0))) in
      begin match f with
      | `A (_,n) ->
        T.set_param_Int p n;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? ) ")) ~name:"list_first" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let list_last db ~f =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `A (_, ids) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0))) in
      begin match f with
      | `A (n,_) ->
        T.set_param_Int p n;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (_, ids) -> " ( 1 = ? AND " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " ) ")) ~name:"list_last" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let multiple_lists db ~f =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `A (ids, _, more_ids) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) + (match more_ids with [] -> 0 | _ :: _ -> 0))) in
      begin match f with
      | `A (_,n,_) ->
        T.set_param_Int p n;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (ids, _, more_ids) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? AND " ^ (match more_ids with [] -> "FALSE" | _ :: _ -> "2 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal more_ids) ^ ")") ^ " ) ")) ~name:"multiple_lists" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let tuple_list db ~f =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `A (pairs, _) -> 1 + (match pairs with [] -> 0 | _ :: _ -> 0))) in
      begin match f with
      | `A (_,n) ->
        T.set_param_Int p n;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (pairs, _) -> " ( " ^ (match pairs with [] -> "FALSE" | _ :: _ -> "(1, 2) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal pairs_1n); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")") ^ " AND 1 = ? ) ")) ~name:"tuple_list" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let nested db ~outer =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match outer with `A (inner) -> 0 + (match inner with `B (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `C _ -> 1) | `D -> 0)) in
      begin match outer with
      | `D -> ()
      | `A (inner) ->
        begin match inner with
        | `B (_,n) ->
          T.set_param_Int p n;
        | `C (m) ->
          T.set_param_Int p m;
        end;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match outer with `A (inner) -> " ( " ^ (match inner with `B (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? ) " | `C _ -> " ( 1 = ? ) ") ^ " ) " | `D -> " ( TRUE ) ")) ~name:"nested" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let repeated_list db ~repeated =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match repeated with `A (repeated_ids, _) -> 1 + (match repeated_ids with [] -> 0 | _ :: _ -> 0) + (match repeated_ids with [] -> 0 | _ :: _ -> 0))) in
      begin match repeated with
      | `A (_,repeated_n) ->
        T.set_param_Int p repeated_n;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match repeated with `A (repeated_ids, _) -> " ( " ^ (match repeated_ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal repeated_ids) ^ ")") ^ " OR " ^ (match repeated_ids with [] -> "FALSE" | _ :: _ -> "2 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal repeated_ids) ^ ")") ^ " OR 1 = ? ) ")) ~name:"repeated_list" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let duplicate_bound db ~duplicate callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match duplicate with `A (_, duplicate_ids) -> 2 + (match duplicate_ids with [] -> 0 | _ :: _ -> 0))) in
      begin match duplicate with
      | `A (same,_) ->
        T.set_param_Int p same;
        T.set_param_Int p same;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match duplicate with `A (_, duplicate_ids) -> " ( id = ? AND score = ? AND " ^ (match duplicate_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal duplicate_ids) ^ ")") ^ " ) ")) ~name:"duplicate_bound" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let legacy_bound_order db ~search =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match search with `A _ -> 3)) in
      begin match search with
      | `A (email_query,query) ->
        T.set_param_Int p query;
        T.set_param_Int p email_query;
        T.set_param_Int p query;
      end;
      T.finish_params p
    in
    T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1\n\
WHERE " ^ (match search with `A _ -> " ( ? = 1 OR ? = 2 OR ? = 3 ) ")) ~name:"legacy_bound_order" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params get_row

  let projection db ~projection_choice callback =
    let invoke_callback stmt =
      callback
        ~r:(T.get_column_Bool stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match projection_choice with `A (projection_ids, _) -> 1 + (match projection_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match projection_choice with
      | `B -> ()
      | `A (_,projection_n) ->
        T.set_param_Int p projection_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match projection_choice with `A (projection_ids, _) -> " ( " ^ (match projection_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal projection_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
FROM subst_items") ~name:"projection" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let join_on db ~join_choice callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match join_choice with `A (join_ids, _) -> 1 + (match join_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match join_choice with
      | `B -> ()
      | `A (_,join_n) ->
        T.set_param_Int p join_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT a.id\n\
FROM subst_items a\n\
JOIN subst_items b ON " ^ (match join_choice with `A (join_ids, _) -> " ( " ^ (match join_ids with [] -> "FALSE" | _ :: _ -> "b.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal join_ids) ^ ")") ^ " AND a.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"join_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let having db ~having_choice callback =
    let invoke_callback stmt =
      callback
        ~group_id:(T.get_column_Int stmt 0)
        ~r:(T.get_column_Int stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match having_choice with `A (having_ids, _) -> 1 + (match having_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match having_choice with
      | `B -> ()
      | `A (_,having_n) ->
        T.set_param_Int p having_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT group_id, COUNT(*)\n\
FROM subst_items\n\
GROUP BY group_id\n\
HAVING " ^ (match having_choice with `A (having_ids, _) -> " ( " ^ (match having_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal having_ids) ^ ")") ^ " AND COUNT(*) > ? ) " | `B -> " ( TRUE ) ")) ~name:"having" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let order_by db ~order_choice callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match order_choice with `A (order_ids, _) -> 1 + (match order_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match order_choice with
      | `B -> ()
      | `A (_,order_n) ->
        T.set_param_Int p order_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
ORDER BY " ^ (match order_choice with `A (order_ids, _) -> " ( CASE WHEN " ^ (match order_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal order_ids) ^ ")") ^ " THEN ? ELSE id END ) " | `B -> " ( id ) ")) ~name:"order_by" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let subquery db ~subquery_choice =
    let get_row stmt =
      (T.get_column_Bool_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match subquery_choice with `A (subquery_ids, _) -> 1 + (match subquery_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match subquery_choice with
      | `B -> ()
      | `A (_,subquery_n) ->
        T.set_param_Int p subquery_n;
      end;
      T.finish_params p
    in
    T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT (\n\
  SELECT " ^ (match subquery_choice with `A (subquery_ids, _) -> " ( " ^ (match subquery_ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal subquery_ids) ^ ")") ^ " AND 1 = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
)") ~name:"subquery" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params get_row

  let optional db ~optional_ids callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match optional_ids with Some (optional_ids, _) -> 1 + (match optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
      begin match optional_ids with
      | None -> ()
      | Some (_, optional_n) ->
        T.set_param_Int p optional_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_ids with Some (optional_ids, _) -> " ( " ^ " " ^ (match optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let update_set db ~update_choice =
    let set_params stmt =
      let p = T.start_params stmt (0 + (match update_choice with `A (update_ids, _) -> 1 + (match update_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match update_choice with
      | `B -> ()
      | `A (_,update_n) ->
        T.set_param_Int p update_n;
      end;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("UPDATE subst_items\n\
SET score = " ^ (match update_choice with `A (update_ids, _) -> " ( CASE WHEN " ^ (match update_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal update_ids) ^ ")") ^ " THEN ? ELSE score END ) " | `B -> " ( score ) ")) ~name:"update_set" ~kind:Sqlgg_traits.Query.(Update (Some "subst_items")) ()) set_params

  let delete_where db ~delete_choice =
    let set_params stmt =
      let p = T.start_params stmt (0 + (match delete_choice with `A (delete_ids, _) -> 1 + (match delete_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match delete_choice with
      | `B -> ()
      | `A (_,delete_n) ->
        T.set_param_Int p delete_n;
      end;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("DELETE FROM subst_items\n\
WHERE " ^ (match delete_choice with `A (delete_ids, _) -> " ( " ^ (match delete_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal delete_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ")) ~name:"delete_where" ~kind:Sqlgg_traits.Query.(Delete ["subst_items"]) ()) set_params

  let insert_select db ~insert_choice =
    let set_params stmt =
      let p = T.start_params stmt (0 + (match insert_choice with `A (insert_ids, _) -> 1 + (match insert_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match insert_choice with
      | `B -> ()
      | `A (_,insert_n) ->
        T.set_param_Int p insert_n;
      end;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("INSERT INTO subst_items (id)\n\
SELECT " ^ (match insert_choice with `A (insert_ids, _) -> " ( CASE WHEN " ^ (match insert_ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal insert_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ")) ~name:"insert_select" ~kind:Sqlgg_traits.Query.(Insert "subst_items") ()) set_params

  let function_arg db ~function_choice callback =
    let invoke_callback stmt =
      callback
        ~r:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match function_choice with `A (function_ids, _) -> 1 + (match function_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match function_choice with
      | `B -> ()
      | `A (_,function_n) ->
        T.set_param_Int p function_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(" ^ (match function_choice with `A (function_ids, _) -> " ( CASE WHEN " ^ (match function_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal function_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ") ^ ", 0)\n\
FROM subst_items") ~name:"function_arg" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let aggregate_arg db ~aggregate_choice =
    let get_row stmt =
      (T.get_column_Int_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match aggregate_choice with `A (aggregate_ids, _) -> 1 + (match aggregate_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match aggregate_choice with
      | `B -> ()
      | `A (_,aggregate_n) ->
        T.set_param_Int p aggregate_n;
      end;
      T.finish_params p
    in
    T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT SUM(" ^ (match aggregate_choice with `A (aggregate_ids, _) -> " ( CASE WHEN " ^ (match aggregate_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal aggregate_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ") ^ ")\n\
FROM subst_items") ~name:"aggregate_arg" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params get_row

  let flat_case_control db ~case_ids ~case_n callback =
    let invoke_callback stmt =
      callback
        ~r:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1 + (match case_ids with [] -> 0 | _ :: _ -> 0)) in
      T.set_param_Int p case_n;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN " ^ (match case_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_ids) ^ ")") ^ " THEN ? ELSE score END\n\
FROM subst_items") ~name:"flat_case_control" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let optional_nested_choice db ~optional_pick callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match optional_pick with Some (optional_pick) -> 0 + (match optional_pick with `A (optional_nested_ids, _) -> 1 + (match optional_nested_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) | None -> 0)) in
      begin match optional_pick with
      | None -> ()
      | Some optional_pick ->
        begin match optional_pick with
        | `B -> ()
        | `A (_,optional_nested_n) ->
          T.set_param_Int p optional_nested_n;
        end;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_pick with Some (optional_pick) -> " ( " ^ " " ^ (match optional_pick with `A (optional_nested_ids, _) -> " ( " ^ (match optional_nested_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_nested_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional_nested_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let choice_nested_optional db ~nested_optional_pick callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match nested_optional_pick with `A (nested_optional_ids, _) -> 1 + (match nested_optional_ids with Some (nested_optional_ids, _) -> 1 + (match nested_optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0) | `B -> 0)) in
      begin match nested_optional_pick with
      | `B -> ()
      | `A (nested_optional_ids,nested_optional_g) ->
        begin match nested_optional_ids with
        | None -> ()
        | Some (_, nested_optional_n) ->
          T.set_param_Int p nested_optional_n;
        end;
        T.set_param_Int p nested_optional_g;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match nested_optional_pick with `A (nested_optional_ids, _) -> " ( " ^ (match nested_optional_ids with Some (nested_optional_ids, _) -> " ( " ^ " " ^ (match nested_optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal nested_optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ " AND group_id = ? ) " | `B -> " ( TRUE ) ")) ~name:"choice_nested_optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let repeated_tuple_list db ~repeated_tuple_pick callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match repeated_tuple_pick with `A (repeated_pairs, _) -> 1 + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match repeated_tuple_pick with
      | `B -> ()
      | `A (_,repeated_tuple_n) ->
        T.set_param_Int p repeated_tuple_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match repeated_tuple_pick with `A (repeated_pairs, _) -> " (\n\
    " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(id, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(score, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR id = ?\n\
  ) " | `B -> " ( TRUE ) ")) ~name:"repeated_tuple_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let case_choice_condition db ~case_choice callback =
    let invoke_callback stmt =
      callback
        ~r:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match case_choice with `A (case_choice_ids, _) -> 1 + (match case_choice_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match case_choice with
      | `B -> ()
      | `A (_,case_choice_n) ->
        T.set_param_Int p case_choice_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE\n\
  WHEN " ^ (match case_choice with `A (case_choice_ids, _) -> " ( " ^ (match case_choice_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_choice_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
  THEN 1\n\
  ELSE 0\n\
END\n\
FROM subst_items") ~name:"case_choice_condition" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let set_default_nested_choice db ~default_pick =
    let set_params stmt =
      let p = T.start_params stmt (0 + (match default_pick with Some (default_pick) -> 0 + (match default_pick with `A (default_ids, _) -> 1 + (match default_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) | None -> 0)) in
      begin match default_pick with
      | None -> ()
      | Some default_pick ->
        begin match default_pick with
        | `B -> ()
        | `A (_,default_n) ->
          T.set_param_Int p default_n;
        end;
      end;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("UPDATE subst_items\n\
SET score = " ^ (match default_pick with Some (default_pick) -> " ( " ^ " " ^ (match default_pick with `A (default_ids, _) -> " ( CASE WHEN " ^ (match default_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal default_ids) ^ ")") ^ " THEN ? ELSE score END ) " | `B -> " ( score ) ") ^ " " ^ " ) " | None -> " DEFAULT ")) ~name:"set_default_nested_choice" ~kind:Sqlgg_traits.Query.(Update (Some "subst_items")) ()) set_params

  let shared_inside_choice db ~shared_choice =
    let get_row stmt =
      (T.get_column_Bool stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match shared_choice with `A (shared_ids, _) -> 1 + (match shared_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match shared_choice with
      | `B -> ()
      | `A (_,shared_n) ->
        T.set_param_Int p shared_n;
      end;
      T.finish_params p
    in
    T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match shared_choice with `A (shared_ids, _) -> " ( EXISTS (WITH filtered AS (SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match shared_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal shared_ids) ^ ")") ^ " AND score = ?) SELECT id FROM filtered) ) " | `B -> " ( TRUE ) ")) ~name:"shared_inside_choice" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params get_row

  module Single = struct
    let list_first db ~f callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0))) in
        begin match f with
        | `A (_,n) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? ) ")) ~name:"list_first" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let list_last db ~f callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (_, ids) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0))) in
        begin match f with
        | `A (n,_) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (_, ids) -> " ( 1 = ? AND " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " ) ")) ~name:"list_last" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let multiple_lists db ~f callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (ids, _, more_ids) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) + (match more_ids with [] -> 0 | _ :: _ -> 0))) in
        begin match f with
        | `A (_,n,_) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (ids, _, more_ids) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? AND " ^ (match more_ids with [] -> "FALSE" | _ :: _ -> "2 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal more_ids) ^ ")") ^ " ) ")) ~name:"multiple_lists" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let tuple_list db ~f callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (pairs, _) -> 1 + (match pairs with [] -> 0 | _ :: _ -> 0))) in
        begin match f with
        | `A (_,n) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match f with `A (pairs, _) -> " ( " ^ (match pairs with [] -> "FALSE" | _ :: _ -> "(1, 2) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal pairs_1n); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")") ^ " AND 1 = ? ) ")) ~name:"tuple_list" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let nested db ~outer callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match outer with `A (inner) -> 0 + (match inner with `B (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `C _ -> 1) | `D -> 0)) in
        begin match outer with
        | `D -> ()
        | `A (inner) ->
          begin match inner with
          | `B (_,n) ->
            T.set_param_Int p n;
          | `C (m) ->
            T.set_param_Int p m;
          end;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match outer with `A (inner) -> " ( " ^ (match inner with `B (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND 1 = ? ) " | `C _ -> " ( 1 = ? ) ") ^ " ) " | `D -> " ( TRUE ) ")) ~name:"nested" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let repeated_list db ~repeated callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match repeated with `A (repeated_ids, _) -> 1 + (match repeated_ids with [] -> 0 | _ :: _ -> 0) + (match repeated_ids with [] -> 0 | _ :: _ -> 0))) in
        begin match repeated with
        | `A (_,repeated_n) ->
          T.set_param_Int p repeated_n;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1 WHERE " ^ (match repeated with `A (repeated_ids, _) -> " ( " ^ (match repeated_ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal repeated_ids) ^ ")") ^ " OR " ^ (match repeated_ids with [] -> "FALSE" | _ :: _ -> "2 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal repeated_ids) ^ ")") ^ " OR 1 = ? ) ")) ~name:"repeated_list" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let legacy_bound_order db ~search callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match search with `A _ -> 3)) in
        begin match search with
        | `A (email_query,query) ->
          T.set_param_Int p query;
          T.set_param_Int p email_query;
          T.set_param_Int p query;
        end;
        T.finish_params p
      in
      T.select_one_maybe db (Sqlgg_traits.Query.make ~sql:("SELECT 1\n\
WHERE " ^ (match search with `A _ -> " ( ? = 1 OR ? = 2 OR ? = 3 ) ")) ~name:"legacy_bound_order" ~kind:Sqlgg_traits.Query.(Select Zero_one) ()) set_params invoke_callback

    let subquery db ~subquery_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Bool_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match subquery_choice with `A (subquery_ids, _) -> 1 + (match subquery_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match subquery_choice with
        | `B -> ()
        | `A (_,subquery_n) ->
          T.set_param_Int p subquery_n;
        end;
        T.finish_params p
      in
      T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT (\n\
  SELECT " ^ (match subquery_choice with `A (subquery_ids, _) -> " ( " ^ (match subquery_ids with [] -> "FALSE" | _ :: _ -> "1 IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal subquery_ids) ^ ")") ^ " AND 1 = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
)") ~name:"subquery" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params invoke_callback

    let aggregate_arg db ~aggregate_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match aggregate_choice with `A (aggregate_ids, _) -> 1 + (match aggregate_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match aggregate_choice with
        | `B -> ()
        | `A (_,aggregate_n) ->
          T.set_param_Int p aggregate_n;
        end;
        T.finish_params p
      in
      T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT SUM(" ^ (match aggregate_choice with `A (aggregate_ids, _) -> " ( CASE WHEN " ^ (match aggregate_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal aggregate_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ") ^ ")\n\
FROM subst_items") ~name:"aggregate_arg" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params invoke_callback

    let shared_inside_choice db ~shared_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Bool stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match shared_choice with `A (shared_ids, _) -> 1 + (match shared_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match shared_choice with
        | `B -> ()
        | `A (_,shared_n) ->
          T.set_param_Int p shared_n;
        end;
        T.finish_params p
      in
      T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match shared_choice with `A (shared_ids, _) -> " ( EXISTS (WITH filtered AS (SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match shared_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal shared_ids) ^ ")") ^ " AND score = ?) SELECT id FROM filtered) ) " | `B -> " ( TRUE ) ")) ~name:"shared_inside_choice" ~kind:Sqlgg_traits.Query.(Select One) ()) set_params invoke_callback

  end (* module Single *)
  
  module Fold = struct
    let duplicate_bound db ~duplicate callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match duplicate with `A (_, duplicate_ids) -> 2 + (match duplicate_ids with [] -> 0 | _ :: _ -> 0))) in
        begin match duplicate with
        | `A (same,_) ->
          T.set_param_Int p same;
          T.set_param_Int p same;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match duplicate with `A (_, duplicate_ids) -> " ( id = ? AND score = ? AND " ^ (match duplicate_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal duplicate_ids) ^ ")") ^ " ) ")) ~name:"duplicate_bound" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let projection db ~projection_choice callback acc =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Bool stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match projection_choice with `A (projection_ids, _) -> 1 + (match projection_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match projection_choice with
        | `B -> ()
        | `A (_,projection_n) ->
          T.set_param_Int p projection_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match projection_choice with `A (projection_ids, _) -> " ( " ^ (match projection_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal projection_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
FROM subst_items") ~name:"projection" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let join_on db ~join_choice callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match join_choice with `A (join_ids, _) -> 1 + (match join_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match join_choice with
        | `B -> ()
        | `A (_,join_n) ->
          T.set_param_Int p join_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT a.id\n\
FROM subst_items a\n\
JOIN subst_items b ON " ^ (match join_choice with `A (join_ids, _) -> " ( " ^ (match join_ids with [] -> "FALSE" | _ :: _ -> "b.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal join_ids) ^ ")") ^ " AND a.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"join_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let having db ~having_choice callback acc =
      let invoke_callback stmt =
        callback
          ~group_id:(T.get_column_Int stmt 0)
          ~r:(T.get_column_Int stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match having_choice with `A (having_ids, _) -> 1 + (match having_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match having_choice with
        | `B -> ()
        | `A (_,having_n) ->
          T.set_param_Int p having_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT group_id, COUNT(*)\n\
FROM subst_items\n\
GROUP BY group_id\n\
HAVING " ^ (match having_choice with `A (having_ids, _) -> " ( " ^ (match having_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal having_ids) ^ ")") ^ " AND COUNT(*) > ? ) " | `B -> " ( TRUE ) ")) ~name:"having" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let order_by db ~order_choice callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match order_choice with `A (order_ids, _) -> 1 + (match order_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match order_choice with
        | `B -> ()
        | `A (_,order_n) ->
          T.set_param_Int p order_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
ORDER BY " ^ (match order_choice with `A (order_ids, _) -> " ( CASE WHEN " ^ (match order_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal order_ids) ^ ")") ^ " THEN ? ELSE id END ) " | `B -> " ( id ) ")) ~name:"order_by" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let optional db ~optional_ids callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match optional_ids with Some (optional_ids, _) -> 1 + (match optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
        begin match optional_ids with
        | None -> ()
        | Some (_, optional_n) ->
          T.set_param_Int p optional_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_ids with Some (optional_ids, _) -> " ( " ^ " " ^ (match optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let function_arg db ~function_choice callback acc =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match function_choice with `A (function_ids, _) -> 1 + (match function_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match function_choice with
        | `B -> ()
        | `A (_,function_n) ->
          T.set_param_Int p function_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(" ^ (match function_choice with `A (function_ids, _) -> " ( CASE WHEN " ^ (match function_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal function_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ") ^ ", 0)\n\
FROM subst_items") ~name:"function_arg" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let flat_case_control db ~case_ids ~case_n callback acc =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1 + (match case_ids with [] -> 0 | _ :: _ -> 0)) in
        T.set_param_Int p case_n;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN " ^ (match case_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_ids) ^ ")") ^ " THEN ? ELSE score END\n\
FROM subst_items") ~name:"flat_case_control" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let optional_nested_choice db ~optional_pick callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match optional_pick with Some (optional_pick) -> 0 + (match optional_pick with `A (optional_nested_ids, _) -> 1 + (match optional_nested_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) | None -> 0)) in
        begin match optional_pick with
        | None -> ()
        | Some optional_pick ->
          begin match optional_pick with
          | `B -> ()
          | `A (_,optional_nested_n) ->
            T.set_param_Int p optional_nested_n;
          end;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_pick with Some (optional_pick) -> " ( " ^ " " ^ (match optional_pick with `A (optional_nested_ids, _) -> " ( " ^ (match optional_nested_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_nested_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional_nested_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let choice_nested_optional db ~nested_optional_pick callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match nested_optional_pick with `A (nested_optional_ids, _) -> 1 + (match nested_optional_ids with Some (nested_optional_ids, _) -> 1 + (match nested_optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0) | `B -> 0)) in
        begin match nested_optional_pick with
        | `B -> ()
        | `A (nested_optional_ids,nested_optional_g) ->
          begin match nested_optional_ids with
          | None -> ()
          | Some (_, nested_optional_n) ->
            T.set_param_Int p nested_optional_n;
          end;
          T.set_param_Int p nested_optional_g;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match nested_optional_pick with `A (nested_optional_ids, _) -> " ( " ^ (match nested_optional_ids with Some (nested_optional_ids, _) -> " ( " ^ " " ^ (match nested_optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal nested_optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ " AND group_id = ? ) " | `B -> " ( TRUE ) ")) ~name:"choice_nested_optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let repeated_tuple_list db ~repeated_tuple_pick callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match repeated_tuple_pick with `A (repeated_pairs, _) -> 1 + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match repeated_tuple_pick with
        | `B -> ()
        | `A (_,repeated_tuple_n) ->
          T.set_param_Int p repeated_tuple_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match repeated_tuple_pick with `A (repeated_pairs, _) -> " (\n\
    " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(id, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(score, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR id = ?\n\
  ) " | `B -> " ( TRUE ) ")) ~name:"repeated_tuple_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let case_choice_condition db ~case_choice callback acc =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match case_choice with `A (case_choice_ids, _) -> 1 + (match case_choice_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match case_choice with
        | `B -> ()
        | `A (_,case_choice_n) ->
          T.set_param_Int p case_choice_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE\n\
  WHEN " ^ (match case_choice with `A (case_choice_ids, _) -> " ( " ^ (match case_choice_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_choice_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
  THEN 1\n\
  ELSE 0\n\
END\n\
FROM subst_items") ~name:"case_choice_condition" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let duplicate_bound db ~duplicate callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match duplicate with `A (_, duplicate_ids) -> 2 + (match duplicate_ids with [] -> 0 | _ :: _ -> 0))) in
        begin match duplicate with
        | `A (same,_) ->
          T.set_param_Int p same;
          T.set_param_Int p same;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match duplicate with `A (_, duplicate_ids) -> " ( id = ? AND score = ? AND " ^ (match duplicate_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal duplicate_ids) ^ ")") ^ " ) ")) ~name:"duplicate_bound" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let projection db ~projection_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Bool stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match projection_choice with `A (projection_ids, _) -> 1 + (match projection_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match projection_choice with
        | `B -> ()
        | `A (_,projection_n) ->
          T.set_param_Int p projection_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match projection_choice with `A (projection_ids, _) -> " ( " ^ (match projection_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal projection_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
FROM subst_items") ~name:"projection" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let join_on db ~join_choice callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match join_choice with `A (join_ids, _) -> 1 + (match join_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match join_choice with
        | `B -> ()
        | `A (_,join_n) ->
          T.set_param_Int p join_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT a.id\n\
FROM subst_items a\n\
JOIN subst_items b ON " ^ (match join_choice with `A (join_ids, _) -> " ( " ^ (match join_ids with [] -> "FALSE" | _ :: _ -> "b.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal join_ids) ^ ")") ^ " AND a.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"join_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let having db ~having_choice callback =
      let invoke_callback stmt =
        callback
          ~group_id:(T.get_column_Int stmt 0)
          ~r:(T.get_column_Int stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match having_choice with `A (having_ids, _) -> 1 + (match having_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match having_choice with
        | `B -> ()
        | `A (_,having_n) ->
          T.set_param_Int p having_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT group_id, COUNT(*)\n\
FROM subst_items\n\
GROUP BY group_id\n\
HAVING " ^ (match having_choice with `A (having_ids, _) -> " ( " ^ (match having_ids with [] -> "FALSE" | _ :: _ -> "group_id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal having_ids) ^ ")") ^ " AND COUNT(*) > ? ) " | `B -> " ( TRUE ) ")) ~name:"having" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let order_by db ~order_choice callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match order_choice with `A (order_ids, _) -> 1 + (match order_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match order_choice with
        | `B -> ()
        | `A (_,order_n) ->
          T.set_param_Int p order_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
ORDER BY " ^ (match order_choice with `A (order_ids, _) -> " ( CASE WHEN " ^ (match order_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal order_ids) ^ ")") ^ " THEN ? ELSE id END ) " | `B -> " ( id ) ")) ~name:"order_by" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let optional db ~optional_ids callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match optional_ids with Some (optional_ids, _) -> 1 + (match optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
        begin match optional_ids with
        | None -> ()
        | Some (_, optional_n) ->
          T.set_param_Int p optional_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_ids with Some (optional_ids, _) -> " ( " ^ " " ^ (match optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let function_arg db ~function_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match function_choice with `A (function_ids, _) -> 1 + (match function_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match function_choice with
        | `B -> ()
        | `A (_,function_n) ->
          T.set_param_Int p function_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(" ^ (match function_choice with `A (function_ids, _) -> " ( CASE WHEN " ^ (match function_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal function_ids) ^ ")") ^ " THEN ? ELSE 0 END ) " | `B -> " ( 0 ) ") ^ ", 0)\n\
FROM subst_items") ~name:"function_arg" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let flat_case_control db ~case_ids ~case_n callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1 + (match case_ids with [] -> 0 | _ :: _ -> 0)) in
        T.set_param_Int p case_n;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN " ^ (match case_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_ids) ^ ")") ^ " THEN ? ELSE score END\n\
FROM subst_items") ~name:"flat_case_control" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let optional_nested_choice db ~optional_pick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match optional_pick with Some (optional_pick) -> 0 + (match optional_pick with `A (optional_nested_ids, _) -> 1 + (match optional_nested_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) | None -> 0)) in
        begin match optional_pick with
        | None -> ()
        | Some optional_pick ->
          begin match optional_pick with
          | `B -> ()
          | `A (_,optional_nested_n) ->
            T.set_param_Int p optional_nested_n;
          end;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match optional_pick with Some (optional_pick) -> " ( " ^ " " ^ (match optional_pick with `A (optional_nested_ids, _) -> " ( " ^ (match optional_nested_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal optional_nested_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " " ^ " ) " | None -> " TRUE ")) ~name:"optional_nested_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let choice_nested_optional db ~nested_optional_pick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match nested_optional_pick with `A (nested_optional_ids, _) -> 1 + (match nested_optional_ids with Some (nested_optional_ids, _) -> 1 + (match nested_optional_ids with [] -> 0 | _ :: _ -> 0) | None -> 0) | `B -> 0)) in
        begin match nested_optional_pick with
        | `B -> ()
        | `A (nested_optional_ids,nested_optional_g) ->
          begin match nested_optional_ids with
          | None -> ()
          | Some (_, nested_optional_n) ->
            T.set_param_Int p nested_optional_n;
          end;
          T.set_param_Int p nested_optional_g;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match nested_optional_pick with `A (nested_optional_ids, _) -> " ( " ^ (match nested_optional_ids with Some (nested_optional_ids, _) -> " ( " ^ " " ^ (match nested_optional_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal nested_optional_ids) ^ ")") ^ " AND score = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ " AND group_id = ? ) " | `B -> " ( TRUE ) ")) ~name:"choice_nested_optional" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let repeated_tuple_list db ~repeated_tuple_pick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match repeated_tuple_pick with `A (repeated_pairs, _) -> 1 + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) + (match repeated_pairs with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match repeated_tuple_pick with
        | `B -> ()
        | `A (_,repeated_tuple_n) ->
          T.set_param_Int p repeated_tuple_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id\n\
FROM subst_items\n\
WHERE " ^ (match repeated_tuple_pick with `A (repeated_pairs, _) -> " (\n\
    " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(id, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR " ^ (match repeated_pairs with [] -> "FALSE" | _ :: _ -> "(score, group_id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (repeated_pairs_0n, repeated_pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal repeated_pairs_1n); Buffer.add_char _sqlgg_b ')') repeated_pairs; Buffer.contents _sqlgg_b) ^ ")") ^ "\n\
    OR id = ?\n\
  ) " | `B -> " ( TRUE ) ")) ~name:"repeated_tuple_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let case_choice_condition db ~case_choice callback =
      let invoke_callback stmt =
        callback
          ~r:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match case_choice with `A (case_choice_ids, _) -> 1 + (match case_choice_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match case_choice with
        | `B -> ()
        | `A (_,case_choice_n) ->
          T.set_param_Int p case_choice_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE\n\
  WHEN " ^ (match case_choice with `A (case_choice_ids, _) -> " ( " ^ (match case_choice_ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal case_choice_ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ "\n\
  THEN 1\n\
  ELSE 0\n\
END\n\
FROM subst_items") ~name:"case_choice_condition" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
