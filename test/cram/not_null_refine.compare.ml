module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Dynamic_not_null = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
      let descr : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("descr");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method name = Cols.name
      method descr = Cols.descr
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Dynamic_or_stays_nullable = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
      let descr : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("descr");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method name = Cols.name
      method descr = Cols.descr
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_items db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE items (\n\
  id INT NOT NULL,\n\
  name TEXT NULL,\n\
  descr TEXT NULL,\n\
  num INT NULL,\n\
  num2 INT NULL\n\
)") ~name:"create_items" ~kind:Sqlgg_traits.Query.(Create "items") ())

  let static_not_null db  callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let optional_guard_stays_nullable db ~name callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match name with Some _ -> 1 | None -> 0)) in
      begin match name with
      | None -> ()
      | Some name ->
        T.set_param_Text p name;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match name with Some _ -> " ( " ^ "name = " ^ "?" ^ " ) " | None -> " TRUE ")) ~name:"optional_guard_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let in_param_is_strict db ~names callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"in_param_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let not_in_param_stays_nullable db ~names callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "TRUE" | _ :: _ -> "name NOT IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"not_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let negated_in_param_stays_nullable db ~names callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE NOT (" ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")") ^ ")") ~name:"negated_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let in_tuple_list_is_strict db ~pairs callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text stmt 0)
        ~descr:(T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "FALSE" | _ :: _ -> "(name, descr) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_1n); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"in_tuple_list_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let not_in_tuple_list_stays_nullable db ~pairs callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text_nullable stmt 0)
        ~descr:(T.get_column_Text_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "TRUE" | _ :: _ -> "(name, descr) NOT IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"not_in_tuple_list_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let in_subquery_stays_nullable db  callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text_nullable stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IN (SELECT descr FROM items)") ~name:"in_subquery_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let between_params_are_strict db ~lo ~hi callback =
    let invoke_callback stmt =
      callback
        ~num:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Int p lo;
      T.set_param_Int p hi;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num FROM items WHERE num BETWEEN ? AND ?") ~name:"between_params_are_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let between_bound_columns_stay_nullable db ~hi callback =
    let invoke_callback stmt =
      callback
        ~num:(T.get_column_Int stmt 0)
        ~num2:(T.get_column_Int_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      begin match hi with None -> T.set_param_null p | Some v -> T.set_param_Int p v end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num, num2 FROM items WHERE num BETWEEN num2 AND ?") ~name:"between_bound_columns_stay_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  module Fold = struct
    let static_not_null db  callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let optional_guard_stays_nullable db ~name callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match name with Some _ -> 1 | None -> 0)) in
        begin match name with
        | None -> ()
        | Some name ->
          T.set_param_Text p name;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match name with Some _ -> " ( " ^ "name = " ^ "?" ^ " ) " | None -> " TRUE ")) ~name:"optional_guard_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let in_param_is_strict db ~names callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"in_param_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let not_in_param_stays_nullable db ~names callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "TRUE" | _ :: _ -> "name NOT IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"not_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let negated_in_param_stays_nullable db ~names callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE NOT (" ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")") ^ ")") ~name:"negated_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let in_tuple_list_is_strict db ~pairs callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
          ~descr:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "FALSE" | _ :: _ -> "(name, descr) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_1n); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"in_tuple_list_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let not_in_tuple_list_stays_nullable db ~pairs callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
          ~descr:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "TRUE" | _ :: _ -> "(name, descr) NOT IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"not_in_tuple_list_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let in_subquery_stays_nullable db  callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IN (SELECT descr FROM items)") ~name:"in_subquery_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let between_params_are_strict db ~lo ~hi callback acc =
      let invoke_callback stmt =
        callback
          ~num:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p lo;
        T.set_param_Int p hi;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num FROM items WHERE num BETWEEN ? AND ?") ~name:"between_params_are_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let between_bound_columns_stay_nullable db ~hi callback acc =
      let invoke_callback stmt =
        callback
          ~num:(T.get_column_Int stmt 0)
          ~num2:(T.get_column_Int_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        begin match hi with None -> T.set_param_null p | Some v -> T.set_param_Int p v end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num, num2 FROM items WHERE num BETWEEN num2 AND ?") ~name:"between_bound_columns_stay_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let static_not_null db  callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let optional_guard_stays_nullable db ~name callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match name with Some _ -> 1 | None -> 0)) in
        begin match name with
        | None -> ()
        | Some name ->
          T.set_param_Text p name;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match name with Some _ -> " ( " ^ "name = " ^ "?" ^ " ) " | None -> " TRUE ")) ~name:"optional_guard_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let in_param_is_strict db ~names callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"in_param_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let not_in_param_stays_nullable db ~names callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE " ^ (match names with [] -> "TRUE" | _ :: _ -> "name NOT IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"not_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let negated_in_param_stays_nullable db ~names callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE NOT (" ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")") ^ ")") ~name:"negated_in_param_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let in_tuple_list_is_strict db ~pairs callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
          ~descr:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "FALSE" | _ :: _ -> "(name, descr) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Text.to_literal pairs_1n); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"in_tuple_list_is_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let not_in_tuple_list_stays_nullable db ~pairs callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
          ~descr:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name, descr FROM items WHERE " ^ (match pairs with [] -> "TRUE" | _ :: _ -> "(name, descr) NOT IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ~name:"not_in_tuple_list_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let in_subquery_stays_nullable db  callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text_nullable stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IN (SELECT descr FROM items)") ~name:"in_subquery_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let between_params_are_strict db ~lo ~hi callback =
      let invoke_callback stmt =
        callback
          ~num:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p lo;
        T.set_param_Int p hi;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num FROM items WHERE num BETWEEN ? AND ?") ~name:"between_params_are_strict" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let between_bound_columns_stay_nullable db ~hi callback =
      let invoke_callback stmt =
        callback
          ~num:(T.get_column_Int stmt 0)
          ~num2:(T.get_column_Int_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        begin match hi with None -> T.set_param_null p | Some v -> T.set_param_Int p v end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT num, num2 FROM items WHERE num BETWEEN num2 AND ?") ~name:"between_bound_columns_stay_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
