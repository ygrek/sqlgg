module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Dynamic_projection = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let selected f : _ t =
        let _set_selected p =
          begin match f with
          | `B -> ()
          | `A (_,n) ->
            T.set_param_Int p n;
          end;
          ()
        in
        {
          set = _set_selected;
          read = (fun row idx -> (T.get_column_Bool row idx, idx + 1));
          column = ("" ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) "));
          count = 0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `B -> 0);
          deps = [];
        }
    end
    include Cols
    let cols = object
      method selected = Cols.selected
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM subst_items") ~name:"dynamic_projection" ~kind:Sqlgg_traits.Query.(Select Nat) ())
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
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM subst_items") ~name:"dynamic_projection" ~kind:Sqlgg_traits.Query.(Select Nat) ())
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
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM subst_items") ~name:"dynamic_projection" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Dynamic_join_with_choice = struct
    type brand = Profiles
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("users.id");
          count = 0;
          deps = [];
        }
      let bio : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("profiles.bio");
          count = 0;
          deps = [Profiles];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method bio = Cols.bio
    end

    let select db (col : _ t) ~filter callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) + col.count) in
        col.set p;
        begin match filter with
        | `B -> ()
        | `A (_,filter_n) ->
          T.set_param_Int p filter_n;
        end;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM users" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles ON profiles.user_id = users.id" else "") ^ "\n\
WHERE " ^ ((match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) "))) ~name:"dynamic_join_with_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ())
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~filter callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) + col.count) in
          col.set p;
          begin match filter with
          | `B -> ()
          | `A (_,filter_n) ->
            T.set_param_Int p filter_n;
          end;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM users" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles ON profiles.user_id = users.id" else "") ^ "\n\
WHERE " ^ ((match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) "))) ~name:"dynamic_join_with_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~filter =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0) + col.count) in
          col.set p;
          begin match filter with
          | `B -> ()
          | `A (_,filter_n) ->
            T.set_param_Int p filter_n;
          end;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ "\n\
FROM users" ^ (if List.mem Profiles col.deps then " LEFT JOIN profiles ON profiles.user_id = users.id" else "") ^ "\n\
WHERE " ^ ((match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) "))) ~name:"dynamic_join_with_choice" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_subst_items db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE subst_items (\n\
  id INT NOT NULL,\n\
  score INT NOT NULL\n\
)") ~name:"create_subst_items" ~kind:Sqlgg_traits.Query.(Create "subst_items") ())

  let dynamic_projection_static db ~f callback =
    let invoke_callback stmt =
      callback
        ~selected:(T.get_column_Bool stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match f with
      | `B -> ()
      | `A (_,n) ->
        T.set_param_Int p n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " AS selected\n\
FROM subst_items") ~name:"dynamic_projection_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let create_users db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE users (\n\
  id INT NOT NULL PRIMARY KEY\n\
)") ~name:"create_users" ~kind:Sqlgg_traits.Query.(Create "users") ())

  let create_profiles db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE profiles (\n\
  user_id INT NOT NULL PRIMARY KEY,\n\
  bio TEXT\n\
)") ~name:"create_profiles" ~kind:Sqlgg_traits.Query.(Create "profiles") ())

  let dynamic_join_with_choice_static db ~filter callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~bio:(T.get_column_Text_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
      begin match filter with
      | `B -> ()
      | `A (_,filter_n) ->
        T.set_param_Int p filter_n;
      end;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT users.id, profiles.bio\n\
FROM users\n\
LEFT JOIN profiles ON profiles.user_id = users.id\n\
WHERE " ^ (match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"dynamic_join_with_choice_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  module Fold = struct
    let dynamic_projection_static db ~f callback acc =
      let invoke_callback stmt =
        callback
          ~selected:(T.get_column_Bool stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match f with
        | `B -> ()
        | `A (_,n) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " AS selected\n\
FROM subst_items") ~name:"dynamic_projection_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let dynamic_join_with_choice_static db ~filter callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~bio:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match filter with
        | `B -> ()
        | `A (_,filter_n) ->
          T.set_param_Int p filter_n;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT users.id, profiles.bio\n\
FROM users\n\
LEFT JOIN profiles ON profiles.user_id = users.id\n\
WHERE " ^ (match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"dynamic_join_with_choice_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let dynamic_projection_static db ~f callback =
      let invoke_callback stmt =
        callback
          ~selected:(T.get_column_Bool stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `A (ids, _) -> 1 + (match ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match f with
        | `B -> ()
        | `A (_,n) ->
          T.set_param_Int p n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT " ^ (match f with `A (ids, _) -> " ( " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ " AND score = ? ) " | `B -> " ( TRUE ) ") ^ " AS selected\n\
FROM subst_items") ~name:"dynamic_projection_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let dynamic_join_with_choice_static db ~filter callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~bio:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match filter with `A (filter_ids, _) -> 1 + (match filter_ids with [] -> 0 | _ :: _ -> 0) | `B -> 0)) in
        begin match filter with
        | `B -> ()
        | `A (_,filter_n) ->
          T.set_param_Int p filter_n;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT users.id, profiles.bio\n\
FROM users\n\
LEFT JOIN profiles ON profiles.user_id = users.id\n\
WHERE " ^ (match filter with `A (filter_ids, _) -> " ( " ^ (match filter_ids with [] -> "FALSE" | _ :: _ -> "users.id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal filter_ids) ^ ")") ^ " AND users.id = ? ) " | `B -> " ( TRUE ) ")) ~name:"dynamic_join_with_choice_static" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
