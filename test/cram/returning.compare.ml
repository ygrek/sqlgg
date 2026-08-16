module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_users db  =
    T.execute db ("CREATE TABLE users (\n\
  id INTEGER PRIMARY KEY,\n\
  name TEXT NOT NULL,\n\
  nick TEXT\n\
)") T.no_params

  let insert_returning_id db ~name ~nick =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id") set_params get_row

  let insert_returning_all db ~name ~nick =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text stmt 1), (T.get_column_Text_nullable stmt 2)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING *") set_params get_row

  let insert_returning_nullable db ~name ~nick =
    let get_row stmt =
      (T.get_column_Text_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING nick") set_params get_row

  let insert_returning_expr db ~name ~nick =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id, CONCAT(name, '!') AS greeting") set_params get_row

  let insert_returning_param db ~name ~nick ~suffix =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (3) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.set_param_Text p suffix;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id, CONCAT(name, ?) AS tagged") set_params get_row

  let insert_set_returning db ~name ~nick =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users SET name = ?, nick = ? RETURNING id") set_params get_row

  let insert_tuple_list_returning db ~values callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~nick:(T.get_column_Text_nullable stmt 1)
    in
    ( match values with [] -> IO.return () | _ :: _ -> T.select db ("INSERT INTO users (name, nick) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (name, nick) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal name); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match nick with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') values; Buffer.contents _sqlgg_b) ^ " RETURNING id, nick") T.no_params invoke_callback)

  let insert_multi_values_returning db ~name1 ~nick1 ~name2 ~nick2 callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (4) in
      T.set_param_Text p name1;
      begin match nick1 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.set_param_Text p name2;
      begin match nick2 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
      T.finish_params p
    in
    T.select db ("INSERT INTO users (name, nick) VALUES (?, ?), (?, ?) RETURNING id") set_params invoke_callback

  let insert_select_returning db ~min callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p min;
      T.finish_params p
    in
    T.select db ("INSERT INTO users (name, nick) SELECT name, nick FROM users WHERE id > ? RETURNING id") set_params invoke_callback

  let insert_on_conflict_returning db ~id ~name =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Int p id;
      T.set_param_Text p name;
      T.finish_params p
    in
    T.select_one db ("INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT(id) DO UPDATE SET name = excluded.name RETURNING id, nick") set_params get_row

  let insert_do_nothing_returning db ~id ~name =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Int p id;
      T.set_param_Text p name;
      T.finish_params p
    in
    T.select_one_maybe db ("INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT(id) DO NOTHING RETURNING id, nick") set_params get_row

  let update_returning db ~name ~id callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~nick:(T.get_column_Text_nullable stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, nick") set_params invoke_callback

  let update_returning_param db ~name ~id ~suffix callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~tagged:(T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (3) in
      T.set_param_Text p name;
      T.set_param_Int p id;
      T.set_param_Text p suffix;
      T.finish_params p
    in
    T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params invoke_callback

  let delete_returning db ~id callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text stmt 1)
        ~nick:(T.get_column_Text_nullable stmt 2)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select db ("DELETE FROM users WHERE id = ? RETURNING *") set_params invoke_callback

  let delete_returning_param db ~id ~suffix callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~tagged:(T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Int p id;
      T.set_param_Text p suffix;
      T.finish_params p
    in
    T.select db ("DELETE FROM users WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params invoke_callback

  module Single = struct
    let insert_returning_id db ~name ~nick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id") set_params invoke_callback

    let insert_returning_all db ~name ~nick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~nick:(T.get_column_Text_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING *") set_params invoke_callback

    let insert_returning_nullable db ~name ~nick callback =
      let invoke_callback stmt =
        callback
          ~nick:(T.get_column_Text_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING nick") set_params invoke_callback

    let insert_returning_expr db ~name ~nick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~greeting:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id, CONCAT(name, '!') AS greeting") set_params invoke_callback

    let insert_returning_param db ~name ~nick ~suffix callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~tagged:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (3) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.set_param_Text p suffix;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (name, nick) VALUES (?, ?) RETURNING id, CONCAT(name, ?) AS tagged") set_params invoke_callback

    let insert_set_returning db ~name ~nick callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        begin match nick with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users SET name = ?, nick = ? RETURNING id") set_params invoke_callback

    let insert_on_conflict_returning db ~id ~name callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p id;
        T.set_param_Text p name;
        T.finish_params p
      in
      T.select_one db ("INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT(id) DO UPDATE SET name = excluded.name RETURNING id, nick") set_params invoke_callback

    let insert_do_nothing_returning db ~id ~name callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p id;
        T.set_param_Text p name;
        T.finish_params p
      in
      T.select_one_maybe db ("INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT(id) DO NOTHING RETURNING id, nick") set_params invoke_callback

  end (* module Single *)
  
  module Fold = struct
    let insert_tuple_list_returning db ~values callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (( match values with [] -> IO.return () | _ :: _ -> T.select db ("INSERT INTO users (name, nick) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (name, nick) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal name); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match nick with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') values; Buffer.contents _sqlgg_b) ^ " RETURNING id, nick") T.no_params (fun x -> r_acc := invoke_callback x !r_acc)))
      (fun () -> IO.return !r_acc)

    let insert_multi_values_returning db ~name1 ~nick1 ~name2 ~nick2 callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4) in
        T.set_param_Text p name1;
        begin match nick1 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.set_param_Text p name2;
        begin match nick2 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("INSERT INTO users (name, nick) VALUES (?, ?), (?, ?) RETURNING id") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let insert_select_returning db ~min callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("INSERT INTO users (name, nick) SELECT name, nick FROM users WHERE id > ? RETURNING id") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let update_returning db ~name ~id callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, nick") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let update_returning_param db ~name ~id ~suffix callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~tagged:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (3) in
        T.set_param_Text p name;
        T.set_param_Int p id;
        T.set_param_Text p suffix;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let delete_returning db ~id callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~nick:(T.get_column_Text_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("DELETE FROM users WHERE id = ? RETURNING *") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let delete_returning_param db ~id ~suffix callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~tagged:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p id;
        T.set_param_Text p suffix;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("DELETE FROM users WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let insert_tuple_list_returning db ~values callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (( match values with [] -> IO.return () | _ :: _ -> T.select db ("INSERT INTO users (name, nick) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (name, nick) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Text.to_literal name); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match nick with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') values; Buffer.contents _sqlgg_b) ^ " RETURNING id, nick") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc)))
      (fun () -> IO.return (List.rev !r_acc))

    let insert_multi_values_returning db ~name1 ~nick1 ~name2 ~nick2 callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4) in
        T.set_param_Text p name1;
        begin match nick1 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.set_param_Text p name2;
        begin match nick2 with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("INSERT INTO users (name, nick) VALUES (?, ?), (?, ?) RETURNING id") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let insert_select_returning db ~min callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("INSERT INTO users (name, nick) SELECT name, nick FROM users WHERE id > ? RETURNING id") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let update_returning db ~name ~id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~nick:(T.get_column_Text_nullable stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, nick") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let update_returning_param db ~name ~id ~suffix callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~tagged:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (3) in
        T.set_param_Text p name;
        T.set_param_Int p id;
        T.set_param_Text p suffix;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("UPDATE users SET name = ? WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let delete_returning db ~id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~nick:(T.get_column_Text_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("DELETE FROM users WHERE id = ? RETURNING *") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let delete_returning_param db ~id ~suffix callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~tagged:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p id;
        T.set_param_Text p suffix;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("DELETE FROM users WHERE id = ? RETURNING id, CONCAT(name, ?) AS tagged") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
