module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_users db  =
    T.execute_unprepared db ("CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL, email TEXT NULL)")

  let find_user db ~id callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE id = ?") ~name:"find_user" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let select_2 db  =
    let get_row stmt =
      (T.get_column_Int stmt 0)
    in
    T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT COUNT(*) AS total FROM users") ~name:"select_2" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params get_row

  let find_users db ~ids callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text stmt 1)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ~name:"find_users" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let add_users db ~rows =
    ( match rows with [] -> IO.return { T.affected_rows = 0L; insert_id = None } | _ :: _ -> T.execute_unprepared db ("INSERT INTO users (id, name) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (id, name) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal id); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Text.to_literal name); Buffer.add_char _sqlgg_b ')') rows; Buffer.contents _sqlgg_b)))

  let rename_user db ~name ~id =
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_Text p name;
      T.set_param_Int p id;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("UPDATE users SET name = ? WHERE id = ?") ~name:"rename_user" ~kind:Sqlgg_traits.Query.(Update (Some "users")) ()) set_params

  let delete_users_6 db ~id =
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p id;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("DELETE FROM users WHERE id = ?") ~name:"delete_users_6" ~kind:Sqlgg_traits.Query.(Delete ["users"]) ()) set_params

  module Single = struct
    let select_2 db  callback =
      let invoke_callback stmt =
        callback
          ~total:(T.get_column_Int stmt 0)
      in
      T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT COUNT(*) AS total FROM users") ~name:"select_2" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params invoke_callback

  end (* module Single *)
  
  module Fold = struct
    let find_user db ~id callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE id = ?") ~name:"find_user" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let find_users db ~ids callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ~name:"find_users" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let find_user db ~id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE id = ?") ~name:"find_user" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let find_users db ~ids callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT id, name FROM users WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ~name:"find_users" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
