module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_registration_feedbacks db  =
    T.execute db ("CREATE TABLE IF NOT EXISTS registration_feedbacks (\n\
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,\n\
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,\n\
  `user_message_2` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,\n\
  PRIMARY KEY (`id`)\n\
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin") T.no_params

  let test_name db ~id ~search =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text stmt 1), (T.get_column_Text stmt 2)
    in
    let set_params stmt =
      let p = T.start_params stmt (1 + (match search with Some (_, _, xs, xss) -> 3 + (match xs with [] -> 0 | _ :: _ -> 0) + (match xss with `A _ -> 1 | `B _ -> 1) | None -> 0)) in
      T.set_param_Int p id;
      begin match search with
      | None -> ()
      | Some (search, search2, _, xss) ->
        T.set_param_Text p search;
        T.set_param_Text p search;
        T.set_param_Text p search2;
        begin match xss with
        | `A (a) ->
          T.set_param_Text p a;
        | `B (b) ->
          T.set_param_Text p b;
        end;
      end;
      T.finish_params p
    in
    T.select_one_maybe db ("SELECT * FROM registration_feedbacks WHERE\n\
  id = ? AND\n\
 " ^ (match search with Some (_, _, xs, xss) -> " ( " ^ " \n\
  `user_message` = " ^ "?" ^ " \n\
    OR `user_message` = " ^ "?" ^ "\n\
    OR `user_message_2` = " ^ "?" ^ " \n\
    OR " ^ (match xs with [] -> "FALSE" | _ :: _ -> "`user_message_2` IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal xs) ^ ")") ^ "\n\
    OR " ^ (match xss with `A _ -> " user_message_2 = " ^ "?" ^ " " | `B _ -> " user_message_2 = " ^ "?" ^ " ") ^ "\n\
 " ^ " ) " | None -> " TRUE ")) set_params get_row

  module Single = struct
    let test_name db ~id ~search callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~user_message:(T.get_column_Text stmt 1)
          ~user_message_2:(T.get_column_Text stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1 + (match search with Some (_, _, xs, xss) -> 3 + (match xs with [] -> 0 | _ :: _ -> 0) + (match xss with `A _ -> 1 | `B _ -> 1) | None -> 0)) in
        T.set_param_Int p id;
        begin match search with
        | None -> ()
        | Some (search, search2, _, xss) ->
          T.set_param_Text p search;
          T.set_param_Text p search;
          T.set_param_Text p search2;
          begin match xss with
          | `A (a) ->
            T.set_param_Text p a;
          | `B (b) ->
            T.set_param_Text p b;
          end;
        end;
        T.finish_params p
      in
      T.select_one_maybe db ("SELECT * FROM registration_feedbacks WHERE\n\
  id = ? AND\n\
 " ^ (match search with Some (_, _, xs, xss) -> " ( " ^ " \n\
  `user_message` = " ^ "?" ^ " \n\
    OR `user_message` = " ^ "?" ^ "\n\
    OR `user_message_2` = " ^ "?" ^ " \n\
    OR " ^ (match xs with [] -> "FALSE" | _ :: _ -> "`user_message_2` IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal xs) ^ ")") ^ "\n\
    OR " ^ (match xss with `A _ -> " user_message_2 = " ^ "?" ^ " " | `B _ -> " user_message_2 = " ^ "?" ^ " ") ^ "\n\
 " ^ " ) " | None -> " TRUE ")) set_params invoke_callback

  end (* module Single *)
end (* module Sqlgg *)
