module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_registration_feedbacks db  =
    T.execute db ("CREATE TABLE IF NOT EXISTS registration_feedbacks (\n\
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,\n\
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT(''),\n\
  `grant_types` varchar(80) COLLATE utf8_bin NOT NULL DEFAULT 'implicit authorization_code',\n\
  PRIMARY KEY (`id`)\n\
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin") T.no_params

  let insert_registration_feedbacks_1 db ~user_message ~grant_types =
    let set_params stmt =
      let p = T.start_params stmt (0 + (match user_message with Some _ -> 1 | None -> 0) + (match grant_types with Some (grant_types) -> 0 + (match grant_types with `A -> 0 | `B -> 0) | None -> 0)) in
      begin match user_message with
      | None -> ()
      | Some user_message ->
        T.set_param_Text p user_message;
      end;
      T.finish_params p
    in
    T.execute db ("INSERT INTO `registration_feedbacks`\n\
SET\n\
  `user_message` = " ^ (match user_message with Some _ -> " ( " ^ " CONCAT(" ^ "?" ^ ", '22222') " ^ " ) " | None -> " DEFAULT ") ^ ",\n\
  `grant_types` = " ^ (match grant_types with Some (grant_types) -> " ( " ^ " " ^ (match grant_types with `A -> "'2'" | `B -> "'2'") ^ " " ^ " ) " | None -> " DEFAULT ")) set_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
