module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_random_table_1 db  =
    T.execute db ("CREATE TABLE IF NOT EXISTS random_table_1 (\n\
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,\n\
  `id2` int(10) unsigned NOT NULL AUTO_INCREMENT,\n\
  PRIMARY KEY (`id`)\n\
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin") T.no_params

  let select_1 db ~id ~ids2 callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match id with `Another (ids) -> 0 + (match ids with [] -> 0 | _ :: _ -> 0) | `Andanother _ -> 1 | `Lol (idss) -> 0 + (match idss with [] -> 0 | _ :: _ -> 0)) + (match ids2 with Some (ids2) -> 0 + (match ids2 with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
      begin match id with
      | `Another (ids) ->
        begin match ids with
        | [] -> ()
        | _ :: _ ->
          ()
        end;
      | `Andanother (x) ->
        T.set_param_Int p x;
      | `Lol (idss) ->
        begin match idss with
        | [] -> ()
        | _ :: _ ->
          ()
        end;
      end;
      begin match ids2 with
      | None -> ()
      | Some (ids2) ->
        begin match ids2 with
        | [] -> ()
        | _ :: _ ->
          ()
        end;
      end;
      T.finish_params p
    in
    T.select db ("SELECT id FROM random_table_1\n\
WHERE \n\
  " ^ (match id with `Another (ids) -> " " ^ (match ids with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids_0n, ids_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_1n); Buffer.add_char _sqlgg_b ')') ids; Buffer.contents _sqlgg_b) ^ ")") ^ " " | `Andanother _ -> " " ^ "?" ^ " > 2 " | `Lol (idss) -> " " ^ (match idss with [] -> "FALSE" | _ :: _ -> "`id` IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal idss) ^ ")") ^ " ") ^ " \n\
  OR " ^ (match ids2 with Some (ids2) -> " ( " ^ " " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")") ^ " " ^ " ) " | None -> " TRUE ")) set_params invoke_callback

  let select_2 db ~ids2 callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match ids2 with [] -> 0 | _ :: _ -> 0)) in
      begin match ids2 with
      | [] -> ()
      | _ :: _ ->
        ()
      end;
      T.finish_params p
    in
    T.select db ("SELECT id FROM random_table_1\n\
WHERE " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")")) set_params invoke_callback

  module Fold = struct
    let select_1 db ~id ~ids2 callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match id with `Another (ids) -> 0 + (match ids with [] -> 0 | _ :: _ -> 0) | `Andanother _ -> 1 | `Lol (idss) -> 0 + (match idss with [] -> 0 | _ :: _ -> 0)) + (match ids2 with Some (ids2) -> 0 + (match ids2 with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
        begin match id with
        | `Another (ids) ->
          begin match ids with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        | `Andanother (x) ->
          T.set_param_Int p x;
        | `Lol (idss) ->
          begin match idss with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        end;
        begin match ids2 with
        | None -> ()
        | Some (ids2) ->
          begin match ids2 with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM random_table_1\n\
WHERE \n\
  " ^ (match id with `Another (ids) -> " " ^ (match ids with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids_0n, ids_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_1n); Buffer.add_char _sqlgg_b ')') ids; Buffer.contents _sqlgg_b) ^ ")") ^ " " | `Andanother _ -> " " ^ "?" ^ " > 2 " | `Lol (idss) -> " " ^ (match idss with [] -> "FALSE" | _ :: _ -> "`id` IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal idss) ^ ")") ^ " ") ^ " \n\
  OR " ^ (match ids2 with Some (ids2) -> " ( " ^ " " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")") ^ " " ^ " ) " | None -> " TRUE ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let select_2 db ~ids2 callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids2 with [] -> 0 | _ :: _ -> 0)) in
        begin match ids2 with
        | [] -> ()
        | _ :: _ ->
          ()
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM random_table_1\n\
WHERE " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let select_1 db ~id ~ids2 callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match id with `Another (ids) -> 0 + (match ids with [] -> 0 | _ :: _ -> 0) | `Andanother _ -> 1 | `Lol (idss) -> 0 + (match idss with [] -> 0 | _ :: _ -> 0)) + (match ids2 with Some (ids2) -> 0 + (match ids2 with [] -> 0 | _ :: _ -> 0) | None -> 0)) in
        begin match id with
        | `Another (ids) ->
          begin match ids with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        | `Andanother (x) ->
          T.set_param_Int p x;
        | `Lol (idss) ->
          begin match idss with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        end;
        begin match ids2 with
        | None -> ()
        | Some (ids2) ->
          begin match ids2 with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM random_table_1\n\
WHERE \n\
  " ^ (match id with `Another (ids) -> " " ^ (match ids with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids_0n, ids_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids_1n); Buffer.add_char _sqlgg_b ')') ids; Buffer.contents _sqlgg_b) ^ ")") ^ " " | `Andanother _ -> " " ^ "?" ^ " > 2 " | `Lol (idss) -> " " ^ (match idss with [] -> "FALSE" | _ :: _ -> "`id` IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal idss) ^ ")") ^ " ") ^ " \n\
  OR " ^ (match ids2 with Some (ids2) -> " ( " ^ " " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")") ^ " " ^ " ) " | None -> " TRUE ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let select_2 db ~ids2 callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids2 with [] -> 0 | _ :: _ -> 0)) in
        begin match ids2 with
        | [] -> ()
        | _ :: _ ->
          ()
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM random_table_1\n\
WHERE " ^ (match ids2 with [] -> "FALSE" | _ :: _ -> "(`id`, `id2`) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (ids2_0n, ids2_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (T.Types.Int.to_literal ids2_1n); Buffer.add_char _sqlgg_b ')') ids2; Buffer.contents _sqlgg_b) ^ ")")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
