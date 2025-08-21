module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_example db  =
    T.execute db ("CREATE TABLE example (\n\
    id INT AUTO_INCREMENT PRIMARY KEY,\n\
    name VARCHAR(255) NOT NULL,\n\
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP\n\
)") T.no_params

  let select_1 db ~x callback =
    let invoke_callback stmt =
      callback
        ~id:(ExampleId.get_column (T.get_column_int64 stmt 0))
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match x with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db ("SELECT id FROM example WHERE " ^ (match x with [] -> "FALSE" | _ :: _ -> "(name, id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (x_0n, x_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b ((fun v -> T.Types.Text.string_to_literal (Name.set_param v)) x_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b ((fun v -> T.Types.Int.int64_to_literal (ExampleId.set_param v)) x_1n); Buffer.add_char _sqlgg_b ')') x; Buffer.contents _sqlgg_b) ^ ")")) set_params invoke_callback

  let create_example2 db  =
    T.execute db ("CREATE TABLE example2 (\n\
    id INT AUTO_INCREMENT PRIMARY KEY,\n\
    name_2 VARCHAR(255) NOT NULL\n\
)") T.no_params

  let select_3 db ~name ~name_2 ~id callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name_2:(Name2.get_column (T.get_column_string stmt 1))
    in
    let set_params stmt =
      let p = T.start_params stmt (1 + (match name with [] -> 0 | _ :: _ -> 0) + (match name_2 with [] -> 0 | _ :: _ -> 0)) in
      T.set_param_int64 p (ExampleId.set_param id);
      T.finish_params p
    in
    T.select db ("SELECT example2.id, name_2 \n\
FROM example\n\
JOIN example2 ON example.id = example2.id\n\
WHERE " ^ (match name with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name.set_param v)) name) ^ ")") ^ " AND " ^ (match name_2 with [] -> "FALSE" | _ :: _ -> "example2.name_2 IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name2.set_param v)) name_2) ^ ")") ^ " AND example.id = ?") set_params invoke_callback

  module Fold = struct
    let select_1 db ~x callback acc =
      let invoke_callback stmt =
        callback
          ~id:(ExampleId.get_column (T.get_column_int64 stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM example WHERE " ^ (match x with [] -> "FALSE" | _ :: _ -> "(name, id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (x_0n, x_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b ((fun v -> T.Types.Text.string_to_literal (Name.set_param v)) x_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b ((fun v -> T.Types.Int.int64_to_literal (ExampleId.set_param v)) x_1n); Buffer.add_char _sqlgg_b ')') x; Buffer.contents _sqlgg_b) ^ ")")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let select_3 db ~name ~name_2 ~id callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name_2:(Name2.get_column (T.get_column_string stmt 1))
      in
      let set_params stmt =
        let p = T.start_params stmt (1 + (match name with [] -> 0 | _ :: _ -> 0) + (match name_2 with [] -> 0 | _ :: _ -> 0)) in
        T.set_param_int64 p (ExampleId.set_param id);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT example2.id, name_2 \n\
FROM example\n\
JOIN example2 ON example.id = example2.id\n\
WHERE " ^ (match name with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name.set_param v)) name) ^ ")") ^ " AND " ^ (match name_2 with [] -> "FALSE" | _ :: _ -> "example2.name_2 IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name2.set_param v)) name_2) ^ ")") ^ " AND example.id = ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let select_1 db ~x callback =
      let invoke_callback stmt =
        callback
          ~id:(ExampleId.get_column (T.get_column_int64 stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM example WHERE " ^ (match x with [] -> "FALSE" | _ :: _ -> "(name, id) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (x_0n, x_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b ((fun v -> T.Types.Text.string_to_literal (Name.set_param v)) x_0n); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b ((fun v -> T.Types.Int.int64_to_literal (ExampleId.set_param v)) x_1n); Buffer.add_char _sqlgg_b ')') x; Buffer.contents _sqlgg_b) ^ ")")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let select_3 db ~name ~name_2 ~id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name_2:(Name2.get_column (T.get_column_string stmt 1))
      in
      let set_params stmt =
        let p = T.start_params stmt (1 + (match name with [] -> 0 | _ :: _ -> 0) + (match name_2 with [] -> 0 | _ :: _ -> 0)) in
        T.set_param_int64 p (ExampleId.set_param id);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT example2.id, name_2 \n\
FROM example\n\
JOIN example2 ON example.id = example2.id\n\
WHERE " ^ (match name with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name.set_param v)) name) ^ ")") ^ " AND " ^ (match name_2 with [] -> "FALSE" | _ :: _ -> "example2.name_2 IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Text.string_to_literal (Name2.set_param v)) name_2) ^ ")") ^ " AND example.id = ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
