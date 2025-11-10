module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

    module Enum_0 = T.Make_enum(struct
      type t = [`Failed | `Passed | `Skipped]
      let inj = function | "Failed" -> `Failed | "Passed" -> `Passed | "Skipped" -> `Skipped | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
      let proj = function  | `Failed -> "Failed"| `Passed -> "Passed"| `Skipped -> "Skipped"
    end)

    module Enum_1 = T.Make_enum(struct
      type t = [`Active | `Inactive | `Pending]
      let inj = function | "Active" -> `Active | "Inactive" -> `Inactive | "Pending" -> `Pending | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
      let proj = function  | `Active -> "Active"| `Inactive -> "Inactive"| `Pending -> "Pending"
    end)

  let create_test_status db  =
    T.execute db ("CREATE TABLE test_status (\n\
  status ENUM('Failed','Skipped','Passed') NOT NULL\n\
)") T.no_params

  let test1 db ~status_list callback =
    let invoke_callback stmt =
      callback
        ~status:(Enum_0.get_column stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match status_list with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db ("SELECT * FROM test_status WHERE " ^ (match status_list with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map Enum_0.to_literal status_list) ^ ")")) set_params invoke_callback

  let create_users db  =
    T.execute db ("CREATE TABLE users (\n\
  id INT NOT NULL,\n\
  status ENUM('Active','Inactive','Pending') NOT NULL\n\
)") T.no_params

  let test2 db ~rows =
    ( match rows with [] -> IO.return { T.affected_rows = 0L; insert_id = None } | _ :: _ -> T.execute db ("INSERT INTO users (id, status) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (id, status) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal id); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (Enum_1.to_literal status); Buffer.add_char _sqlgg_b ')') rows; Buffer.contents _sqlgg_b)) T.no_params )

  module Fold = struct
    let test1 db ~status_list callback acc =
      let invoke_callback stmt =
        callback
          ~status:(Enum_0.get_column stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match status_list with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT * FROM test_status WHERE " ^ (match status_list with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map Enum_0.to_literal status_list) ^ ")")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let test1 db ~status_list callback =
      let invoke_callback stmt =
        callback
          ~status:(Enum_0.get_column stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match status_list with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT * FROM test_status WHERE " ^ (match status_list with [] -> "FALSE" | _ :: _ -> "status IN " ^  "(" ^ String.concat ", " (List.map Enum_0.to_literal status_list) ^ ")")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
