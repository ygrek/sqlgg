module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_files db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE files (id INT, data BLOB, name TEXT)") ~name:"create_files" ~kind:Sqlgg_traits.Query.(Create "files") ())

  let by_data db ~datas callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match datas with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match datas with [] -> "FALSE" | _ :: _ -> "data IN " ^  "(" ^ String.concat ", " (List.map T.Types.Blob.to_literal datas) ^ ")")) ~name:"by_data" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let by_name db ~names callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int_nullable stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"by_name" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  module Fold = struct
    let by_data db ~datas callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match datas with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match datas with [] -> "FALSE" | _ :: _ -> "data IN " ^  "(" ^ String.concat ", " (List.map T.Types.Blob.to_literal datas) ^ ")")) ~name:"by_data" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let by_name db ~names callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"by_name" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let by_data db ~datas callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match datas with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match datas with [] -> "FALSE" | _ :: _ -> "data IN " ^  "(" ^ String.concat ", " (List.map T.Types.Blob.to_literal datas) ^ ")")) ~name:"by_data" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let by_name db ~names callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match names with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM files WHERE " ^ (match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ~name:"by_name" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
