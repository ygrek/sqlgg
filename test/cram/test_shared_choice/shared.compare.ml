module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_t db  =
    T.execute db ("CREATE TABLE t (\n\
    id INT NOT NULL,\n\
    name TEXT,\n\
    status INT\n\
)") T.no_params

  let shared_const db ~x callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match x with `Small -> 0 | `Big -> 0) + (match x with `Small -> 0 | `Big -> 0)) in
      T.finish_params p
    in
    T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ") ^ "\n\
   OR status = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ")) set_params invoke_callback

  let shared_param_same_name db ~x callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match x with `Const -> 0 | `Param _ -> 1) + (match x with `Const -> 0 | `Param _ -> 1)) in
      begin match x with
      | `Const -> ()
      | `Param (v) ->
        T.set_param_Int p v;
      end;
      begin match x with
      | `Const -> ()
      | `Param (v) ->
        T.set_param_Int p v;
      end;
      T.finish_params p
    in
    T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ") ^ "\n\
   OR status = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ")) set_params invoke_callback

  let shared_params_diff_names db ~f callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1)) in
      begin match f with
      | `All -> ()
      | `ByStatus (s) ->
        T.set_param_Int p s;
      end;
      begin match f with
      | `All -> ()
      | `ByStatus (other) ->
        T.set_param_Int p other;
      end;
      T.finish_params p
    in
    T.select db ("SELECT id FROM t\n\
WHERE " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( id <> ? ) ")) set_params invoke_callback

  let shared_cte db ~f callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1)) in
      begin match f with
      | `All -> ()
      | `ById (a) ->
        T.set_param_Int p a;
      end;
      begin match f with
      | `All -> ()
      | `ById (b) ->
        T.set_param_Int p b;
      end;
      T.finish_params p
    in
    T.select db ("WITH recent AS (\n\
  SELECT id FROM t WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ") ^ "\n\
)\n\
SELECT id FROM recent WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) set_params invoke_callback

  let shared_mixed db ~ids ~nm ~f ~sort callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0)) in
      begin match nm with
      | None -> ()
      | Some nm ->
        T.set_param_Text p nm;
      end;
      begin match f with
      | `All -> ()
      | `ByStatus (s) ->
        T.set_param_Int p s;
      end;
      begin match f with
      | `All -> ()
      | `ByStatus (s2) ->
        T.set_param_Int p s2;
      end;
      T.finish_params p
    in
    T.select db ("SELECT id FROM t\n\
WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ "\n\
  AND " ^ (match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND (" ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ " OR id = 0)\n\
ORDER BY " ^ (match sort with `I -> " ( id ) " | `N -> " ( name ) ")) set_params invoke_callback

  module Fold = struct
    let shared_const db ~x callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with `Small -> 0 | `Big -> 0) + (match x with `Small -> 0 | `Big -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ") ^ "\n\
   OR status = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let shared_param_same_name db ~x callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with `Const -> 0 | `Param _ -> 1) + (match x with `Const -> 0 | `Param _ -> 1)) in
        begin match x with
        | `Const -> ()
        | `Param (v) ->
          T.set_param_Int p v;
        end;
        begin match x with
        | `Const -> ()
        | `Param (v) ->
          T.set_param_Int p v;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ") ^ "\n\
   OR status = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let shared_params_diff_names db ~f callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1)) in
        begin match f with
        | `All -> ()
        | `ByStatus (s) ->
          T.set_param_Int p s;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (other) ->
          T.set_param_Int p other;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( id <> ? ) ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let shared_cte db ~f callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1)) in
        begin match f with
        | `All -> ()
        | `ById (a) ->
          T.set_param_Int p a;
        end;
        begin match f with
        | `All -> ()
        | `ById (b) ->
          T.set_param_Int p b;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("WITH recent AS (\n\
  SELECT id FROM t WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ") ^ "\n\
)\n\
SELECT id FROM recent WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let shared_mixed db ~ids ~nm ~f ~sort callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0)) in
        begin match nm with
        | None -> ()
        | Some nm ->
          T.set_param_Text p nm;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (s) ->
          T.set_param_Int p s;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (s2) ->
          T.set_param_Int p s2;
        end;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ "\n\
  AND " ^ (match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND (" ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ " OR id = 0)\n\
ORDER BY " ^ (match sort with `I -> " ( id ) " | `N -> " ( name ) ")) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let shared_const db ~x callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with `Small -> 0 | `Big -> 0) + (match x with `Small -> 0 | `Big -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ") ^ "\n\
   OR status = " ^ (match x with `Small -> " ( 1 ) " | `Big -> " ( 100 ) ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let shared_param_same_name db ~x callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match x with `Const -> 0 | `Param _ -> 1) + (match x with `Const -> 0 | `Param _ -> 1)) in
        begin match x with
        | `Const -> ()
        | `Param (v) ->
          T.set_param_Int p v;
        end;
        begin match x with
        | `Const -> ()
        | `Param (v) ->
          T.set_param_Int p v;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE id = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ") ^ "\n\
   OR status = " ^ (match x with `Const -> " ( 1 ) " | `Param _ -> " ( ? ) ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let shared_params_diff_names db ~f callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1)) in
        begin match f with
        | `All -> ()
        | `ByStatus (s) ->
          T.set_param_Int p s;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (other) ->
          T.set_param_Int p other;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( id <> ? ) ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let shared_cte db ~f callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1)) in
        begin match f with
        | `All -> ()
        | `ById (a) ->
          T.set_param_Int p a;
        end;
        begin match f with
        | `All -> ()
        | `ById (b) ->
          T.set_param_Int p b;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("WITH recent AS (\n\
  SELECT id FROM t WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ") ^ "\n\
)\n\
SELECT id FROM recent WHERE " ^ (match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let shared_mixed db ~ids ~nm ~f ~sort callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0)) in
        begin match nm with
        | None -> ()
        | Some nm ->
          T.set_param_Text p nm;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (s) ->
          T.set_param_Int p s;
        end;
        begin match f with
        | `All -> ()
        | `ByStatus (s2) ->
          T.set_param_Int p s2;
        end;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id FROM t\n\
WHERE " ^ (match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")") ^ "\n\
  AND " ^ (match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ") ^ "\n\
  AND " ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ "\n\
  AND (" ^ (match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ") ^ " OR id = 0)\n\
ORDER BY " ^ (match sort with `I -> " ( id ) " | `N -> " ( name ) ")) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
