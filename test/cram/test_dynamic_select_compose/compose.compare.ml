module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module In_list = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~ids ~nm callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
        col.set p;
        T.set_param_Text p nm;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~ids ~nm callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.set_param_Text p nm;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~ids ~nm =
        let set_params stmt =
          let p = T.start_params stmt (1 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.set_param_Text p nm;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Tuple_list = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~pairs callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~pairs callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~pairs =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Opt_filter = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~v callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match v with Some _ -> 1 | None -> 0) + col.count) in
        col.set p;
        begin match v with
        | None -> ()
        | Some v ->
          T.set_param_Int p v;
        end;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match v with Some _ -> " ( " ^ " id = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~v callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match v with Some _ -> 1 | None -> 0) + col.count) in
          col.set p;
          begin match v with
          | None -> ()
          | Some v ->
            T.set_param_Int p v;
          end;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match v with Some _ -> " ( " ^ " id = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~v =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match v with Some _ -> 1 | None -> 0) + col.count) in
          col.set p;
          begin match v with
          | None -> ()
          | Some v ->
            T.set_param_Int p v;
          end;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match v with Some _ -> " ( " ^ " id = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Order_choice = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~sort callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~sort callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~sort =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Order_comma_choice = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~sort callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t ORDER BY id" ^ "," ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~sort callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t ORDER BY id" ^ "," ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~sort =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match sort with `I -> 0 | `N -> 0) + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t ORDER BY id" ^ "," ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Where_choice = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~f callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
        col.set p;
        begin match f with
        | `All -> ()
        | `ById (id) ->
          T.set_param_Int p id;
        end;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~f callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          col.set p;
          begin match f with
          | `All -> ()
          | `ById (id) ->
            T.set_param_Int p id;
          end;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~f =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          col.set p;
          begin match f with
          | `All -> ()
          | `ById (id) ->
            T.set_param_Int p id;
          end;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Shared_choice = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~f callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
        col.set p;
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
      T.select db
      ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~f callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          col.set p;
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
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~f =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          col.set p;
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
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Kitchen_sink = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~ids ~pairs ~nm ~f ~sort callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match pairs with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0) + col.count) in
        col.set p;
        begin match nm with
        | None -> ()
        | Some nm ->
          T.set_param_Text p nm;
        end;
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
      T.select db
      ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ "\n\
  AND " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ^ "\n\
  AND " ^ ((match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ "\n\
  AND " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)\n\
ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~ids ~pairs ~nm ~f ~sort callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match pairs with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0) + col.count) in
          col.set p;
          begin match nm with
          | None -> ()
          | Some nm ->
            T.set_param_Text p nm;
          end;
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
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ "\n\
  AND " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ^ "\n\
  AND " ^ ((match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ "\n\
  AND " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)\n\
ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~ids ~pairs ~nm ~f ~sort =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match ids with [] -> 0 | _ :: _ -> 0) + (match pairs with [] -> 0 | _ :: _ -> 0) + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + (match sort with `I -> 0 | `N -> 0) + (match nm with Some _ -> 1 | None -> 0) + col.count) in
          col.set p;
          begin match nm with
          | None -> ()
          | Some nm ->
            T.set_param_Text p nm;
          end;
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
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ "\n\
  AND " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")) ^ "\n\
  AND " ^ ((match nm with Some _ -> " ( " ^ " name = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ "\n\
  AND " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ "\n\
  AND (" ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ " OR id = 0)\n\
ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_t db  =
    T.execute db ("CREATE TABLE t (\n\
    id INT,\n\
    name TEXT\n\
)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
