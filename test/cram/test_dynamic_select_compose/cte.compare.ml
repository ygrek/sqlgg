module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Cte_plain = struct
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

    let select db ~st (col : _ t) ~lo callback =
      let set_params stmt =
        let p = T.start_params stmt (2 + col.count) in
        T.set_param_Int p st;
        col.set p;
        T.set_param_Int p lo;
        T.finish_params p
      in
      T.select db
      ("WITH filtered AS (SELECT id, name FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db ~st (col : _ t) ~lo callback acc =
        let set_params stmt =
          let p = T.start_params stmt (2 + col.count) in
          T.set_param_Int p st;
          col.set p;
          T.set_param_Int p lo;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db ~st (col : _ t) ~lo callback =
        let set_params stmt =
          let p = T.start_params stmt (2 + col.count) in
          T.set_param_Int p st;
          col.set p;
          T.set_param_Int p lo;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Cte_in_list = struct
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

    let select db ~ids ~nm (col : _ t) ~lo callback =
      let set_params stmt =
        let p = T.start_params stmt (2 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
        T.set_param_Text p nm;
        col.set p;
        T.set_param_Int p lo;
        T.finish_params p
      in
      T.select db
      ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db ~ids ~nm (col : _ t) ~lo callback acc =
        let set_params stmt =
          let p = T.start_params stmt (2 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
          T.set_param_Text p nm;
          col.set p;
          T.set_param_Int p lo;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db ~ids ~nm (col : _ t) ~lo callback =
        let set_params stmt =
          let p = T.start_params stmt (2 + (match ids with [] -> 0 | _ :: _ -> 0) + col.count) in
          T.set_param_Text p nm;
          col.set p;
          T.set_param_Int p lo;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match ids with [] -> "FALSE" | _ :: _ -> "id IN " ^  "(" ^ String.concat ", " (List.map T.Types.Int.to_literal ids) ^ ")")) ^ " AND name = ?)\n\
SELECT " ^ col.column ^ " FROM filtered WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Cte_choice = struct
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

    let select db ~f (col : _ t) ~sort callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + col.count) in
        begin match f with
        | `All -> ()
        | `ByStatus (s) ->
          T.set_param_Int p s;
        end;
        col.set p;
        T.finish_params p
      in
      T.select db
      ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db ~f (col : _ t) ~sort callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + col.count) in
          begin match f with
          | `All -> ()
          | `ByStatus (s) ->
            T.set_param_Int p s;
          end;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db ~f (col : _ t) ~sort callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ByStatus _ -> 1) + (match sort with `I -> 0 | `N -> 0) + col.count) in
          begin match f with
          | `All -> ()
          | `ByStatus (s) ->
            T.set_param_Int p s;
          end;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ByStatus _ -> " ( status = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered ORDER BY " ^ ((match sort with `I -> " ( id ) " | `N -> " ( name ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Cte_opt_filter = struct
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

    let select db ~st (col : _ t) ~pairs callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + (match st with Some _ -> 1 | None -> 0) + col.count) in
        begin match st with
        | None -> ()
        | Some st ->
          T.set_param_Int p st;
        end;
        col.set p;
        T.finish_params p
      in
      T.select db
      ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match st with Some _ -> " ( " ^ " status = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db ~st (col : _ t) ~pairs callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + (match st with Some _ -> 1 | None -> 0) + col.count) in
          begin match st with
          | None -> ()
          | Some st ->
            T.set_param_Int p st;
          end;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match st with Some _ -> " ( " ^ " status = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db ~st (col : _ t) ~pairs callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match pairs with [] -> 0 | _ :: _ -> 0) + (match st with Some _ -> 1 | None -> 0) + col.count) in
          begin match st with
          | None -> ()
          | Some st ->
            T.set_param_Int p st;
          end;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match st with Some _ -> " ( " ^ " status = " ^ "?" ^ " " ^ " ) " | None -> " TRUE ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match pairs with [] -> "FALSE" | _ :: _ -> "(id, name) IN " ^ "(" ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (pairs_0n, pairs_1n) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (match pairs_0n with None -> "NULL" | Some v -> T.Types.Int.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match pairs_1n with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_char _sqlgg_b ')') pairs; Buffer.contents _sqlgg_b) ^ ")")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Cte_shared_choice = struct
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
        begin match f with
        | `All -> ()
        | `ById (a) ->
          T.set_param_Int p a;
        end;
        col.set p;
        begin match f with
        | `All -> ()
        | `ById (b) ->
          T.set_param_Int p b;
        end;
        T.finish_params p
      in
      T.select db
      ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~f callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          begin match f with
          | `All -> ()
          | `ById (a) ->
            T.set_param_Int p a;
          end;
          col.set p;
          begin match f with
          | `All -> ()
          | `ById (b) ->
            T.set_param_Int p b;
          end;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~f callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `All -> 0 | `ById _ -> 1) + (match f with `All -> 0 | `ById _ -> 1) + col.count) in
          begin match f with
          | `All -> ()
          | `ById (a) ->
            T.set_param_Int p a;
          end;
          col.set p;
          begin match f with
          | `All -> ()
          | `ById (b) ->
            T.set_param_Int p b;
          end;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH filtered AS (SELECT id, name FROM t WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")) ^ ")\n\
SELECT " ^ col.column ^ " FROM filtered WHERE " ^ ((match f with `All -> " ( TRUE ) " | `ById _ -> " ( id = ? ) ")))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Cte_param_col = struct
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
      let is_new cutoff : _ t =
        let _set_is_new p =
          begin match cutoff with None -> T.set_param_null p | Some v -> T.set_param_Int p v end;
          ()
        in
        {
          set = _set_is_new;
          read = (fun row idx -> (T.get_column_Bool_nullable row idx, idx + 1));
          column = ("(status >= " ^ "?" ^ ")");
          count = 1;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
      method is_new = Cols.is_new
    end

    let select db ~st (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        T.set_param_Int p st;
        col.set p;
        T.finish_params p
      in
      T.select db
      ("WITH recent AS (SELECT id, name, status FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM recent")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db ~st (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          T.set_param_Int p st;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("WITH recent AS (SELECT id, name, status FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM recent")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db ~st (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          T.set_param_Int p st;
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("WITH recent AS (SELECT id, name, status FROM t WHERE status = ?)\n\
SELECT " ^ col.column ^ " FROM recent")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Where_subquery = struct
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

    let select db (col : _ t) ~st ~names callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + (match names with [] -> 0 | _ :: _ -> 0) + col.count) in
        col.set p;
        T.set_param_Int p st;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t\n\
WHERE id IN (SELECT id FROM t WHERE status = ? AND " ^ ((match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ^ ")")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~st ~names callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + (match names with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.set_param_Int p st;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE id IN (SELECT id FROM t WHERE status = ? AND " ^ ((match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ^ ")")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~st ~names callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + (match names with [] -> 0 | _ :: _ -> 0) + col.count) in
          col.set p;
          T.set_param_Int p st;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t\n\
WHERE id IN (SELECT id FROM t WHERE status = ? AND " ^ ((match names with [] -> "FALSE" | _ :: _ -> "name IN " ^  "(" ^ String.concat ", " (List.map T.Types.Text.to_literal names) ^ ")")) ^ ")")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_t db  =
    T.execute db ("CREATE TABLE t (\n\
    id INT,\n\
    name TEXT,\n\
    status INT\n\
)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
