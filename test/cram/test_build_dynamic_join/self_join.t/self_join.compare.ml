module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Bad = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u1.id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("u2.name");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Good = struct
    type brand = Users
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u1.id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("u2.name");
          count = 0;
          deps = [Users];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users u1" ^ (if List.mem Users col.deps then " LEFT JOIN users u2 ON u2.id = u1.manager_id" else ""))
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u1" ^ (if List.mem Users col.deps then " LEFT JOIN users u2 ON u2.id = u1.manager_id" else ""))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u1" ^ (if List.mem Users col.deps then " LEFT JOIN users u2 ON u2.id = u1.manager_id" else ""))
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_users db  =
    T.execute db ("CREATE TABLE users (id INT PRIMARY KEY, name TEXT, manager_id INT)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
