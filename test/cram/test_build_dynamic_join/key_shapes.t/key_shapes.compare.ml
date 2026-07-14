module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Unique_key = struct
    type brand = Accounts
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u.id");
          count = 0;
          deps = [];
        }
      let label : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("a.label");
          count = 0;
          deps = [Accounts];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method label = Cols.label
    end

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Accounts col.deps then " LEFT JOIN accounts a ON a.email = u.email" else "") ^ " WHERE u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Accounts col.deps then " LEFT JOIN accounts a ON a.email = u.email" else "") ^ " WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Accounts col.deps then " LEFT JOIN accounts a ON a.email = u.email" else "") ^ " WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Composite_partial = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u.id");
          count = 0;
          deps = [];
        }
      let title : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("m.title");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method title = Cols.title
    end

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Composite_full = struct
    type brand = Memberships
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("u.id");
          count = 0;
          deps = [];
        }
      let title : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("m.title");
          count = 0;
          deps = [Memberships];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method title = Cols.title
    end

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Memberships col.deps then " LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept" else "") ^ " WHERE u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Memberships col.deps then " LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept" else "") ^ " WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users u" ^ (if List.mem Memberships col.deps then " LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept" else "") ^ " WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_users db  =
    T.execute db ("CREATE TABLE users (id INT PRIMARY KEY, name TEXT, email TEXT, org INT, dept INT)") T.no_params

  let create_accounts db  =
    T.execute db ("CREATE TABLE accounts (id INT PRIMARY KEY, email TEXT UNIQUE, label TEXT)") T.no_params

  let create_memberships db  =
    T.execute db ("CREATE TABLE memberships (org INT, dept INT, title TEXT, PRIMARY KEY (org, dept))") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
