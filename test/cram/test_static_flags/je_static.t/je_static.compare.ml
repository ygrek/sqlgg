module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Wide = struct
    type brand = Stats
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("i.id");
          count = 0;
          deps = [];
        }
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("i.name");
          count = 0;
          deps = [];
        }
      let sold : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("s.sold");
          count = 0;
          deps = [Stats];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
      method sold = Cols.sold
    end

    let select db (col : _ t) ~min_id callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p min_id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ "\n\
FROM items i" ^ (if List.mem Stats col.deps then " LEFT JOIN stats s ON s.item_id = i.id" else "") ^ "\n\
WHERE i.id > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~min_id callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p min_id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM items i" ^ (if List.mem Stats col.deps then " LEFT JOIN stats s ON s.item_id = i.id" else "") ^ "\n\
WHERE i.id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~min_id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p min_id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ "\n\
FROM items i" ^ (if List.mem Stats col.deps then " LEFT JOIN stats s ON s.item_id = i.id" else "") ^ "\n\
WHERE i.id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_items db  =
    T.execute db ("CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL)") T.no_params

  let create_stats db  =
    T.execute db ("CREATE TABLE stats (item_id INT NOT NULL PRIMARY KEY, sold INT NULL)") T.no_params

  let wide_static db ~min_id callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text_nullable stmt 1)
        ~sold:(T.get_column_Int_nullable stmt 2)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p min_id;
      T.finish_params p
    in
    T.select db ("SELECT i.id, i.name, s.sold\n\
FROM items i\n\
LEFT JOIN stats s ON s.item_id = i.id\n\
WHERE i.id > ?") set_params invoke_callback

  module Fold = struct
    let wide_static db ~min_id callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text_nullable stmt 1)
          ~sold:(T.get_column_Int_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min_id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT i.id, i.name, s.sold\n\
FROM items i\n\
LEFT JOIN stats s ON s.item_id = i.id\n\
WHERE i.id > ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let wide_static db ~min_id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text_nullable stmt 1)
          ~sold:(T.get_column_Int_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min_id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT i.id, i.name, s.sold\n\
FROM items i\n\
LEFT JOIN stats s ON s.item_id = i.id\n\
WHERE i.id > ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
