module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Dynamic_not_null = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
      let descr : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("descr");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method name = Cols.name
      method descr = Cols.descr
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
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
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL AND descr IS NOT NULL") ~name:"dynamic_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Dynamic_or_stays_nullable = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let name : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          deps = [];
        }
      let descr : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("descr");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method name = Cols.name
      method descr = Cols.descr
    end

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
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
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        (Sqlgg_traits.Query.make ~sql:("SELECT " ^ col.column ^ " FROM items WHERE name IS NOT NULL OR descr IS NOT NULL") ~name:"dynamic_or_stays_nullable" ~kind:Sqlgg_traits.Query.(Select Nat) ())
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_items db  =
    T.execute_unprepared db ("CREATE TABLE items (\n\
  id INT NOT NULL,\n\
  name TEXT NULL,\n\
  descr TEXT NULL\n\
)")

  let static_not_null db  callback =
    let invoke_callback stmt =
      callback
        ~name:(T.get_column_Text stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  module Fold = struct
    let static_not_null db  callback acc =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let static_not_null db  callback =
      let invoke_callback stmt =
        callback
          ~name:(T.get_column_Text stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT name FROM items WHERE name IS NOT NULL") ~name:"static_not_null" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
