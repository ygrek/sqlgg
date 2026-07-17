module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Scope_q1 = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
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
      let price : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          deps = [];
        }
      let category : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("category");
          count = 0;
          deps = [];
        }
    end
    include Cols
    let cols = object
      method id = Cols.id
      method name = Cols.name
      method price = Cols.price
      method category = Cols.category
    end

    let select db (col : _ t) ~id =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db
      ("SELECT " ^ col.column ^ " FROM products WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))

  end

  module Scope_q2 = struct
    type brand
    include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
    module Cols = struct
      let stock : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("stock");
          count = 0;
          deps = [];
        }
      let id : _ t =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
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
      method stock = Cols.stock
      method id = Cols.id
      method name = Cols.name
    end

    let select db (col : _ t) ~min_stock callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p min_stock;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM products WHERE stock > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~min_stock callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p min_stock;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM products WHERE stock > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~min_stock =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p min_stock;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM products WHERE stock > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col)) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_products db  =
    T.execute db ("CREATE TABLE products (\n\
    id INT PRIMARY KEY,\n\
    name TEXT,\n\
    price DECIMAL(10,2),\n\
    category TEXT,\n\
    stock INT\n\
)") T.no_params

  module Single = struct
  end (* module Single *)
  
  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
