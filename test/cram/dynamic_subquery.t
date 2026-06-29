Dynamic select penetrates a pass-through (SELECT *) subquery source: the dynamic
column placeholder is pushed down INTO the subquery. A non-pass-through outer query
keeps the dynamic placeholder at the outer level (subquery projection stays fixed).

  $ cat > dyn_subq.sql <<'EOF'
  > CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price INT);
  > -- [sqlgg] dynamic_select=true
  > -- @products_dyn
  > SELECT * FROM (SELECT id, name, price FROM products WHERE price > @min) AS sub;
  > -- [sqlgg] dynamic_select=true
  > -- @cols_over_subq
  > SELECT sub.id, sub.name FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = @id;
  > EOF

Full generated code:

  $ cat dyn_subq.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - | tee output.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Products_dyn_col = struct
      type 'a t = {
        set: T.params -> unit;
        read: T.row -> int -> 'a * int;
        column: string;
        count: int;
      }
  
      let pure x = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      }
  
      let apply f a = {
        set = (fun p -> f.set p; a.set p);
        read = (fun row idx ->
          let (vf, i1) = f.read row idx in
          let (va, i2) = a.read row i1 in
          (vf va, i2));
        column = (match f.column, a.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.count + a.count;
      }
  
      let map f a = apply (pure f) a
  
      let (let+) t f = map f t
      let (and+) a b = apply (map (fun a b -> (a, b)) a) b
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
  
      let select db (col : _ t) ~min callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p min;
          T.finish_params p
        in
        T.select db
        ("SELECT * FROM (SELECT " ^ col.column ^ " FROM products WHERE price > ?) AS sub")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col)
  
      module Fold = struct
        let select db (col : _ t) ~min callback acc =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p min;
            T.finish_params p
          in
          let r_acc = ref acc in
          IO.(>>=) (T.select db
          ("SELECT * FROM (SELECT " ^ col.column ^ " FROM products WHERE price > ?) AS sub")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col !r_acc)))
          (fun () -> IO.return !r_acc)
  
      end (* module Fold *)
  
      module List = struct
        let select db (col : _ t) ~min callback =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p min;
            T.finish_params p
          in
          let r_acc = ref [] in
          IO.(>>=) (T.select db
          ("SELECT * FROM (SELECT " ^ col.column ^ " FROM products WHERE price > ?) AS sub")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
    module Cols_over_subq_col = struct
      type 'a t = {
        set: T.params -> unit;
        read: T.row -> int -> 'a * int;
        column: string;
        count: int;
      }
  
      let pure x = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      }
  
      let apply f a = {
        set = (fun p -> f.set p; a.set p);
        read = (fun row idx ->
          let (vf, i1) = f.read row idx in
          let (va, i2) = a.read row i1 in
          (vf va, i2));
        column = (match f.column, a.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.count + a.count;
      }
  
      let map f a = apply (pure f) a
  
      let (let+) t f = map f t
      let (and+) a b = apply (map (fun a b -> (a, b)) a) b
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("sub.id");
          count = 0;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("sub.name");
          count = 0;
        }
  
      let select db (col : _ t) ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = ?")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col)
  
      module Fold = struct
        let select db (col : _ t) ~id callback acc =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p id;
            T.finish_params p
          in
          let r_acc = ref acc in
          IO.(>>=) (T.select db
          ("SELECT " ^ col.column ^ " FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col !r_acc)))
          (fun () -> IO.return !r_acc)
  
      end (* module Fold *)
  
      module List = struct
        let select db (col : _ t) ~id callback =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p id;
            T.finish_params p
          in
          let r_acc = ref [] in
          IO.(>>=) (T.select db
          ("SELECT " ^ col.column ^ " FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Compile the generated module together with the printing mock traits implementation:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml

A tiny driver that picks different fields at runtime and lets the mock print the SQL
that is actually sent (with the chosen columns spliced into the subquery):

  $ cat > run_subq.ml <<'EOF'
  > module S = Output.Sqlgg(Print_impl)
  > open S.Products_dyn_col
  > 
  > let run label col =
  >   Printf.printf "=== %s ===\n%!" label;
  >   Print_impl.clear_mock_responses ();
  >   Print_impl.setup_select_response [];
  >   ignore (List.select () col ~min:10L (fun x -> x))
  > 
  > let () =
  >   run "pick id" id;
  >   run "pick name" name;
  >   run "pick id + name + price" (let+ i = id and+ n = name and+ p = price in (i, n, p))
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c run_subq.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run_subq.exe output.ml print_impl.ml run_subq.ml

Run it and observe the final SQL per field selection:

  $ ./run_subq.exe 2>&1 | grep -E '^===|^\[SQL\]'
  === pick id ===
  [SQL] SELECT * FROM (SELECT id FROM products WHERE price > 10) AS sub
  === pick name ===
  [SQL] SELECT * FROM (SELECT name FROM products WHERE price > 10) AS sub
  === pick id + name + price ===
  [SQL] SELECT * FROM (SELECT id, name, price FROM products WHERE price > 10) AS sub
