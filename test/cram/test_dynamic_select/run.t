Test DynamicSelect with applicative combinators generates proper SQL:
  $ cp ../print_ocaml_impl.ml .
  $ cat dynamic_select.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c product_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe print_ocaml_impl.cmo product_id.cmo output.cmo test_run.ml
  $ ./test_run.exe
  Dynamic Select Query Generation Tests
  ==================================================
  === Starting Dynamic Select Tests ===
  
  --- Test Group 1: Basic select_one_maybe ---
  [TEST 1.1] Single field: Name
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 1.1] Completed
  
  [TEST 1.2] Single field: Price
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT price FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Decimal_nullable[0] = Some 99.990000
  [TEST 1.2] Completed
  
  [TEST 1.3] Combined fields: Name and Price using let+/and+
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name, price FROM products WHERE id = 3
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Gadget"
  [MOCK] get_column_Decimal_nullable[1] = Some 149.990000
  [TEST 1.3] Completed
  
  [TEST 1.4] Three fields: Name, Price, Category
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name, price, category FROM products WHERE id = 4
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Phone"
  [MOCK] get_column_Decimal_nullable[1] = Some 599.990000
  [MOCK] get_column_Text_nullable[2] = Some "Electronics"
  [TEST 1.4] Completed
  
  [TEST 1.5] Mapped field: Price with transformation
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT price FROM products WHERE id = 5
  [MOCK] Returning one row
  [MOCK] get_column_Decimal_nullable[0] = Some 100.000000
  [TEST 1.5] Completed
  
  [TEST 1.6] Return constructor (constant value)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  FROM products WHERE id = 6
  [MOCK] Returning one row
  [TEST 1.6] Completed
  
  --- Test Group 2: Select with callback ---
  [TEST 2.1] List with single field: Name
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT name FROM products WHERE stock > 10
  [MOCK] Returning 2 rows
    Row 0: col0=Widget 
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
    Row: col=Widget
    Row 1: col0=Gadget 
  [MOCK] get_column_Text_nullable[0] = Some "Gadget"
    Row: col=Gadget
  [TEST 2.1] Completed
  
  [TEST 2.2] List with combined fields: Id, Name and Price
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, name, price FROM products WHERE stock > 5
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Widget col2=19.99 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[2] = Some 19.990000
    Row: id=1, name=Widget, price=19.99
    Row 1: col0=2 col1=Gadget col2=29.99 
  [MOCK] get_column_Int[0] = 2
  [MOCK] get_column_Text_nullable[1] = Some "Gadget"
  [MOCK] get_column_Decimal_nullable[2] = Some 29.990000
    Row: id=2, name=Gadget, price=29.99
  [TEST 2.2] Completed
  
  --- Test Group 3: Multiple dynamic selects ---
  [TEST 3.1] Single field: label
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT CONCAT(name, ' - ', category) FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget - Electronics"
  [TEST 3.1] Completed
  
  [TEST 3.2] Combined: label and total_value
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT CONCAT(name, ' - ', category), price * stock FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget - Electronics"
  [MOCK] get_column_Decimal_nullable[1] = Some 999.500000
  [TEST 3.2] Completed
  
  --- Test Group 4: Verbatim branches ---
  [TEST 4.1] Fallback literal field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT 'N/A' FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text[0] = "N/A"
  [TEST 4.1] Completed
  
  [TEST 4.2] Combined: id, name, fallback, category
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, name, 'N/A', category FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Text[2] = "N/A"
  [MOCK] get_column_Text_nullable[3] = Some "Electronics"
  [TEST 4.2] Completed
  
  --- Test Group 5: Parameter in branch ---
  [TEST 5.1] Name field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 5.1] Completed
  
  [TEST 5.2] Custom param field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT 'Custom Value' FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text[0] = "Custom Value"
  [TEST 5.2] Completed
  
  [TEST 5.3] Combined: id, name, custom
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, name, 'Hello' FROM products WHERE id = 3
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Text[2] = "Hello"
  [TEST 5.3] Completed
  
  --- Test Group 6: Dynamic at first position ---
  [TEST 6.1] Dynamic select at first position
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 6.1] Completed
  
  [TEST 6.2] Dynamic select at first position with combinator
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name, price FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [TEST 6.2] Completed
  
  --- Test Group 7: select_one ---
  [TEST 7.1] select_one with single field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name FROM products WHERE id = 1 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 7.1] Completed
  
  [TEST 7.2] select_one with combined fields
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name, price FROM products WHERE id = 2 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [TEST 7.2] Completed
  
  --- Test Group 8: module-wrapped column ---
  [TEST 8.1] Module-wrapped column: Id
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id FROM products_wrapped WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 42
  [TEST 8.1] Completed
  
  [TEST 8.2] Module-wrapped: regular column Name
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT name FROM products_wrapped WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 8.2] Completed
  
  --- Test Group 9: IN list inside subquery branch ---
  [TEST 9.1] IN list inside subquery branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, name, (SELECT 1 FROM products WHERE price IN (1., 2.) LIMIT 1) FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int_nullable[2] = Some 1
  [TEST 9.1] Completed
  
  --- Test Group 10: arithmetic param inside branch ---
  [TEST 10.1] Arithmetic param in branch (price + tax)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, price + 20. FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Decimal_nullable[1] = Some 120.000000
  [TEST 10.1] Completed
  
  --- Test Group 11: two params inside branch ---
  [TEST 11.1] Two params in branch (range)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (10. <= price) AND (price <= 20.) FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Bool_nullable[1] = Some true
  [TEST 11.1] Completed
  
  --- Test Group 12: param + IN list inside one branch ---
  [TEST 12.1] Param + IN list in branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, CONCAT(name, '_x') IN ('a_x', 'b_x') FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Bool_nullable[1] = Some true
  [TEST 12.1] Completed
  
  --- Test Group 13: option-actions inside subquery WHERE ---
  [TEST 13.1] Option-actions in subquery (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE  TRUE  LIMIT 1) FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 13.1] Completed
  
  [TEST 13.2] Option-actions in subquery (Some)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE  (  price > 10.  )  LIMIT 1) FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 13.2] Completed
  
  --- Test Group 14: tuple list IN inside subquery WHERE ---
  [TEST 14.1] Tuple list IN inside subquery
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE (id, stock) IN ((1, 10)) LIMIT 1) FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 14.1] Completed
  
  --- Test Group 15: CASE expression inside branch ---
  [TEST 15.1] CASE expression inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, CASE WHEN id = 2 THEN 123 ELSE 0 END FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int[1] = 123
  [TEST 15.1] Completed
  
  --- Test Group 16: typed param inside branch ---
  [TEST 16.1] Typed param inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 'hello' FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text[1] = "hello"
  [TEST 16.1] Completed
  
  --- Test Group 17: monster nested scenario ---
  [TEST 17.1] Monster subquery field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT (SELECT
  CASE
  WHEN p2.id = 2 THEN
  CASE WHEN 1 = 1 THEN 'then_v' ELSE 'else_v' END
  ELSE 0
  END
  FROM products p2
  WHERE  (  p2.price > 10.  ) 
  AND p2.name IN ('a', 'b')
  AND (p2.id, p2.stock) IN ((1, 10))
  LIMIT 1)
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[0] = Some 42
  [TEST 17.1] Completed
  
  [TEST 17.2] Combined: id + monster
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT
  CASE
  WHEN p2.id = 2 THEN
  CASE WHEN 1 = 1 THEN 'then_v' ELSE 'else_v' END
  ELSE 0
  END
  FROM products p2
  WHERE  (  p2.price > 10.  ) 
  AND p2.name IN ('a', 'b')
  AND (p2.id, p2.stock) IN ((1, 10))
  LIMIT 1)
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 42
  [TEST 17.2] Completed
  
  --- Test Group 18: ultimate combo (multiple branches) ---
  [TEST 18.1] Plain stock field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  stock
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[0] = Some 100
  [TEST 18.1] Completed
  
  [TEST 18.2] IN list subquery field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  (SELECT COUNT(*) FROM products WHERE id IN (1, 2, 3))
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 3
  [TEST 18.2] Completed
  
  [TEST 18.3] Optional subquery field (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  (SELECT stock FROM products WHERE  TRUE  LIMIT 1)
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[0] = Some 50
  [TEST 18.3] Completed
  
  [TEST 18.4] CASE expression field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  CASE WHEN 1 = 1
  THEN (SELECT COUNT(*) FROM products WHERE name IN ('foo', 'bar'))
  ELSE (SELECT COUNT(*) FROM products WHERE name IN ('foo', 'bar'))
  END
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 5
  [TEST 18.4] Completed
  
  [TEST 18.5] Full combo: all fields combined
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  id, stock, (SELECT COUNT(*) FROM products WHERE id IN (1, 2)), (SELECT stock FROM products WHERE  (  stock > 5  )  LIMIT 1), CASE WHEN 1 = 1
  THEN (SELECT COUNT(*) FROM products WHERE name IN ('foo'))
  ELSE (SELECT COUNT(*) FROM products WHERE name IN ('foo'))
  END, (SELECT 1 FROM products WHERE (id, stock) IN ((1, 10)) LIMIT 1), (SELECT COUNT(*)
  FROM products p2
  WHERE  (  p2.id = 5  ) 
  AND p2.name IN ('x')
  AND p2.price > 100.)
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 100
  [MOCK] get_column_Int[2] = 3
  [MOCK] get_column_Int_nullable[3] = Some 50
  [MOCK] get_column_Int[4] = 5
  [MOCK] get_column_Int_nullable[5] = Some 1
  [MOCK] get_column_Int[6] = 42
  [TEST 18.5] Completed
  
  --- Test Group 19---
  [TEST 19] All fields combined
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT
  id, name, category, stock, price * (1 + 10)
  FROM products
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=Widget col2=Electronics col3=50 col4=119.99 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Text_nullable[2] = Some "Electronics"
  [MOCK] get_column_Int_nullable[3] = Some 50
  [MOCK] get_column_Decimal_nullable[4] = Some 119.990000
  [TEST 19] Completed
  
  --- Test Group 20: star expansion runtime ---
  [TEST 20.1] Star-only dynamic select
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT products.id, products.name, products.price, products.category, products.stock FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[2] = Some 19.990000
  [MOCK] get_column_Text_nullable[3] = Some "Electronics"
  [MOCK] get_column_Int_nullable[4] = Some 50
  [TEST 20.1] Completed
  
  --- Test Group 21: star + expr runtime ---
  [TEST 21.1] Star + expr dynamic select
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT products.id, products.name, products.price, products.category, products.stock, id + 2 FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[2] = Some 19.990000
  [MOCK] get_column_Text_nullable[3] = Some "Electronics"
  [MOCK] get_column_Int_nullable[4] = Some 50
  [MOCK] get_column_Int[5] = 3
  [TEST 21.1] Completed
  
  --- Test Group 22: multiline select-list ---
  [TEST 22.1] Multiline select-list formatting
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  id, name, price, category
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[2] = Some 19.990000
  [MOCK] get_column_Text_nullable[3] = Some "Electronics"
  [TEST 22.1] Completed
  
  === All Dynamic Select Tests Passed ===
  
  ==================================================
  All tests executed successfully!

Test DynamicSelect edge: single column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @single_col
  > SELECT id FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Single_col = struct
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
      end
      include Cols
      let cols = object
        method id = Cols.id
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: SELECT * remains static select:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @all_cols
  > SELECT * FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module All_cols = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let id : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = "t.id";
            count = 0;
            deps = [];
          }
        let name : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = "t.name";
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
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: SELECT * with expression in same list:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @all_cols_plus_expr
  > SELECT *, id + 2 AS id_plus FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module All_cols_plus_expr = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let id : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = "t.id";
            count = 0;
            deps = [];
          }
        let name : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = "t.name";
            count = 0;
            deps = [];
          }
        let id_plus : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("id + 2");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method id_plus = Cols.id_plus
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: auto names for expressions without alias:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @auto_names
  > SELECT id + 1, id * 2, name FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Auto_names = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let col1 : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("id + 1");
            count = 0;
            deps = [];
          }
        let col2 : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("id * 2");
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
        method col1 = Cols.col1
        method col2 = Cols.col2
        method name = Cols.name
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: expression at first position:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @expr_first
  > SELECT id + 1 AS id_plus FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Expr_first = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let id_plus : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("id + 1");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id_plus = Cols.id_plus
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: literal only:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @literal_only
  > SELECT 'hello' AS greeting, 42 AS answer FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Literal_only = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let greeting : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
            column = ("'hello'");
            count = 0;
            deps = [];
          }
        let answer : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
            column = ("42");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method greeting = Cols.greeting
        method answer = Cols.answer
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: many columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (a INT, b TEXT, c DECIMAL(10,2), d INT, e TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @many_cols
  > SELECT a, b, c, d, e FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Many_cols = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let a : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("a");
            count = 0;
            deps = [];
          }
        let b : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("b");
            count = 0;
            deps = [];
          }
        let c : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("c");
            count = 0;
            deps = [];
          }
        let d : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("d");
            count = 0;
            deps = [];
          }
        let e : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("e");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method a = Cols.a
        method b = Cols.b
        method c = Cols.c
        method d = Cols.d
        method e = Cols.e
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT, c DECIMAL(10,2), d INT, e TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: no space after commas:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @no_space
  > SELECT id,name,price FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module No_space = struct
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
        let price : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method price = Cols.price
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: minimal spacing:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (a INT, b INT);
  > -- [sqlgg] dynamic_select=true
  > -- @tight
  > SELECT a,b FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Tight = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let a : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("a");
            count = 0;
            deps = [];
          }
        let b : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("b");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method a = Cols.a
        method b = Cols.b
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: column without alias gets auto name:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @no_alias
  > SELECT id + 1 FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module No_alias = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let col1 : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("id + 1");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method col1 = Cols.col1
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with dynamic_select flag:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @select_ids2
  > SELECT id, balance, @t + 1 AS t_plus_one, (SELECT 6 + @seven LIMIT 1) AS sub_result FROM accounts WHERE id > @t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Select_ids2 = struct
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
        let balance : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("balance");
            count = 0;
            deps = [];
          }
        let t_plus_one t : _ t =
          let _set_t_plus_one p =
            T.set_param_Int p t;
            ()
          in
          {
            set = _set_t_plus_one;
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
            column = ("" ^ "?" ^ " + 1");
            count = 1;
            deps = [];
          }
        let sub_result seven : _ t =
          let _set_sub_result p =
            T.set_param_Int p seven;
            ()
          in
          {
            set = _set_sub_result;
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("(SELECT 6 + " ^ "?" ^ " LIMIT 1)");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method balance = Cols.balance
        method t_plus_one = Cols.t_plus_one
        method sub_result = Cols.sub_result
      end
  
      let select db (col : _ t) ~t callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p t;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col)
  
      module Fold = struct
        let select db (col : _ t) ~t callback acc =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p t;
            T.finish_params p
          in
          let r_acc = ref acc in
          IO.(>>=) (T.select db
          ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col !r_acc)))
          (fun () -> IO.return !r_acc)
  
      end (* module Fold *)
  
      module List = struct
        let select db (col : _ t) ~t callback =
          let set_params stmt =
            let p = T.start_params stmt (1 + col.count) in
            col.set p;
            T.set_param_Int p t;
            T.finish_params p
          in
          let r_acc = ref [] in
          IO.(>>=) (T.select db
          ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_accounts db  =
      T.execute db ("CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2))") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with two dynamic columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE items (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @multi_dynamic
  > SELECT id, name, price, price * 2 AS doubled_price FROM items;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Multi_dynamic = struct
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
        let price : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price");
            count = 0;
            deps = [];
          }
        let doubled_price : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price * 2");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method price = Cols.price
        method doubled_price = Cols.doubled_price
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM items")
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
          ("SELECT " ^ col.column ^ " FROM items")
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
          ("SELECT " ^ col.column ^ " FROM items")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_items db  =
      T.execute db ("CREATE TABLE items (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with Verbatim branches:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, status TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @with_verbatim
  > SELECT id, status, 'active' AS literal_status FROM users;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module With_verbatim = struct
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
        let status : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("status");
            count = 0;
            deps = [];
          }
        let literal_status : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
            column = ("'active'");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method status = Cols.status
        method literal_status = Cols.literal_status
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM users")
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
          ("SELECT " ^ col.column ^ " FROM users")
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
          ("SELECT " ^ col.column ^ " FROM users")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (id INT, status TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect at beginning of SELECT:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE data (a INT, b TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @first_col
  > SELECT a, b FROM data;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module First_col = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let a : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("a");
            count = 0;
            deps = [];
          }
        let b : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("b");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method a = Cols.a
        method b = Cols.b
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM data")
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
          ("SELECT " ^ col.column ^ " FROM data")
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
          ("SELECT " ^ col.column ^ " FROM data")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_data db  =
      T.execute db ("CREATE TABLE data (a INT, b TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect disabled in subquery (fallback to Choice):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE t1 (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @with_subquery
  > SELECT id, (SELECT @x { A { 1 } | B { 2 } } LIMIT 1) as sub FROM t1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module With_subquery = struct
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
        let sub x : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("(SELECT " ^ (match x with `A -> " ( 1 ) " | `B -> " ( 2 ) ") ^ " LIMIT 1)");
            count = 0 + (match x with `A -> 0 | `B -> 0);
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method sub = Cols.sub
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t1")
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
          ("SELECT " ^ col.column ^ " FROM t1")
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
          ("SELECT " ^ col.column ^ " FROM t1")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t1 db  =
      T.execute db ("CREATE TABLE t1 (id INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with module annotation:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE wrapped (
  >     -- [sqlgg] module=Product_id
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     price DECIMAL(10,2)
  > );
  > -- [sqlgg] dynamic_select=true
  > -- @with_module
  > SELECT id, name, price FROM wrapped WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module With_module = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let id : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (Product_id.get_column (T.get_column_int64 row idx), idx + 1));
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
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method price = Cols.price
      end
  
      let select db (col : _ t) ~id =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_int64 p (Product_id.set_param id);
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM wrapped WHERE id = ?")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    end
  
  
    let create_wrapped db  =
      T.execute db ("CREATE TABLE wrapped (\n\
          id INT PRIMARY KEY,\n\
      name TEXT,\n\
      price DECIMAL(10,2)\n\
  )") T.no_params
  
    module Single = struct
    end (* module Single *)
  end (* module Sqlgg *)

Test DynamicSelect with LIMIT 1 (select_one):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @select_one_product
  > SELECT name, price FROM products WHERE id = @id LIMIT 1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Select_one_product = struct
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
        let price : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method name = Cols.name
        method price = Cols.price
      end
  
      let select db (col : _ t) ~id =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM products WHERE id = ? LIMIT 1")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price DECIMAL(10,2))") T.no_params
  
    module Single = struct
    end (* module Single *)
  end (* module Sqlgg *)

Test DynamicSelect comprehensive list:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE products (
  >  id INT PRIMARY KEY,
  >  name TEXT,
  >  price DECIMAL(10,2),
  >  category TEXT,
  >  stock INT
  > );
  > -- [sqlgg] dynamic_select=true
  > -- @ultimate_combo_simple2
  > SELECT
  >    id,
  >    name,
  >    category, stock,
  >    price * (1 + @tax_rate) AS price_with_tax
  > FROM products;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Ultimate_combo_simple2 = struct
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
        let category : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("category");
            count = 0;
            deps = [];
          }
        let stock : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("stock");
            count = 0;
            deps = [];
          }
        let price_with_tax tax_rate : _ t =
          let _set_price_with_tax p =
            T.set_param_Int p tax_rate;
            ()
          in
          {
            set = _set_price_with_tax;
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price * (1 + " ^ "?" ^ ")");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method category = Cols.category
        method stock = Cols.stock
        method price_with_tax = Cols.price_with_tax
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
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
          ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
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
          ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
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
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: param as bare column expression (spacing at ctor boundary):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, val TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @bare_param
  > SELECT id, @custom_val :: Text AS custom FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Bare_param = struct
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
        let custom custom_val : _ t =
          let _set_custom p =
            T.set_param_Text p custom_val;
            ()
          in
          {
            set = _set_custom;
            read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
            column = ("" ^ "?");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method custom = Cols.custom
      end
  
      let select db (col : _ t) ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, val TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: consecutive params as columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @multi_param
  > SELECT @a :: Int AS col_a, @b :: Text AS col_b FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Multi_param = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let col_a a : _ t =
          let _set_col_a p =
            T.set_param_Int p a;
            ()
          in
          {
            set = _set_col_a;
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
            column = ("" ^ "?");
            count = 1;
            deps = [];
          }
        let col_b b : _ t =
          let _set_col_b p =
            T.set_param_Text p b;
            ()
          in
          {
            set = _set_col_b;
            read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
            column = ("" ^ "?");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method col_a = Cols.col_a
        method col_b = Cols.col_b
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: mixed columns and params without spaces after commas:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @tight_commas
  > SELECT id,name,price,@extra :: Int AS bonus FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Tight_commas = struct
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
        let price : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("price");
            count = 0;
            deps = [];
          }
        let bonus extra : _ t =
          let _set_bonus p =
            T.set_param_Int p extra;
            ()
          in
          {
            set = _set_bonus;
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
            column = ("" ^ "?");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
        method price = Cols.price
        method bonus = Cols.bonus
      end
  
      let select db (col : _ t) ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: subquery expression as dynamic column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @subquery_col
  > SELECT id, (SELECT COUNT(*) FROM t t2 WHERE t2.id <= t.id) AS rank FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Subquery_col = struct
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
        let rank : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
            column = ("(SELECT COUNT(*) FROM t t2 WHERE t2.id <= t.id)");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method rank = Cols.rank
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: CASE WHEN as dynamic column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, status INT);
  > -- [sqlgg] dynamic_select=true
  > -- @case_col
  > SELECT id, CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END AS label FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Case_col = struct
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
        let label : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
            column = ("CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method label = Cols.label
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, status INT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: function call with multiple args as column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, first_name TEXT, last_name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @func_col
  > SELECT id, CONCAT(first_name, ' ', last_name) AS full_name FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Func_col = struct
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
        let full_name : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("CONCAT(first_name, ' ', last_name)");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method full_name = Cols.full_name
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, first_name TEXT, last_name TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: arithmetic with param at expression start:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @param_start_expr
  > SELECT id, @multiplier * price AS scaled FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Param_start_expr = struct
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
        let scaled multiplier : _ t =
          let _set_scaled p =
            begin match multiplier with None -> T.set_param_null p | Some v -> T.set_param_Decimal p v end;
            ()
          in
          {
            set = _set_scaled;
            read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
            column = ("" ^ "?" ^ " * price");
            count = 1;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method id = Cols.id
        method scaled = Cols.scaled
      end
  
      let select db (col : _ t) ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
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
          ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, price DECIMAL(10,2))") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: explicit choices with alias alongside plain columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, category TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @mixed_explicit
  > SELECT id, @col { Name { name } | Cat { category } } AS detail FROM t WHERE id = @id;
  > EOF
  Failed mixed_explicit: SELECT id, @col { Name { name } | Cat { category } } AS detail FROM t WHERE id = @id
  At : @col { Name { name } | Cat { category } }
  Fatal error: exception Failure("sharing choices not implemented")
  [2]

Virtual select: tab-separated columns (non-space whitespace):
  $ printf 'CREATE TABLE t (a INT, b TEXT);\n-- [sqlgg] dynamic_select=true\n-- @tab_sep\nSELECT a,\tb FROM t;\n' | sqlgg -gen caml -no-header -dialect=mysql - 2>&1
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Tab_sep = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
        let a : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
            column = ("a");
            count = 0;
            deps = [];
          }
        let b : _ t =
          {
            set = (fun _p -> ());
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
            column = ("b");
            count = 0;
            deps = [];
          }
      end
      include Cols
      let cols = object
        method a = Cols.a
        method b = Cols.b
      end
  
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        T.select db
        ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
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
          ("SELECT " ^ col.column ^ " FROM t")
          set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            __sqlgg_r_col) :: !r_acc))
          (fun () -> IO.return (List.rev !r_acc))
  
      end (* module List *)
  
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT)") T.no_params
  
    module Fold = struct
    end (* module Fold *)
    
    module List = struct
    end (* module List *)
  end (* module Sqlgg *)

