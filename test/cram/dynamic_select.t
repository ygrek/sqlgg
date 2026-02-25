Test DynamicSelect with applicative combinators generates proper SQL:
  $ cp test_build_dynamic_select/dynamic_select.sql .
  $ cp test_build_dynamic_select/product_id.ml .
  $ cp test_build_dynamic_select/test_run.ml .
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
  [SQL] SELECT CONCAT(name, ' - ', category) AS total_value FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget - Electronics"
  [TEST 3.1] Completed
  
  [TEST 3.2] Combined: label and total_value
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT CONCAT(name, ' - ', category), price * stock AS total_value FROM products WHERE id = 2
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
  [SQL] SELECT name AS custom FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 5.1] Completed
  
  [TEST 5.2] Custom param field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT 'Custom Value' AS custom FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text[0] = "Custom Value"
  [TEST 5.2] Completed
  
  [TEST 5.3] Combined: id, name, custom
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, name, 'Hello' AS custom FROM products WHERE id = 3
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
  [SQL] SELECT id, name, (SELECT 1 FROM products WHERE price IN (1., 2.) LIMIT 1) AS filtered FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int_nullable[2] = Some 1
  [TEST 9.1] Completed
  
  --- Test Group 10: arithmetic param inside branch ---
  [TEST 10.1] Arithmetic param in branch (price + tax)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, price + 20. AS add_tax FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Decimal_nullable[1] = Some 120.000000
  [TEST 10.1] Completed
  
  --- Test Group 11: two params inside branch ---
  [TEST 11.1] Two params in branch (range)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (10. <= price) AND (price <= 20.) AS in_range FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Bool_nullable[1] = Some true
  [TEST 11.1] Completed
  
  --- Test Group 12: param + IN list inside one branch ---
  [TEST 12.1] Param + IN list in branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, CONCAT(name, '_x') IN ('a_x', 'b_x') AS match_ FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Bool_nullable[1] = Some true
  [TEST 12.1] Completed
  
  --- Test Group 13: option-actions inside subquery WHERE ---
  [TEST 13.1] Option-actions in subquery (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE  TRUE  LIMIT 1) AS opt FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 13.1] Completed
  
  [TEST 13.2] Option-actions in subquery (Some)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE  (  price > 10.  )  LIMIT 1) AS opt FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 13.2] Completed
  
  --- Test Group 14: tuple list IN inside subquery WHERE ---
  [TEST 14.1] Tuple list IN inside subquery
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, (SELECT 1 FROM products WHERE (id, stock) IN ((1, 10)) LIMIT 1) AS pairs FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 1
  [TEST 14.1] Completed
  
  --- Test Group 15: CASE expression inside branch ---
  [TEST 15.1] CASE expression inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, CASE WHEN id = 2 THEN 123 ELSE 0 END AS casey FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int[1] = 123
  [TEST 15.1] Completed
  
  --- Test Group 16: typed param inside branch ---
  [TEST 16.1] Typed param inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 'hello' AS typed FROM products WHERE id = 1
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
  LIMIT 1) AS monster
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
  LIMIT 1) AS monster
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int_nullable[1] = Some 42
  [TEST 17.2] Completed
  
  --- Test Group 18: ultimate combo (multiple branches) ---
  [TEST 18.1] Plain stock field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  stock AS full_combo
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[0] = Some 100
  [TEST 18.1] Completed
  
  [TEST 18.2] IN list subquery field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  (SELECT COUNT(*) FROM products WHERE id IN (1, 2, 3)) AS full_combo
  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 3
  [TEST 18.2] Completed
  
  [TEST 18.3] Optional subquery field (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT
  (SELECT stock FROM products WHERE  TRUE  LIMIT 1) AS full_combo
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
  END AS full_combo
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
  AND p2.price > 100.) AS full_combo
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
  id, name, category, stock, price * (1 + 10) AS price_with_tax
  FROM products
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=Widget col2=Electronics col3=50 col4=119.99 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Text_nullable[2] = Some "Electronics"
  [MOCK] get_column_Int_nullable[3] = Some 50
  [MOCK] get_column_Decimal_nullable[4] = Some 119.990000
  [TEST 19] Completed
  
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
  
    type 'a field_data = {
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
    module Single_col_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let single_col db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let single_col db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let single_col db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Expr_first_col = struct
      let id_plus =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id + 1");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let expr_first db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS id_plus FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let expr_first db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS id_plus FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let expr_first db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS id_plus FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Literal_only_col = struct
      let greeting =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("'hello'");
          count = 0;
        }
      let answer =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("42");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    let literal_only db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS answer FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let literal_only db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS answer FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let literal_only db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS answer FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Many_cols_col = struct
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
        }
      let c =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("c");
          count = 0;
        }
      let d =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("d");
          count = 0;
        }
      let e =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("e");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT, c DECIMAL(10,2), d INT, e TEXT)") T.no_params
  
    let many_cols db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let many_cols db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let many_cols db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module No_space_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
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
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let no_space db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let no_space db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let no_space db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Tight_col = struct
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b INT)") T.no_params
  
    let tight db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tight db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tight db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: column without alias fails:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @no_alias
  > SELECT id + 1 FROM t;
  > EOF
  Failed no_alias: SELECT id + 1 FROM t
  Fatal error: exception Failure("dynamic_select requires AS alias for all columns")
  [2]

Test DynamicSelect with dynamic_select flag:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @select_ids2
  > SELECT id, balance, @t + 1 AS t_plus_one, (SELECT 6 + @seven LIMIT 1) AS sub_result FROM accounts WHERE id > @t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    type 'a field_data = {
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
    module Select_ids2_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let balance =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("balance");
          count = 0;
        }
      let t_plus_one t =
        let _set_t_plus_one p =
          T.set_param_Int p t;
          ()
        in
        {
          set = _set_t_plus_one;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?" ^ " + 1");
          count = 1;
        }
      let sub_result seven =
        let _set_sub_result p =
          T.set_param_Int p seven;
          ()
        in
        {
          set = _set_sub_result;
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("(SELECT 6 + " ^ "?" ^ " LIMIT 1)");
          count = 1;
        }
    end
  
  
    let create_accounts db  =
      T.execute db ("CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2))") T.no_params
  
    let select_ids2 db ~col ~t callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p t;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS sub_result FROM accounts WHERE id > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let select_ids2 db ~col ~t callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p t;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS sub_result FROM accounts WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_ids2 db ~col ~t callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p t;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS sub_result FROM accounts WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Multi_dynamic_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
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
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
      let doubled_price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price * 2");
          count = 0;
        }
    end
  
  
    let create_items db  =
      T.execute db ("CREATE TABLE items (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let multi_dynamic db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS doubled_price FROM items")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let multi_dynamic db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS doubled_price FROM items")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let multi_dynamic db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS doubled_price FROM items")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module With_verbatim_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let status =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("status");
          count = 0;
        }
      let literal_status =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("'active'");
          count = 0;
        }
    end
  
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (id INT, status TEXT)") T.no_params
  
    let with_verbatim db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS literal_status FROM users")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let with_verbatim db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS literal_status FROM users")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let with_verbatim db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS literal_status FROM users")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module First_col_col = struct
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
        }
    end
  
  
    let create_data db  =
      T.execute db ("CREATE TABLE data (a INT, b TEXT)") T.no_params
  
    let first_col db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM data")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let first_col db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM data")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let first_col db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM data")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module With_subquery_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let sub x =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("(SELECT " ^ (match x with `A -> " 1 " | `B -> " 2 ") ^ " LIMIT 1)");
          count = 0 + (match x with `A -> 0 | `B -> 0);
        }
    end
  
  
    let create_t1 db  =
      T.execute db ("CREATE TABLE t1 (id INT)") T.no_params
  
    let with_subquery db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " as sub FROM t1")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let with_subquery db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " as sub FROM t1")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let with_subquery db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " as sub FROM t1")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module With_module_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (Product_id.get_column (T.get_column_int64 row idx), idx + 1));
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
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
    end
  
  
    let create_wrapped db  =
      T.execute db ("CREATE TABLE wrapped (\n\
          id INT PRIMARY KEY,\n\
      name TEXT,\n\
      price DECIMAL(10,2)\n\
  )") T.no_params
  
    let with_module db ~col ~id =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_int64 p (Product_id.set_param id);
        T.finish_params p
      in
      T.select_one_maybe db
      ("SELECT " ^ col.column ^ " FROM wrapped WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    module Single = struct
      let with_module db ~col ~id =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_int64 p (Product_id.set_param id);
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM wrapped WHERE id = ?")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
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
  
    type 'a field_data = {
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
    module Select_one_product_col = struct
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
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let select_one_product db ~col ~id =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db
      ("SELECT " ^ col.column ^ " FROM products WHERE id = ? LIMIT 1")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    module Single = struct
      let select_one_product db ~col ~id =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM products WHERE id = ? LIMIT 1")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
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
  
    type 'a field_data = {
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
    module Ultimate_combo_simple2_col = struct
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
      let category =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("category");
          count = 0;
        }
      let stock =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("stock");
          count = 0;
        }
      let price_with_tax tax_rate =
        let _set_price_with_tax p =
          T.set_param_Int p tax_rate;
          ()
        in
        {
          set = _set_price_with_tax;
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price * (1 + " ^ "?" ^ ")");
          count = 1;
        }
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (\n\
   id INT PRIMARY KEY,\n\
   name TEXT,\n\
   price DECIMAL(10,2),\n\
   category TEXT,\n\
   stock INT\n\
  )") T.no_params
  
    let ultimate_combo_simple2 db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT\n\
     " ^ col.column ^ " AS price_with_tax\n\
  FROM products")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let ultimate_combo_simple2 db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT\n\
     " ^ col.column ^ " AS price_with_tax\n\
  FROM products")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let ultimate_combo_simple2 db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT\n\
     " ^ col.column ^ " AS price_with_tax\n\
  FROM products")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Bare_param_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let custom custom_val =
        let _set_custom p =
          T.set_param_Text p custom_val;
          ()
        in
        {
          set = _set_custom;
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, val TEXT)") T.no_params
  
    let bare_param db ~col ~id callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS custom FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let bare_param db ~col ~id callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS custom FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let bare_param db ~col ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS custom FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Multi_param_col = struct
      let col_a a =
        let _set_col_a p =
          T.set_param_Int p a;
          ()
        in
        {
          set = _set_col_a;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
        }
      let col_b b =
        let _set_col_b p =
          T.set_param_Text p b;
          ()
        in
        {
          set = _set_col_b;
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    let multi_param db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS col_b FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let multi_param db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS col_b FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let multi_param db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS col_b FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Tight_commas_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
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
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
        }
      let bonus extra =
        let _set_bonus p =
          T.set_param_Int p extra;
          ()
        in
        {
          set = _set_bonus;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let tight_commas db ~col ~id callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS bonus FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tight_commas db ~col ~id callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS bonus FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tight_commas db ~col ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS bonus FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Subquery_col_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let rank =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("(SELECT COUNT(*) FROM t t2 WHERE t2.id <= t.id)");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let subquery_col db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS rank FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let subquery_col db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS rank FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let subquery_col db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS rank FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
      module Enum_0 = T.Make_enum(struct
        type t = [`Active | `Inactive]
        let inj = function | "active" -> `Active | "inactive" -> `Inactive | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
        let proj = function  | `Active -> "active"| `Inactive -> "inactive"
      end)
  
    type 'a field_data = {
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
    module Case_col_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let label =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (Enum_0.get_column row idx, idx + 1));
          column = ("CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, status INT)") T.no_params
  
    let case_col db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS label FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let case_col db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS label FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let case_col db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS label FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Func_col_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let full_name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("CONCAT(first_name, ' ', last_name)");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, first_name TEXT, last_name TEXT)") T.no_params
  
    let func_col db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS full_name FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let func_col db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS full_name FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let func_col db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS full_name FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Param_start_expr_col = struct
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
        }
      let scaled multiplier =
        let _set_scaled p =
          begin match multiplier with None -> T.set_param_null p | Some v -> T.set_param_Decimal p v end;
          ()
        in
        {
          set = _set_scaled;
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("" ^ "?" ^ " * price");
          count = 1;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, price DECIMAL(10,2))") T.no_params
  
    let param_start_expr db ~col ~id callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " AS scaled FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let param_start_expr db ~col ~id callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS scaled FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let param_start_expr db ~col ~id callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " AS scaled FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
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
  
    type 'a field_data = {
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
    module Tab_sep_col = struct
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
        }
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT)") T.no_params
  
    let tab_sep db ~col callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tab_sep db ~col callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tab_sep db ~col callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

