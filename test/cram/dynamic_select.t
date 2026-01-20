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
  [SQL] SELECT id,  name  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int[0] = 1
  [TEST 1.1] Completed
  
  [TEST 1.2] Single field: Price
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  price  FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [MOCK] get_column_Int[0] = 1
  [TEST 1.2] Completed
  
  [TEST 1.3] Combined fields: Name and Price using let+/and+
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  name ,  price  FROM products WHERE id = 3
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[1] = Some "Gadget"
  [MOCK] get_column_Decimal_nullable[2] = Some 149.990000
  [MOCK] get_column_Int[0] = 1
  [TEST 1.3] Completed
  
  [TEST 1.4] Three fields: Name, Price, Category
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  name ,  price ,  category  FROM products WHERE id = 4
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[1] = Some "Phone"
  [MOCK] get_column_Decimal_nullable[2] = Some 599.990000
  [MOCK] get_column_Text_nullable[3] = Some "Electronics"
  [MOCK] get_column_Int[0] = 1
  [TEST 1.4] Completed
  
  [TEST 1.5] Mapped field: Price with transformation
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  price  FROM products WHERE id = 5
  [MOCK] Returning one row
  [MOCK] get_column_Decimal_nullable[1] = Some 100.000000
  [MOCK] get_column_Int[0] = 1
  [TEST 1.5] Completed
  
  [TEST 1.6] Return constructor (constant value)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id FROM products WHERE id = 6
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 1
  [TEST 1.6] Completed
  
  --- Test Group 2: Select with callback ---
  [TEST 2.1] List with single field: Name
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id,  name  FROM products WHERE stock > 10
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Widget 
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int[0] = 1
    Row: id=1, col=Widget
    Row 1: col0=2 col1=Gadget 
  [MOCK] get_column_Text_nullable[1] = Some "Gadget"
  [MOCK] get_column_Int[0] = 2
    Row: id=2, col=Gadget
  [TEST 2.1] Completed
  
  [TEST 2.2] List with combined fields: Name and Price
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id,  name ,  price  FROM products WHERE stock > 5
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Widget col2=19.99 
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[2] = Some 19.990000
  [MOCK] get_column_Int[0] = 1
    Row: id=1, name=Widget, price=19.99
    Row 1: col0=2 col1=Gadget col2=29.99 
  [MOCK] get_column_Text_nullable[1] = Some "Gadget"
  [MOCK] get_column_Decimal_nullable[2] = Some 29.990000
  [MOCK] get_column_Int[0] = 2
    Row: id=2, name=Gadget, price=29.99
  [TEST 2.2] Completed
  
  --- Test Group 3: Multiple dynamic selects ---
  [TEST 3.1] Two dynamic selects: x=A(name), y=C(price)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name ,  price  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [TEST 3.1] Completed
  
  [TEST 3.2] Two dynamic selects with combinators
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name ,  category ,  price ,  stock  FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Text_nullable[1] = Some "Electronics"
  [MOCK] get_column_Decimal_nullable[2] = Some 99.990000
  [MOCK] get_column_Int_nullable[3] = Some 50
  [TEST 3.2] Completed
  
  --- Test Group 4: Verbatim branches ---
  [TEST 4.1] Verbatim branch: Default
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  'N/A'  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text[1] = "N/A"
  [MOCK] get_column_Int[0] = 1
  [TEST 4.1] Completed
  
  [TEST 4.2] Regular branch after Verbatim: Name
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  name  FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int[0] = 1
  [TEST 4.2] Completed
  
  --- Test Group 5: Parameter in branch ---
  [TEST 5.1] Static branch (no param)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  name  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[1] = Some "Widget"
  [MOCK] get_column_Int[0] = 1
  [TEST 5.1] Completed
  
  [TEST 5.2] Dynamic branch (with param)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  'Custom Value'  FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text[1] = "Custom Value"
  [MOCK] get_column_Int[0] = 1
  [TEST 5.2] Completed
  
  --- Test Group 6: Dynamic at first position ---
  [TEST 6.1] Dynamic select at first position
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name , id, stock FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Int_nullable[2] = Some 100
  [MOCK] get_column_Int[1] = 1
  [TEST 6.1] Completed
  
  [TEST 6.2] Dynamic select at first position with combinator
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name ,  price , id, stock FROM products WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [MOCK] get_column_Int_nullable[3] = Some 100
  [MOCK] get_column_Int[2] = 1
  [TEST 6.2] Completed
  
  --- Test Group 7: select_one ---
  [TEST 7.1] select_one with single field
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name  FROM products WHERE id = 1 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 7.1] Completed
  
  [TEST 7.2] select_one with combined fields
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name ,  price  FROM products WHERE id = 2 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [MOCK] get_column_Decimal_nullable[1] = Some 99.990000
  [TEST 7.2] Completed
  
  --- Test Group 8: module-wrapped column ---
  [TEST 8.1] Module-wrapped column: Id
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  id  FROM products_wrapped WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 42
  [TEST 8.1] Completed
  
  [TEST 8.2] Module-wrapped: regular column Name
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT  name  FROM products_wrapped WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "Widget"
  [TEST 8.2] Completed
  
  --- Test Group 9: IN list inside subquery branch ---
  [TEST 9.1] IN list inside subquery branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT 1 FROM products WHERE price IN (1., 2.) LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 1
  [MOCK] get_column_Int[0] = 1
  [TEST 9.1] Completed
  
  --- Test Group 10: arithmetic param inside branch ---
  [TEST 10.1] Arithmetic param in branch (price + tax)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  price + 20.  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Decimal_nullable[1] = Some 120.000000
  [MOCK] get_column_Int[0] = 1
  [TEST 10.1] Completed
  
  --- Test Group 11: two params inside branch ---
  [TEST 11.1] Two params in branch (range)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (10. <= price) AND (price <= 20.)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Bool_nullable[1] = Some true
  [MOCK] get_column_Int[0] = 1
  [TEST 11.1] Completed
  
  --- Test Group 12: param + IN list inside one branch ---
  [TEST 12.1] Param + IN list in branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  CONCAT(name, '_x') IN ('a_x', 'b_x')  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Bool_nullable[1] = Some true
  [MOCK] get_column_Int[0] = 1
  [TEST 12.1] Completed
  
  --- Test Group 13: option-actions inside subquery WHERE ---
  [TEST 13.1] Option-actions in subquery (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT 1 FROM products WHERE  TRUE  LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 1
  [MOCK] get_column_Int[0] = 1
  [TEST 13.1] Completed
  
  [TEST 13.2] Option-actions in subquery (Some)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT 1 FROM products WHERE  (  price > 10.  )  LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 1
  [MOCK] get_column_Int[0] = 1
  [TEST 13.2] Completed
  
  --- Test Group 14: tuple list IN inside subquery WHERE ---
  [TEST 14.1] Tuple list IN inside subquery
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT 1 FROM products WHERE (id, stock) IN ((1, 10)) LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 1
  [MOCK] get_column_Int[0] = 1
  [TEST 14.1] Completed
  
  --- Test Group 15: CASE expression inside branch ---
  [TEST 15.1] CASE expression inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  CASE WHEN id = 2 THEN 123 ELSE 0 END  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[1] = 123
  [MOCK] get_column_Int[0] = 1
  [TEST 15.1] Completed
  
  --- Test Group 16: typed param inside branch ---
  [TEST 16.1] Typed param inside branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  'hello'  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Text[1] = "hello"
  [MOCK] get_column_Int[0] = 1
  [TEST 16.1] Completed
  
  --- Test Group 17: monster nested scenario ---
  [TEST 17.1] Monster nested (all nested expr kinds)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 
  (SELECT
  CASE
  WHEN  p2.id = 2 
  THEN
  CASE
  WHEN ( 1  = 1) THEN 'then_v' ELSE 'else_v'
  END
  ELSE 0
  END
  FROM products p2
  WHERE  (  p2.price > 10.  ) 
  AND p2.name IN ('a', 'b')
  AND (p2.id, p2.stock) IN ((1, 10))
  LIMIT 1)
   FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 42
  [MOCK] get_column_Int[0] = 1
  [TEST 17.1] Completed
  
  --- Test Group 18: ultimate combo (multiple branches) ---
  [TEST 18.1] Ultimate combo: Plain branch (stock)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  stock  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 100
  [MOCK] get_column_Int[0] = 1
  [TEST 18.1] Completed
  
  [TEST 18.2] Ultimate combo: WithInList branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT COUNT(*) FROM products WHERE id IN (1, 2, 3))  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[1] = 3
  [MOCK] get_column_Int[0] = 1
  [TEST 18.2] Completed
  
  [TEST 18.3] Ultimate combo: WithOptional branch (None)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT stock FROM products WHERE  TRUE  LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 50
  [MOCK] get_column_Int[0] = 1
  [TEST 18.3] Completed
  
  [TEST 18.4] Ultimate combo: WithOptional branch (Some)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT stock FROM products WHERE  (  stock > 5  )  LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 75
  [MOCK] get_column_Int[0] = 1
  [TEST 18.4] Completed
  
  [TEST 18.5] Ultimate combo: WithChoiceAndList branch (UseSum)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 
  CASE WHEN  1  = 1
  THEN (SELECT COUNT(*) FROM products WHERE name IN ('foo', 'bar'))
  ELSE (SELECT COUNT(*) FROM products WHERE name IN ('foo', 'bar'))
  END
   FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[1] = 5
  [MOCK] get_column_Int[0] = 1
  [TEST 18.5] Completed
  
  [TEST 18.6] Ultimate combo: WithChoiceAndList branch (UseAvg)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 
  CASE WHEN  2  = 1
  THEN (SELECT COUNT(*) FROM products WHERE name IN ('baz'))
  ELSE (SELECT COUNT(*) FROM products WHERE name IN ('baz'))
  END
   FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[1] = 3
  [MOCK] get_column_Int[0] = 1
  [TEST 18.6] Completed
  
  [TEST 18.7] Ultimate combo: WithTupleList branch
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id,  (SELECT 1 FROM products WHERE (id, stock) IN ((1, 10), (2, 20)) LIMIT 1)  FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[1] = Some 1
  [MOCK] get_column_Int[0] = 1
  [TEST 18.7] Completed
  
  [TEST 18.8] Ultimate combo: FullCombo branch (opt + IN + param)
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, 
  (SELECT COUNT(*)
  FROM products p2
  WHERE  (  p2.id = 5  ) 
  AND p2.name IN ('x', 'y')
  AND p2.price > 100.)
   FROM products WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1
  [TEST 18.8] Completed
  
  --- Test Group 19---
  [TEST 19] everything in a row
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT 
  id,  name ,  price ,  price ,  price ,  price ,  name ,  name ,  name ,  name ,  name , 
  category, stock,  price +  2  ,  price +  2 + 23  ,  price +  3 + 23  ,  price +  4 + 23  ,  price  
  FROM products
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=42 
  [MOCK] get_column_Text_nullable[1] = Some "mock_text" (default)
  [MOCK] get_column_Decimal_nullable[2] = None
  [MOCK] get_column_Decimal_nullable[3] = None
  [MOCK] get_column_Decimal_nullable[4] = None
  [MOCK] get_column_Decimal_nullable[5] = None
  [MOCK] get_column_Text_nullable[6] = None
  [MOCK] get_column_Text_nullable[7] = None
  [MOCK] get_column_Text_nullable[8] = None
  [MOCK] get_column_Text_nullable[9] = None
  [MOCK] get_column_Text_nullable[10] = None
  [MOCK] get_column_Decimal_nullable[13] = None
  [MOCK] get_column_Decimal_nullable[14] = None
  [MOCK] get_column_Decimal_nullable[15] = None
  [MOCK] get_column_Decimal_nullable[16] = None
  [MOCK] get_column_Decimal_nullable[17] = None
  [MOCK] get_column_Int_nullable[12] = None
  [MOCK] get_column_Text_nullable[11] = None
  [MOCK] get_column_Int[0] = 1
  [TEST 19] Completed
  
  === All Dynamic Select Tests Passed ===
  
  ==================================================
  All tests executed successfully!

