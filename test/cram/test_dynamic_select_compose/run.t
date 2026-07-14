Dynamic select composed with other parameter constructs.

  $ cat compose.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > compose.ml
  $ diff compose.ml compose.compare.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c compose.ml

Dynamic select on top of parameterized CTEs and subqueries.

  $ cat cte.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > cte.ml
  $ diff cte.ml cte.compare.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c cte.ml

Run the generated code against the printing mock and check the final SQL strings.

  $ cp ../print_ocaml_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg,yojson -I . -c run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg,yojson -I . -linkpkg -o run.exe print_ocaml_impl.cmo compose.cmo cte.cmo run.cmo
  $ ./run.exe | grep -v '^\[MOCK'
  -- in_list: empty ids
  [SQL] SELECT name FROM t WHERE FALSE AND name = 'x'
  
  -- in_list: two ids
  [SQL] SELECT name FROM t WHERE id IN (1, 2) AND name = 'x'
  
  -- tuple_list: empty pairs
  [SQL] SELECT id FROM t WHERE FALSE
  
  -- tuple_list: two pairs with null
  [SQL] SELECT id FROM t WHERE (id, name) IN ((1, 'a'), (NULL, 'b'))
  
  -- opt_filter: none
  [SQL] SELECT name FROM t WHERE  TRUE 
  
  -- opt_filter: some
  [SQL] SELECT name FROM t WHERE  (  id = 5  ) 
  
  -- order_choice: by id
  [SQL] SELECT name FROM t ORDER BY  ( id ) 
  
  -- order_choice: by name
  [SQL] SELECT name FROM t ORDER BY  ( name ) 
  
  -- order_comma_choice: comma before choice
  [SQL] SELECT name FROM t ORDER BY id, ( name ) 
  
  -- where_choice: all
  [SQL] SELECT name FROM t WHERE  ( TRUE ) 
  
  -- where_choice: by id
  [SQL] SELECT name FROM t WHERE  ( id = 7 ) 
  
  -- shared_choice: all
  [SQL] SELECT name FROM t
  WHERE  ( TRUE ) 
  AND ( ( TRUE )  OR id = 0)
  
  -- shared_choice: by id (both occurrences)
  [SQL] SELECT name FROM t
  WHERE  ( id = 7 ) 
  AND ( ( id = 7 )  OR id = 0)
  
  -- kitchen_sink: everything on
  [SQL] SELECT name FROM t
  WHERE id IN (1, 2)
  AND (id, name) IN ((1, 'a'))
  AND  (  name = 'x'  ) 
  AND  ( id = 5 ) 
  AND ( ( id = 5 )  OR id = 0)
  ORDER BY  ( name ) 
  
  -- kitchen_sink: everything off
  [SQL] SELECT name FROM t
  WHERE FALSE
  AND FALSE
  AND  TRUE 
  AND  ( TRUE ) 
  AND ( ( TRUE )  OR id = 0)
  ORDER BY  ( id ) 
  
  -- cte_plain
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE status = 1)
  SELECT name FROM filtered WHERE id > 10
  
  -- cte_in_list: empty ids
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE FALSE AND name = 'x')
  SELECT name FROM filtered WHERE id > 0
  
  -- cte_in_list: two ids
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE id IN (1, 2) AND name = 'x')
  SELECT name FROM filtered WHERE id > 0
  
  -- cte_choice: all, by id
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  ( TRUE ) )
  SELECT name FROM filtered ORDER BY  ( id ) 
  
  -- cte_choice: by status, by name
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  ( status = 2 ) )
  SELECT name FROM filtered ORDER BY  ( name ) 
  
  -- cte_opt_filter: none, empty pairs
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  TRUE )
  SELECT name FROM filtered WHERE FALSE
  
  -- cte_opt_filter: some, pairs
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  (  status = 1  ) )
  SELECT name FROM filtered WHERE (id, name) IN ((1, 'a'))
  
  -- cte_shared_choice: all
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  ( TRUE ) )
  SELECT name FROM filtered WHERE  ( TRUE ) 
  
  -- cte_shared_choice: by id (cte + main)
  [SQL] WITH filtered AS (SELECT id, name FROM t WHERE  ( id = 9 ) )
  SELECT name FROM filtered WHERE  ( id = 9 ) 
  
  -- cte_param_col: plain column
  [SQL] WITH recent AS (SELECT id, name, status FROM t WHERE status = 1)
  SELECT id FROM recent
  
  -- cte_param_col: param inside picked column binds after cte param
  [SQL] WITH recent AS (SELECT id, name, status FROM t WHERE status = 1)
  SELECT (status >= 100) FROM recent
  
  -- where_subquery: empty names
  [SQL] SELECT name FROM t
  WHERE id IN (SELECT id FROM t WHERE status = 1 AND FALSE)
  
  -- where_subquery: two names
  [SQL] SELECT name FROM t
  WHERE id IN (SELECT id FROM t WHERE status = 1 AND name IN ('a', 'b'))
  
