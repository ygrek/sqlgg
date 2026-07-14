Matrix: every parameter construct in every position relative to the dynamic
column (before = in CTE body, inside = in the picked column, after = in main
WHERE/ORDER BY), checked by running the generated code and comparing the final
SQL strings. Distinct sentinel values (111x = before, 222x = inside,
333x = after) make any misordered binding visible.

Reusable queries included as CTEs (WITH x AS &frag) must compose the same way.

  $ cat matrix.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > matrix.ml
  $ cat reusable.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > reusable.ml
  $ cp ../print_ocaml_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c matrix.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c reusable.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg,yojson -I . -c run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg,yojson -I . -linkpkg -o run.exe print_ocaml_impl.cmo matrix.cmo reusable.cmo run.cmo
  $ ./run.exe | grep -v '^\[MOCK'
  -- before_param
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT name FROM f
  
  -- before_in: empty
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE FALSE)
  SELECT name FROM f
  
  -- before_in: two
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE id IN (111, 112))
  SELECT name FROM f
  
  -- before_not_in: empty
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE TRUE)
  SELECT name FROM f
  
  -- before_not_in: two
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE id NOT IN (111, 112))
  SELECT name FROM f
  
  -- before_tuple: empty
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE FALSE)
  SELECT name FROM f
  
  -- before_tuple: two
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE (id, name) IN ((111, 'a'), (112, 'b')))
  SELECT name FROM f
  
  -- before_opt: none
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  TRUE )
  SELECT name FROM f
  
  -- before_opt: some
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  (  status = 111  ) )
  SELECT name FROM f
  
  -- before_choice: all
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  ( TRUE ) )
  SELECT name FROM f
  
  -- before_choice: by status
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  ( status = 111 ) )
  SELECT name FROM f
  
  -- inside_param: plain col
  [SQL] SELECT id FROM t
  
  -- inside_param: param col
  [SQL] SELECT (status >= 222) FROM t
  
  -- inside_in: empty
  [SQL] SELECT (FALSE) FROM t
  
  -- inside_in: two
  [SQL] SELECT (id IN (221, 222)) FROM t
  
  -- inside_not_in: empty
  [SQL] SELECT (TRUE) FROM t
  
  -- inside_not_in: two
  [SQL] SELECT (id NOT IN (221, 222)) FROM t
  
  -- inside_tuple: empty
  [SQL] SELECT (FALSE) FROM t
  
  -- inside_tuple: two
  [SQL] SELECT ((id, name) IN ((221, 'a'), (222, 'b'))) FROM t
  
  -- inside_choice: zero
  [SQL] SELECT  ( 0 )  FROM t
  
  -- inside_choice: val
  [SQL] SELECT  ( 222 )  FROM t
  
  -- after_param
  [SQL] SELECT name FROM t WHERE status = 333
  
  -- after_in: empty
  [SQL] SELECT name FROM t WHERE FALSE
  
  -- after_in: two
  [SQL] SELECT name FROM t WHERE id IN (333, 334)
  
  -- after_not_in: empty
  [SQL] SELECT name FROM t WHERE TRUE
  
  -- after_not_in: two
  [SQL] SELECT name FROM t WHERE id NOT IN (333, 334)
  
  -- after_tuple: empty
  [SQL] SELECT name FROM t WHERE FALSE
  
  -- after_tuple: two
  [SQL] SELECT name FROM t WHERE (id, name) IN ((333, 'a'), (334, 'b'))
  
  -- after_opt: none
  [SQL] SELECT name FROM t WHERE  TRUE 
  
  -- after_opt: some
  [SQL] SELECT name FROM t WHERE  (  status = 333  ) 
  
  -- after_choice: all
  [SQL] SELECT name FROM t WHERE  ( TRUE ) 
  
  -- after_choice: by status
  [SQL] SELECT name FROM t WHERE  ( status = 333 ) 
  
  -- shared_before_after: all
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  ( TRUE ) )
  SELECT name FROM f WHERE  ( TRUE ) 
  
  -- shared_before_after: by status
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE  ( status = 123 ) )
  SELECT name FROM f WHERE  ( status = 123 ) 
  
  -- order_before_inside_after: plain col
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT id FROM f WHERE id > 333
  
  -- order_before_inside_after: param col
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT (status >= 222) FROM f WHERE id > 333
  
  -- mega: everything on
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE id IN (111, 112) AND  (  status = 113  ) )
  SELECT (status >= 222)
  FROM f
  WHERE  ( status = 331 ) 
  AND (id, name) IN ((333, 'a'))
  AND  ( status = 331 ) 
  ORDER BY  ( name ) 
  
  -- mega: everything off
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE FALSE AND  TRUE )
  SELECT id
  FROM f
  WHERE  ( TRUE ) 
  AND FALSE
  AND  ( TRUE ) 
  ORDER BY  ( id ) 
  
  -- reuse_param
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT name FROM f WHERE id > 333
  
  -- reuse_param_col: plain col
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT id FROM f
  
  -- reuse_param_col: param col
  [SQL] WITH f AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT (status >= 222) FROM f
  
  -- reuse_mixed: everything on
  [SQL] WITH f AS (SELECT id, name, status FROM t
  WHERE id IN (111, 112) AND  (  status = 113  )  AND  ( status = 114 ) )
  SELECT name
  FROM f
  WHERE (id, name) IN ((333, 'a'))
  ORDER BY  ( name ) 
  
  -- reuse_mixed: everything off
  [SQL] WITH f AS (SELECT id, name, status FROM t
  WHERE FALSE AND  TRUE  AND  ( TRUE ) )
  SELECT id
  FROM f
  WHERE FALSE
  ORDER BY  ( id ) 
  
  -- reuse_twice: same fragment binds both ctes
  [SQL] WITH a AS (SELECT id, name, status FROM t WHERE status = 111), b AS (SELECT id, name, status FROM t WHERE status = 111)
  SELECT a.name FROM a JOIN b ON a.id = b.id WHERE a.id > 333
  
