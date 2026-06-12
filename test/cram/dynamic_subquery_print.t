Dynamic select over subquery sources, exercised through the printing mock so the
final SQL is shown for each runtime column selection.

  $ cp test_build_dynamic_subquery/dyn_subq.sql .
  $ cp test_build_dynamic_subquery/test_run.ml .
  $ cat dyn_subq.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe print_ocaml_impl.cmo output.cmo test_run.ml
  $ ./test_run.exe 2>&1 | grep -E '^---|^\[[0-9]|^\[SQL\]'
  --- Group 1: SELECT * over subquery (pushdown) ---
  [1.1] pick id
  [SQL] SELECT * FROM (SELECT id FROM products WHERE price > 10) AS sub
  [1.2] pick name
  [SQL] SELECT * FROM (SELECT name FROM products WHERE price > 10) AS sub
  [1.3] pick price
  [SQL] SELECT * FROM (SELECT price FROM products WHERE price > 10) AS sub
  [1.4] pick id + name + price
  [SQL] SELECT * FROM (SELECT id, name, price FROM products WHERE price > 10) AS sub
  --- Group 2: SELECT sub.* over subquery (pushdown) ---
  [2.1] pick name
  [SQL] SELECT sub.* FROM (SELECT name FROM products) AS sub
  [2.2] pick id + price
  [SQL] SELECT sub.* FROM (SELECT id, price FROM products) AS sub
  --- Group 3: SELECT * over LEFT JOIN subquery ---
  [3.1] pick uid
  [SQL] SELECT * FROM (SELECT u.id FROM users u LEFT JOIN orders o ON o.user_id = u.id) AS sub
  [3.2] pick ototal (nullable)
  [SQL] SELECT * FROM (SELECT o.total FROM users u LEFT JOIN orders o ON o.user_id = u.id) AS sub
  [3.3] pick uid + uname + ototal
  [SQL] SELECT * FROM (SELECT u.id, u.name, o.total FROM users u LEFT JOIN orders o ON o.user_id = u.id) AS sub
  --- Group 4: non-pass-through (dynamic stays outside) ---
  [4.1] pick id
  [SQL] SELECT sub.id FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = 1
  [4.2] pick id + name
  [SQL] SELECT sub.id, sub.name FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = 1
