Key shapes: non-PK UNIQUE works, composite PK only when ALL parts are equated.

Generated code matches the golden file:

  $ cat key_shapes.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > key_shapes.ml
  $ diff key_shapes.ml key_shapes.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c key_shapes.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe key_shapes.cmo print_impl.cmo run.ml
  $ ./run.exe
  === key_shapes/unique: pick id -> join dropped (UNIQUE key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/unique: pick label -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT a.label FROM users u LEFT JOIN accounts a ON a.email = u.email WHERE u.id = ?
  [SQL] SELECT a.label FROM users u LEFT JOIN accounts a ON a.email = u.email WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_partial: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_full: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_full: pick title -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT m.title FROM users u LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept WHERE u.id = ?
  [SQL] SELECT m.title FROM users u LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept WHERE u.id = 1
  [MOCK] Returning 0 rows
