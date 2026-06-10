Self-joins are matched by their alias key, not the bare table name: non-unique
key keeps the join, PK self-join is droppable.

Generated code matches the golden file:

  $ cat self_join.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > self_join.ml
  $ diff self_join.ml self_join.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c self_join.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe self_join.cmo print_impl.cmo run.ml
  $ ./run.exe
  === self_join/bad: pick id -> join kept (non-unique self key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u1.id FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id
  [SQL] SELECT u1.id FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id
  [MOCK] Returning 0 rows
  === self_join/good: pick id -> join dropped (PK self key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u1.id FROM users u1
  [SQL] SELECT u1.id FROM users u1
  [MOCK] Returning 0 rows
  === self_join/good: pick name -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u2.name FROM users u1 LEFT JOIN users u2 ON u2.id = u1.manager_id
  [SQL] SELECT u2.name FROM users u1 LEFT JOIN users u2 ON u2.id = u1.manager_id
  [MOCK] Returning 0 rows
