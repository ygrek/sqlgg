Self-joins matched by alias key: non-unique keeps the join, PK is droppable.

  $ cat self_join.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > self_join.ml
  $ diff self_join.ml self_join.compare.ml

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
