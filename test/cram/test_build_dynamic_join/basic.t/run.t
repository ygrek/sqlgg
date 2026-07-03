Basic: droppable PK join / non-unique key / table referenced in WHERE.

Generated code matches the golden file:

  $ cat basic.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > basic.ml
  $ diff basic.ml basic.compare.ml

Runtime (print_impl mock): the join disappears only when safely droppable,
and is always present when its column is picked:

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c basic.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe basic.cmo print_impl.cmo run.ml
  $ ./run.exe
  === basic/ok: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/ok: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/nonuniq: pick id -> join kept (non-unique key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u LEFT JOIN orders o ON o.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN orders o ON o.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/ref_in_where: pick id -> join kept (WHERE reference) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE p.bio = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE p.bio = 'x'
  [MOCK] Returning 0 rows
