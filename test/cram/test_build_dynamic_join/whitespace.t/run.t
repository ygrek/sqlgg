Whitespace around dropped joins: a dropped join takes its leading whitespace
with it, a kept one is spliced back with a single space, string literals stay
untouched.

Generated code matches the golden file:

  $ cat whitespace.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > whitespace.ml
  $ diff whitespace.ml whitespace.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c whitespace.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe whitespace.cmo print_impl.cmo run.ml
  $ ./run.exe
  === ws: pick id -> both joins dropped, no gaps ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id
  FROM users u
  WHERE u.note = '  two  spaces  inside  ' AND u.id = ?
  [SQL] SELECT u.id
  FROM users u
  WHERE u.note = '  two  spaces  inside  ' AND u.id = 1
  [MOCK] Returning 0 rows
  === ws: pick id + bio -> profiles kept, billing gap collapsed ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id, p.bio
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = ?
  [SQL] SELECT u.id, p.bio
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = 1
  [MOCK] Returning 0 rows
  === ws: pick id + plan -> profiles gap collapsed, billing kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id, b.plan
  FROM users u LEFT JOIN billing b ON b.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = ?
  [SQL] SELECT u.id, b.plan
  FROM users u LEFT JOIN billing b ON b.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = 1
  [MOCK] Returning 0 rows
  === ws: pick all -> both joins, single spaces ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id, p.bio, b.plan
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN billing b ON b.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = ?
  [SQL] SELECT u.id, p.bio, b.plan
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN billing b ON b.user_id = u.id
  WHERE u.note = '  two  spaces  inside  ' AND u.id = 1
  [MOCK] Returning 0 rows
