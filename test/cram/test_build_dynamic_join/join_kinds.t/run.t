Join kinds: only LEFT JOIN ... ON is a candidate (INNER removes rows, RIGHT
flips sides, comma join has no ON, USING/NATURAL have no ON to analyse).
USING/NATURAL only blocks candidates that come before it, not after.

Generated code matches the golden file:

  $ cat join_kinds.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > join_kinds.ml
  $ diff join_kinds.ml join_kinds.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c join_kinds.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe join_kinds.cmo print_impl.cmo run.ml
  $ ./run.exe
  === join_kinds/inner: pick id -> join kept (INNER) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/using: pick id -> join kept (USING) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/natural: pick id -> join kept (NATURAL) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = ?
  [SQL] SELECT u.id FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/using_after: pick id -> candidate kept (later USING) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id JOIN orders o USING (bio) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id JOIN orders o USING (bio) WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/natural_after: pick id -> candidate kept (later NATURAL) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id NATURAL JOIN orders o WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id NATURAL JOIN orders o WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/right_join: pick id -> join kept (RIGHT) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT u.id FROM users u RIGHT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u RIGHT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/comma_join: pick id -> join kept (comma join) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT u.id FROM users u, profiles p WHERE u.id = ?
  [SQL] SELECT u.id FROM users u, profiles p WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/implicit_before: pick id -> join dropped (USING before candidate) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT u.id FROM users u JOIN shipments s USING (user_id) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u JOIN shipments s USING (user_id) WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/implicit_before: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[9]: SELECT p.bio FROM users u JOIN shipments s USING (user_id) LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u JOIN shipments s USING (user_id) LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
