Subquery sources: a LEFT JOIN of a subquery is never droppable (its columns
inherit the underlying table's UNIQUE/PRIMARY marks but the subquery may
multiply rows); a subquery as the BASE source does not poison a droppable
table join on top of it.

Generated code matches the golden file:

  $ cat subquery_sources.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > subquery_sources.ml
  $ diff subquery_sources.ml subquery_sources.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c subquery_sources.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe subquery_sources.cmo print_impl.cmo run.ml
  $ ./run.exe
  === subquery_sources/plain: pick id -> join kept (subquery source) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/cross_dup: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id FROM users u LEFT JOIN (SELECT p.user_id, p.bio FROM profiles p, users x) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT p.user_id, p.bio FROM profiles p, users x) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/union_dup: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles UNION ALL SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles UNION ALL SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/subq_base: pick id -> table join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT s.id FROM (SELECT id FROM users) s
  [SQL] SELECT s.id FROM (SELECT id FROM users) s
  [MOCK] Returning 0 rows
  === subquery_sources/subq_base: pick bio -> table join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT p.bio FROM (SELECT id FROM users) s LEFT JOIN profiles p ON p.user_id = s.id
  [SQL] SELECT p.bio FROM (SELECT id FROM users) s LEFT JOIN profiles p ON p.user_id = s.id
  [MOCK] Returning 0 rows
