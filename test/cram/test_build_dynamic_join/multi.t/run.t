Multiple joins in one FROM: two independent droppables, a subquery in another
join's ON (conservative keep of everything), the same table joined twice
(distinct constructors per alias).

Generated code matches the golden file:

  $ cat multi.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > multi.ml
  $ diff multi.ml multi.compare.ml

Runtime (print_impl mock): each pick renders only its own join:

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c multi.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe multi.cmo print_impl.cmo run.ml
  $ ./run.exe
  === two_indep: pick id (no joins) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick bio (profiles only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick url (avatars only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT a.url FROM users u LEFT JOIN avatars a ON a.id = u.id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u LEFT JOIN avatars a ON a.id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick bio+url (both) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subq_in_on: pick id -> everything kept (subquery in another join's ON) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = (SELECT MAX(id) FROM avatars) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = (SELECT MAX(id) FROM avatars) WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick bio1 (p1 only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT p1.bio FROM users u LEFT JOIN profiles p1 ON p1.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p1.bio FROM users u LEFT JOIN profiles p1 ON p1.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick bio2 (p2 only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT p2.bio FROM users u LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = ?
  [SQL] SELECT p2.bio FROM users u LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick both ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT p1.bio, p2.bio FROM users u LEFT JOIN profiles p1 ON p1.user_id = u.id LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = ?
  [SQL] SELECT p1.bio, p2.bio FROM users u LEFT JOIN profiles p1 ON p1.user_id = u.id LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = 1
  [MOCK] Returning 0 rows
