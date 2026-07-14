References outside the projection (GROUP BY/ORDER BY/HAVING/WHERE-subquery) pin the join.

  $ cat outside_refs.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > outside_refs.ml
  $ diff outside_refs.ml outside_refs.compare.ml

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c outside_refs.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe outside_refs.cmo print_impl.cmo run.ml
  $ ./run.exe
  === outside_refs/group: pick id -> join kept (GROUP BY) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio
  [MOCK] Returning 0 rows
  === outside_refs/order: pick id -> join kept (ORDER BY) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio
  [MOCK] Returning 0 rows
  === outside_refs/having: pick id -> join kept (HAVING) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0
  [MOCK] Returning 0 rows
  === outside_refs/complex_proj: pick id -> join kept (complex expr) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === outside_refs/subq_in_where: pick id -> join kept (subquery in WHERE) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)
  [MOCK] Returning 0 rows
  === outside_refs/unqualified: pick id -> join kept (unqualified ref) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'
  [MOCK] Returning 0 rows
  === outside_refs/unreferenced: pick id -> join rendered statically ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
