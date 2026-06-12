The README/PR example: one wide reusable query definition acts as a whole
family of queries — pick fewer columns and the SQL narrows accordingly,
dropping the joins whose columns were not picked.

Generated code matches the golden file:

  $ cat example.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > example.ml
  $ diff example.ml example.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c example.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe example.cmo print_impl.cmo run.ml
  $ ./run.exe
  === brief: name + email -> no joins ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.name, u.email
  FROM users u
  WHERE u.org_id = ? AND u.deleted = FALSE
  [SQL] SELECT u.name, u.email
  FROM users u
  WHERE u.org_id = 1 AND u.deleted = FALSE
  [MOCK] Returning 0 rows
  === card: name + bio + avatar_url -> profiles only ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.name, p.bio, p.avatar_url
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id
  WHERE u.org_id = ? AND u.deleted = FALSE
  [SQL] SELECT u.name, p.bio, p.avatar_url
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id
  WHERE u.org_id = 1 AND u.deleted = FALSE
  [MOCK] Returning 0 rows
  === brief + billing, no profile: name + plan -> billing only ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.name, b.plan
  FROM users u LEFT JOIN billing  b ON b.user_id = u.id
  WHERE u.org_id = ? AND u.deleted = FALSE
  [SQL] SELECT u.name, b.plan
  FROM users u LEFT JOIN billing  b ON b.user_id = u.id
  WHERE u.org_id = 1 AND u.deleted = FALSE
  [MOCK] Returning 0 rows
  === admin: everything -> both joins ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.name, p.bio, b.plan, b.balance
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN billing  b ON b.user_id = u.id
  WHERE u.org_id = ? AND u.deleted = FALSE
  [SQL] SELECT u.name, p.bio, b.plan, b.balance
  FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN billing  b ON b.user_id = u.id
  WHERE u.org_id = 1 AND u.deleted = FALSE
  [MOCK] Returning 0 rows
