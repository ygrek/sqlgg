If the child join is NOT droppable, its ON reference pins the parent too.

Generated code matches the golden file:

  $ cat chain_bad.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > chain_bad.ml
  $ diff chain_bad.ml chain_bad.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c chain_bad.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe chain_bad.cmo print_impl.cmo run.ml
  $ ./run.exe
  === chain_bad: pick id -> both joins kept (child pins parent) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.profile_id = p.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.profile_id = p.id WHERE u.id = 1
  [MOCK] Returning 0 rows
