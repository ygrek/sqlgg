ON shapes: params/inequality/OR/subquery keep the join, constants do not.

  $ cat on_shapes.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > on_shapes.ml
  $ diff on_shapes.ml on_shapes.compare.ml

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c on_shapes.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe on_shapes.cmo print_impl.cmo run.ml
  $ ./run.exe
  === on_shapes/param_in_on: pick id -> join kept (param in ON) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = ? WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/inequality: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT profiles.bio FROM users u LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = ?
  [SQL] SELECT profiles.bio FROM users u LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/flipped_on: pick id -> join dropped (operand order does not matter) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/flipped_on: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT p.bio FROM users u LEFT JOIN profiles p ON u.id = p.user_id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON u.id = p.user_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/const_key_on: pick id -> join dropped (key equated to a constant) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[9]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/const_key_on: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[10]: SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = 5 WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = 5 WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/or_in_on: pick id -> join kept (OR can match many rows) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[11]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id OR p.user_id = 0 WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id OR p.user_id = 0 WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/subq_own_on: pick id -> join kept (subquery in own ON) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[12]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = (SELECT MAX(id) FROM users) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = (SELECT MAX(id) FROM users) WHERE u.id = 1
  [MOCK] Returning 0 rows
