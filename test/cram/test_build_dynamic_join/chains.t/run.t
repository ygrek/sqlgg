Transitive chains: closures accumulate child-first (two-level, three-level, diamond).

  $ cat chains.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > chains.ml
  $ diff chains.ml chains.compare.ml

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c chains.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe chains.cmo print_impl.cmo run.ml
  $ ./run.exe
  === chain: pick id ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u WHERE u.id = ?
  [SQL] SELECT u.id FROM users u WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick bio ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick url (pulls profiles transitively) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick all ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain3: pick label (pulls the whole ancestor chain) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE u.id = ?
  [SQL] SELECT b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain3: pick url (badges not pulled) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick url (one branch) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick label (other branch) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = ?
  [SQL] SELECT b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick both (parent emitted once) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[9]: SELECT a.url, b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = ?
  [SQL] SELECT a.url, b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain_pinned: pick id (WHERE pins the whole chain, no joins dropped) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[10]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE b.label = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE b.label = 'x'
  [MOCK] Returning 0 rows
