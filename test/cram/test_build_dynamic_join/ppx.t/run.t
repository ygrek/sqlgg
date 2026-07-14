Ppx-derived records drive join elimination: a record needing only base-table
columns drops both LEFT JOINs, composed records pull in exactly their joins.

  $ cat ppx_je.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > ppx_je.ml

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c ppx_je.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c run.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe ppx_je.cmo print_impl.cmo run.cmo
  $ ./run.exe
  === wide: item record -> both joins dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT i.id, i.name
  FROM items i
  WHERE i.id > ?
  [SQL] SELECT i.id, i.name
  FROM items i
  WHERE i.id > 0
  [MOCK] Returning 0 rows
  === wide: item + stat -> only stats join ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT i.id, i.name, s.sold
  FROM items i LEFT JOIN stats s ON s.item_id = i.id
  WHERE i.id > ?
  [SQL] SELECT i.id, i.name, s.sold
  FROM items i LEFT JOIN stats s ON s.item_id = i.id
  WHERE i.id > 0
  [MOCK] Returning 0 rows
  === wide: described -> only details join ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT d.descr
  FROM items i LEFT JOIN details d ON d.item_id = i.id
  WHERE i.id > ?
  [SQL] SELECT d.descr
  FROM items i LEFT JOIN details d ON d.item_id = i.id
  WHERE i.id > 0
  [MOCK] Returning 0 rows
  === wide: item + described + stat -> both joins ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT i.id, i.name, d.descr, s.sold
  FROM items i LEFT JOIN details d ON d.item_id = i.id LEFT JOIN stats s ON s.item_id = i.id
  WHERE i.id > ?
  [SQL] SELECT i.id, i.name, d.descr, s.sold
  FROM items i LEFT JOIN details d ON d.item_id = i.id LEFT JOIN stats s ON s.item_id = i.id
  WHERE i.id > 0
  [MOCK] Returning 0 rows
  === narrow: same item record, different query ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT id, name FROM items WHERE id > ?
  [SQL] SELECT id, name FROM items WHERE id > 0
  [MOCK] Returning 0 rows
