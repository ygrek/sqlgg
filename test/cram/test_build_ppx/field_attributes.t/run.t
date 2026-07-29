The field attributes against a real query. The one to watch is [@sqlgg.by].
Its conversion comes from the call site, so one record reads columns of
different types.

  $ cat posts.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > posts.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c posts.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c run.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe posts.cmo print_impl.cmo run.cmo
  $ ./run.exe
  === a post ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT id, body, reply_count, hits FROM posts WHERE id > ?
  [SQL] SELECT id, body, reply_count, hits FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=hi col2=7 col3=NULL 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "hi"
  [MOCK] get_column_Int[2] = 7
  [MOCK] get_column_Int_nullable[3] = None
  id=1 text=hi reply_count=7 hits=0
  
  === the same post, columns picked by hand ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT id, body, reply_count, hits FROM posts WHERE id > ?
  [SQL] SELECT id, body, reply_count, hits FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=hi col2=7 col3=NULL 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "hi"
  [MOCK] get_column_Int[2] = 7
  [MOCK] get_column_Int_nullable[3] = None
  id=1 text=hi reply_count=7 hits=-1
  
  === n read as an int64 ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT id, reply_count FROM posts WHERE id > ?
  [SQL] SELECT id, reply_count FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=9 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int[1] = 9
  id=1 n=9
  
  === the same record, n read as text ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT id, body FROM posts WHERE id > ?
  [SQL] SELECT id, body FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=abcd 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Text_nullable[1] = Some "abcd"
  id=1 n=4
  
  === conversion left to the default ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT id, reply_count FROM posts WHERE id > ?
  [SQL] SELECT id, reply_count FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=9 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int[1] = 9
  id=1 n=9
  
  === conversion passed at the call site ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT id, reply_count FROM posts WHERE id > ?
  [SQL] SELECT id, reply_count FROM posts WHERE id > 0
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=9 
  [MOCK] get_column_Int[0] = 1
  [MOCK] get_column_Int[1] = 9
  id=1 n=90
  
  === inserting a post ===
  [MOCK EXECUTE] Connection type: [> `WR ]
  [MOCK] PREPARE[7]: INSERT INTO posts (id, body, reply_count, hits)
  VALUES (?, ?, ?, ?)
  [SQL] INSERT INTO posts (id, body, reply_count, hits)
  VALUES (5, 'hello', 2, 11)
  [MOCK] Execute result: affected_rows=1, insert_id=None
  

A bare [@sqlgg.by] cannot be omitted at the call site.

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c reject_seam.ml 2> /dev/null || echo rejected
  rejected

A conversion that does not fit blames its own field, not the whole record.

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c locate.ml 2>&1 | head -1
  File "locate.ml", line 4, characters 2-7:
