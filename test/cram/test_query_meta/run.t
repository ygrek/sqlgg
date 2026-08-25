Generated code passes a query record to the traits instead of a bare sql string.
[name] is the name from -- @name or a generated fallback, [kind] mirrors the
statement kind, [filename] is the sql file the statement was read from, and
[sql] is the only field that may be assembled at runtime (see the IN list and
the batch insert in queries.compare.ml).

  $ sqlgg -no-header -gen caml_io -params unnamed -gen caml queries.sql > output.ml
  $ diff output.ml queries.compare.ml
  $ echo $?
  0

Statements read from stdin have no file to point at, so [filename] stays unset.

  $ sqlgg -no-header -gen caml_io -params unnamed -gen caml - < queries.sql | grep -c '~filename:' || true
  0

The implementation receives that record, so annotating is plain user code: it
picks the attributes, sqlcommenter only serializes them into a comment. app.ml
attaches the query name, the kind, the source file, and a per request id, and
skips DDL.

  $ cp ../print_ocaml_impl.ml ../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c app.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o app.exe output.cmo print_ocaml_impl.cmo app.cmo
  $ ./app.exe
  
  === named select, parameters stay bound ===
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, name FROM users WHERE id = 1 /*app='users%20api',file='queries.sql',kind='select',query='find_user',request_id='req-1'*/
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  
  === unnamed select gets a generated name ===
  [MOCK SELECT_ONE] Connection type: [> `RO ]
  [SQL] SELECT COUNT(*) AS total FROM users /*app='users%20api',file='queries.sql',kind='select_one',query='select_2',request_id='req-1'*/
  [MOCK] Returning one row
  [MOCK] get_column_Int[0] = 7
  total: 7
  
  === runtime assembled sql, same name and kind ===
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, name FROM users WHERE id IN (1, 2, 3) /*app='users%20api',file='queries.sql',kind='select',query='find_users',request_id='req-1'*/
  [MOCK] Returning 0 rows
  
  === batch insert ===
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO users (id, name) VALUES (1, 'alice'), (2, 'bob')
  [MOCK] Execute result: affected_rows=2, insert_id=10
  
  === per request attributes change, sql of the query does not ===
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE users SET name = 'carol' WHERE id = 1 /*app='users%20api',file='queries.sql',kind='update%20users',query='rename_user',request_id='req-2'*/
  [MOCK] Execute result: affected_rows=1, insert_id=None
  
  === ddl is left alone by this implementation ===
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL, email TEXT NULL)
  [MOCK] Execute result: affected_rows=0, insert_id=None

With prepared statements the wiring goes above the statement cache, not into
[prepare]: the cache keys on the [sql] it is handed, so annotating first keeps
the key equal to the text that gets prepared. Static attributes leave that text
alone and the statement is reused; a per request attribute rewrites it, and the
cost of that shows up as a prepare per call rather than as a stale comment on a
reused statement.

  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c cached_app.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o cached_app.exe output.cmo print_impl.cmo cached_app.cmo
  File "_none_", line 1:
  Error: Module `Unix' is unavailable (required by `Sqlgg_stmt_cache')
  [2]
  $ ./cached_app.exe
  /tmp/dune_cram_bd7ca8_.cram.sh/main.sh: 1: /tmp/dune_cram_bd7ca8_.cram.sh/12.sh: ./cached_app.exe: not found
  [127]

Which queries carry a per request attribute is a routing decision, and [name] is
what it is made on: those go the non prepared route, everything else keeps using
the cache. routing_app.ml runs the same load both ways over a cache of two, so
the cost of not routing is visible as evictions and re prepares.

  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c routing_app.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o routing_app.exe output.cmo print_impl.cmo routing_app.cmo
  File "_none_", line 1:
  Error: Module `Unix' is unavailable (required by `Sqlgg_stmt_cache')
  [2]
  $ ./routing_app.exe
  /tmp/dune_cram_bd7ca8_.cram.sh/main.sh: 1: /tmp/dune_cram_bd7ca8_.cram.sh/15.sh: ./routing_app.exe: not found
  [127]
