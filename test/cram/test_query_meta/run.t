Generated code passes a query record to the traits instead of a bare sql string.
[name] is the name from -- @name or a generated fallback, [kind] mirrors the
statement kind, [filename] is the sql file the statement was read from, and
[sql] is the only field that may be assembled at runtime (see the IN list and
the batch insert in queries.compare.ml).

  $ sqlgg -no-header -gen caml -params unnamed queries.sql > output.ml
  $ diff output.ml queries.compare.ml
  $ echo $?
  0

Statements read from stdin have no file to point at, so [filename] stays unset.

  $ sqlgg -no-header -gen caml -params unnamed - < queries.sql | grep -c '~filename:' || true
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
  [SQL] INSERT INTO users (id, name) VALUES (1, 'alice'), (2, 'bob') /*app='users%20api',file='queries.sql',kind='insert%20users',query='add_users',request_id='req-1'*/
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
  $ ./cached_app.exe
  
  === static attributes only: first call prepares ===
  [MOCK] PREPARE[1]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user'*/
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  Cache: 1/16 items, 1 ops since start
  
  === next request reuses that statement, the sql is the same ===
  [MOCK] SELECT_WITH_STMT[1]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=2 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 2
  row: 2 alice
  Cache: 1/16 items, 2 ops since start
  
  === execute takes the same path ===
  [MOCK] PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user'*/
  [MOCK] EXECUTE_WITH_STMT[2]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/16 items, 3 ops since start
  
  === adding a per request attribute changes the sql on every call ===
  [MOCK] PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-2'*/
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=3 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 3
  row: 3 alice
  Cache: 3/16 items, 4 ops since start
  [MOCK] PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=4 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 4
  row: 4 alice
  Cache: 4/16 items, 5 ops since start
  
  --- MOCK STATS ---
  Prepared: 4, Closed: 0, Open: 4
  Operations:
    1. PREPARE[1]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user'*/
    2. SELECT_WITH_STMT[1]
    3. SELECT_WITH_STMT[1]
    4. PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user'*/
    5. EXECUTE_WITH_STMT[2]
    6. PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-2'*/
    7. SELECT_WITH_STMT[3]
    8. PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
    9. SELECT_WITH_STMT[4]
  ---

Which queries carry a per request attribute is a routing decision, and [name] is
what it is made on: those go the non prepared route, everything else keeps using
the cache. routing_app.ml runs the same load both ways over a cache of two, so
the cost of not routing is visible as evictions and re prepares.

  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c routing_app.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o routing_app.exe output.cmo print_impl.cmo routing_app.cmo
  $ ./routing_app.exe
  
  === static attributes fill the cache ===
  [MOCK] PREPARE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2'*/
  [MOCK] SELECT_ONE_WITH_STMT[1]
  [MOCK] select_one_with_stmt returning one row
  [MOCK] get_column_Int[0] = 7
  [MOCK] PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user'*/
  [MOCK] EXECUTE_WITH_STMT[2]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/2 items, 2 ops since start
  
  === both are reused, comments unchanged ===
  [MOCK] SELECT_ONE_WITH_STMT[1]
  [MOCK] select_one_with_stmt returning one row
  [MOCK] get_column_Int[0] = 7
  [MOCK] EXECUTE_WITH_STMT[2]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/2 items, 4 ops since start
  
  === per request query is routed past the cache, comment is fresh ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-1'*/
  [SQL] SELECT id, name FROM users WHERE id = 1 /*app='users%20api',query='find_user',request_id='req-1'*/
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-2'*/
  [SQL] SELECT id, name FROM users WHERE id = 2 /*app='users%20api',query='find_user',request_id='req-2'*/
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  Cache: 2/2 items, 4 ops since start
  
  === the cached statements survived, still no new prepare for them ===
  [MOCK] SELECT_ONE_WITH_STMT[1]
  [MOCK] select_one_with_stmt returning one row
  [MOCK] get_column_Int[0] = 7
  [MOCK] EXECUTE_WITH_STMT[2]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/2 items, 6 ops since start
  --- after routing ---
  
  --- MOCK STATS ---
  Prepared: 4, Closed: 0, Open: 4
  Operations:
    1. PREPARE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2'*/
    2. SELECT_ONE_WITH_STMT[1]
    3. PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user'*/
    4. EXECUTE_WITH_STMT[2]
    5. SELECT_ONE_WITH_STMT[1]
    6. EXECUTE_WITH_STMT[2]
    7. PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-1'*/
    8. PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-2'*/
    9. SELECT_ONE_WITH_STMT[1]
    10. EXECUTE_WITH_STMT[2]
  ---
  
  === same load through the cache instead: every call is a new statement ===
  [MOCK] PREPARE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-2'*/
  [MOCK] SELECT_ONE_WITH_STMT[1]
  [MOCK] select_one_with_stmt returning one row
  [MOCK] get_column_Int[0] = 7
  [MOCK] PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-2'*/
  [MOCK] EXECUTE_WITH_STMT[2]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/2 items, 2 ops since start
  [MOCK] PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
  [MOCK] CLOSE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-2'*/
  [MOCK] SELECT_WITH_STMT[3]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  [MOCK] PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-4'*/
  [MOCK] CLOSE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-2'*/
  [MOCK] SELECT_WITH_STMT[4]
  [MOCK] Processing 1 rows in select_with_stmt
    Row 0: col0=1 col1=alice 
  [MOCK] get_column_Text[1] = "alice"
  [MOCK] get_column_Int[0] = 1
  row: 1 alice
  Cache: 2/2 items, 4 ops since start
  
  === and the statements it evicted have to be prepared again ===
  [MOCK] PREPARE[5]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-4'*/
  [MOCK] CLOSE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
  [MOCK] SELECT_ONE_WITH_STMT[5]
  [MOCK] select_one_with_stmt returning one row
  [MOCK] get_column_Int[0] = 7
  [MOCK] PREPARE[6]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-4'*/
  [MOCK] CLOSE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-4'*/
  [MOCK] EXECUTE_WITH_STMT[6]
  [MOCK] execute_with_stmt result: affected_rows=1, insert_id=None
  Cache: 2/2 items, 6 ops since start
  --- after naive ---
  
  --- MOCK STATS ---
  Prepared: 6, Closed: 4, Open: 2
  Operations:
    1. PREPARE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-2'*/
    2. SELECT_ONE_WITH_STMT[1]
    3. PREPARE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-2'*/
    4. EXECUTE_WITH_STMT[2]
    5. PREPARE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
    6. CLOSE[1]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-2'*/
    7. SELECT_WITH_STMT[3]
    8. PREPARE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-4'*/
    9. CLOSE[2]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-2'*/
    10. SELECT_WITH_STMT[4]
    11. PREPARE[5]: SELECT COUNT(*) AS total FROM users /*app='users%20api',query='select_2',request_id='req-4'*/
    12. CLOSE[3]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-3'*/
    13. SELECT_ONE_WITH_STMT[5]
    14. PREPARE[6]: UPDATE users SET name = ? WHERE id = ? /*app='users%20api',query='rename_user',request_id='req-4'*/
    15. CLOSE[4]: SELECT id, name FROM users WHERE id = ? /*app='users%20api',query='find_user',request_id='req-4'*/
    16. EXECUTE_WITH_STMT[6]
  ---
