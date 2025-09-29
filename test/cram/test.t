Issue183 is fixed:
  $ /bin/sh ./sqlgg_test.sh registration_feedbacks.sql registration_feedbacks.compare.ml  
  $ echo $?
  0

Support reusable queries as CTE:
  $ /bin/sh ./sqlgg_test.sh reusable_queries_as_ctes.sql reusable_queries_as_ctes.compare.ml  
  $ echo $?
  0

Implement set default feature:
  $ /bin/sh ./sqlgg_test.sh set_default_syntax.sql set_default_syntax.compare.ml  
  $ echo $?
  0

Implement set default feature, no way to set DEFAULT for fields without DEFAULT:
  $ cat set_default_syntax_fail.sql | sqlgg -params unnamed -gen caml - 2>&1 | grep "Column test doesn't have default value"
  Fatal error: exception Failure("Column test doesn't have default value")

WHERE IN tuples composable with the rest:
  $ /bin/sh ./sqlgg_test.sh where_in_composable.sql where_in_composable.compare.ml
  $ echo $?
  0

Implement type mappings, fst part, no insert, no update, no params, only select:
  $ /bin/sh ./sqlgg_test.sh type_mappings.sql type_mappings.compare.ml  
  $ echo $?
  0

Implement type mappings, snd part, comparison op, IN, IN with tuples:
  $ /bin/sh ./sqlgg_test.sh type_mappings_params.sql type_mappings_params.compare.ml  
  $ echo $?
  0
 
Test SQLite dialect with supported collations:
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test_rtrim (
  >     name TEXT COLLATE rtrim,
  >     description TEXT
  > )
  > EOF
  $ echo $?
  0

Test SQLite dialect with unsupported MySQL collations (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE test_mysql_utf8 (
  >     name TEXT COLLATE utf8_general_ci,  -- MySQL specific
  >     surname TEXT
  > );
  > EOF
  Feature Collation is not supported for dialect SQLite (supported by: MySQL, TiDB) at COLLATE utf8_general_ci
  Errors encountered, no code generated
  [1]

Test SQLite dialect with unsupported custom collations (should warn):
  $ sqlgg -gen caml -dialect=sqlite -no-check=collation - <<'EOF' 2>&1 | grep -i "warning"
  > CREATE TABLE test_mysql_utf8 (
  >     name TEXT COLLATE amazing_Collation_xxx,  -- For example custom by sqlite3_create_collation
  >     surname TEXT
  > );
  > EOF
  Warning: Assuming custom collation implementation for SQLite

Test SQLite dialect with unsupported custom collations, skip by all flag (should warn):
  $ sqlgg -gen caml -dialect=sqlite -no-check=all - <<'EOF' 2>&1 | grep -i "warning"
  > CREATE TABLE test_mysql_utf8 (
  >     name TEXT COLLATE amazing_Collation_xxx,  -- For example custom by sqlite3_create_collation
  >     surname TEXT
  > );
  > EOF
  Warning: Assuming custom collation implementation for SQLite

Test TiDB dialect with regular table JOINs (should work):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE orders (id INT, user_id INT, amount DECIMAL);
  > SELECT u.name, o.amount FROM users u JOIN orders o ON u.id = o.user_id;
  > EOF
  $ echo $?
  0

Test TiDB dialect with subquery JOINs (should fail):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE orders (id INT, user_id INT, amount DECIMAL);
  > SELECT u.name, stats.total FROM users u
  > JOIN (
  >     SELECT user_id, SUM(amount) as total 
  >     FROM orders 
  >     GROUP BY user_id
  > ) stats ON u.id = stats.user_id;
  > EOF
  Feature JoinOnSubquery is not supported for dialect TiDB (supported by: MySQL, PostgreSQL, SQLite) at 
  JOIN (
      SELECT user_id, SUM(amount) as total 
      FROM orders 
      GROUP BY user_id
  ) stats ON u.id = stats.user_id
  Errors encountered, no code generated
  [1]

Test MySQL dialect with JOIN on subquery (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE orders (id INT, user_id INT, amount DECIMAL);
  > SELECT u.name, stats.total FROM users u
  > JOIN (
  >     SELECT user_id, SUM(amount) as total 
  >     FROM orders 
  >     GROUP BY user_id
  > ) stats ON u.id = stats.user_id;
  > EOF
  $ echo $?
  0


Test MySQL dialect with PostgreSQL collations (should fail 3 times, since there are 3 invalid collates):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE test_postgres_collations (
  >     data TEXT COLLATE "C",
  >     info TEXT COLLATE "en-US-x-icu",
  >     locale TEXT COLLATE "C.UTF-8"
  > );
  > EOF
  Feature Collation is not supported for dialect MySQL (supported by: PostgreSQL) at COLLATE "C.UTF-8"
  Feature Collation is not supported for dialect MySQL (supported by: PostgreSQL) at COLLATE "en-US-x-icu"
  Feature Collation is not supported for dialect MySQL (supported by: PostgreSQL) at COLLATE "C"
  Errors encountered, no code generated
  [1]

Test PostgreSQL dialect with PostgreSQL collations (should work):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' >/dev/null
  > CREATE TABLE test_postgres (
  >     data TEXT COLLATE "C",
  >     info TEXT COLLATE "default",
  >     unicode_data TEXT COLLATE "unicode"
  > );
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with MySQL collations (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1 
  > CREATE TABLE test_mysql_in_postgres (
  >     name TEXT COLLATE utf8_general_ci
  > );
  > EOF
  Feature Collation is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at COLLATE utf8_general_ci
  Errors encountered, no code generated
  [1]

Test different collations work in their native dialects:
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test_sqlite (data TEXT COLLATE nocase);
  > EOF
  $ echo $?
  0

  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null  
  > CREATE TABLE test_mysql (data TEXT COLLATE utf8mb4_bin);
  > EOF
  $ echo $?
  0

Test quoted vs unquoted collations work the same:
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' >/dev/null
  > CREATE TABLE test_quoted (
  >     data1 TEXT COLLATE "C",
  >     data2 TEXT COLLATE C
  > );
  > EOF
  $ echo $?
  0

Test CREATE TABLE AS SELECT dialect support:
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE summary AS SELECT id, name FROM users;
  > EOF
  $ echo $?
  0

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE summary AS SELECT id, name FROM users;
  > EOF
  $ echo $?
  0

  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null  
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE summary AS SELECT id, name FROM users;
  > EOF
  $ echo $?
  0

Test TiDB dialect with CREATE TABLE AS SELECT (should fail):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE summary AS SELECT id, name FROM users;
  > EOF
  Feature CreateTableAsSelect is not supported for dialect TiDB (supported by: MySQL, PostgreSQL, SQLite) at CREATE TABLE summary AS SELECT id, name FROM users
  Errors encountered, no code generated
  [1]

Test MySQL dialect with ON DUPLICATE KEY UPDATE (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON DUPLICATE KEY UPDATE name = VALUES(name);
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON DUPLICATE KEY UPDATE (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON DUPLICATE KEY UPDATE name = VALUES(name);
  > EOF
  Feature OnDuplicateKey is not supported for dialect SQLite (supported by: MySQL, TiDB) at ON DUPLICATE KEY UPDATE name = VALUES(name)
  Errors encountered, no code generated
  [1]

Test SQLite dialect with ON CONFLICT, column-level primary key (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, column-level unique constraint (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, UNIQUE(id));
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, table-level primary key (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, PRIMARY KEY(id));
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, composite primary key (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, PRIMARY KEY(id, name));
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id, name) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, non-existent primary key (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  Failed : INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test SQLite dialect with ON CONFLICT, non-existent composite primary key (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT, address TEXT, PRIMARY KEY(id, name));
  > INSERT INTO users (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO UPDATE SET name = excluded.name;
  > EOF
  Failed : INSERT INTO users (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO UPDATE SET name = excluded.name
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id, address) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test SQLite dialect with ON CONFLICT, table-level unique constraint (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, UNIQUE(id));
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, composite unique constraint (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, UNIQUE(id, name));
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id, name) DO UPDATE SET name = excluded.name;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT, non-existent unique constraint (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  Failed : INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test SQLite dialect with ON CONFLICT, non-existent composite unique constraint (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT, name TEXT, address TEXT, UNIQUE(id, name));
  > INSERT INTO users (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO UPDATE SET name = excluded.name;
  > EOF
  Failed : INSERT INTO users (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO UPDATE SET name = excluded.name
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id, address) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test MySQL dialect with ON CONFLICT (should fail):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  Feature OnConflict is not supported for dialect MySQL (supported by: SQLite, PostgreSQL) at ON CONFLICT(id) DO UPDATE SET name = excluded.name
  Errors encountered, no code generated
  [1]

Test Json functions:
  $ /bin/sh ./sqlgg_test.sh json_functions.sql json_functions.compare.ml  
  $ echo $?
  0

Test Json functions and ocaml compiles:
  $ cd test_build_json_functions
  $ cat "json_functions.sql" | sqlgg -no-header -gen caml_io -params unnamed -gen caml - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c output.ml
  $ cp ../print_ocaml_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe output.ml print_ocaml_impl.ml test_run.ml
  $ ./test_run.exe
  Starting JSON Path Tests with Inline Mock Implementation
  ============================================================
  === Starting JSON Path Tests with Inline Mocking ===
  
  Running all tests...
  
  [TEST 1] Testing JSON parameter with nested structure
  [MOCK SELECT_ONE] Connection type: [> `RO ]
  [SQL] SELECT JSON_SEARCH('{\"users\":[{\"id\":1,\"settings\":{\"themes\":[\"dark\"]}}]}', 'one', 'dark')
  [MOCK] Returning one row
  [MOCK] get_column_Json_nullable[0] = Some "$.users[0].settings.themes[0]"
  [TEST 1] Result: completed
  
  [TEST 2] Testing JSON path parameter with combinators
  [MOCK SELECT_ONE] Connection type: [> `RO ]
  [SQL] SELECT JSON_ARRAY_APPEND('["a", ["b", "c"], "d"]', '$.data[*].users[last].settings', 1)
  [MOCK] Returning one row
  [MOCK] get_column_Json[0] = {"theme":"dark","language":"en"}
  [TEST 2] Result: completed
  
  [TEST 3] Testing SELECT with JSON path extraction
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT JSON_EXTRACT(col1, '$.name') FROM table1 WHERE id = 1
  [MOCK] Returning one row
  [MOCK] get_column_Json_nullable[0] = Some null (default)
  [TEST 3] Result: completed
  
  [TEST 4] Testing SELECT with nested JSON path
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT JSON_UNQUOTE(JSON_EXTRACT(col1, '$.user.email')) FROM table1 WHERE id = 2
  [MOCK] Returning one row
  [MOCK] get_column_Text_nullable[0] = Some "mock_text" (default)
  [TEST 4] Result: completed
  
  [TEST 5] Testing SELECT ONE with JSON path
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE table1 SET col1 = JSON_SET(col1, '$.last_login', NOW()) WHERE id = 3
  [MOCK] Execute result: affected_rows=1, insert_id=5
  [TEST 5] Result: completed
  
  [TEST 6] Testing simple SELECT with callback
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT JSON_CONTAINS(col1, '"value"') FROM table1 WHERE id = 4
  [MOCK] Returning one row
  [MOCK] get_column_Bool_nullable[0] = Some false (default)
  [TEST 6] Result: completed
  
  [TEST 7] Testing SELECT with JSON path filter
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, name FROM table2 WHERE JSON_EXTRACT(col1, '$.metadata.category') = '\"electronics\"'
  [MOCK] Returning 2 rows
    Row 0: col0=101 col1=Laptop 
  [MOCK] get_column_Text_nullable[1] = Some "Laptop"
  [MOCK] get_column_Int[0] = 101
    -> Callback executed for test7 with id and name
    Row 1: col0=102 col1=Phone 
  [MOCK] get_column_Text_nullable[1] = Some "Phone"
  [MOCK] get_column_Int[0] = 102
    -> Callback executed for test7 with id and name
  [TEST 7] Result: completed
  
  [TEST 8] Testing complex SELECT with multiple JSON paths
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT 
  id,
  JSON_EXTRACT(col1, '$.profile.name') as user_name,
  JSON_EXTRACT(col2, '$.settings.theme') as theme
  FROM table1 
  WHERE JSON_EXTRACT(col1, '$.profile.active') = 'true'
  [MOCK] Returning 1 rows
    Row 0: col0=1 col1=John Doe col2=dark 
  [MOCK] get_column_Json_nullable[2] = Some null (default)
  [MOCK] get_column_Json_nullable[1] = Some null (default)
  [MOCK] get_column_Int[0] = 1
    -> Callback executed for test8 with id, user_name, theme
  [TEST 8] Result: completed
  
  [TEST 9] Testing UPDATE with JSON path conditions
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE table2 
  SET col2 = JSON_SET(col2, '$.timestamps.last_update', NOW())
  WHERE JSON_EXTRACT(col1, '$.user.role') = '\"admin\"'
  [MOCK] Execute result: affected_rows=3, insert_id=42
  [TEST 9] Result: affected_rows=3, insert_id=42
  
  [TEST 10] Testing search with text parameter
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT JSON_SEARCH(col1, 'one', 'admin') FROM table1 WHERE id = 5
  [MOCK] Returning one row
  [MOCK] get_column_Json_nullable[0] = Some "$.test[0]"
  [TEST 10] Result: completed
  
  === All JSON Path Tests Completed Successfully ===
  Summary:
  - Tests 1-8, 10: completed with inline mocked data
  - Test 9: returned execute_response with affected_rows=3
  - All SQL queries were logged
  - Each test setup its own mock data
  
  ============================================================
  All tests executed successfully!

Test MySQL dialect with STRAIGHT_JOIN (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE orders (id INT, user_id INT, amount DECIMAL);
  > SELECT u.name, o.amount FROM users u STRAIGHT_JOIN orders o ON u.id = o.user_id;
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with STRAIGHT_JOIN (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT);
  > CREATE TABLE orders (id INT, user_id INT, amount DECIMAL);
  > SELECT u.name, o.amount FROM users u STRAIGHT_JOIN orders o ON u.id = o.user_id;
  > EOF
  Feature StraightJoin is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at STRAIGHT_JOIN
  Errors encountered, no code generated
  [1]


Test MySQL dialect with REPLACE INTO (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > REPLACE INTO users (id, name) VALUES (1, 'John');
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with REPLACE INTO (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT);
  > REPLACE INTO users (id, name) VALUES (1, 'John');
  > EOF
  Feature ReplaceInto is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at REPLACE INTO
  Errors encountered, no code generated
  [1]

Test MySQL dialect with LOCK IN SHARE MODE (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > SELECT * FROM users LOCK IN SHARE MODE;
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with LOCK IN SHARE MODE (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT);
  > SELECT * FROM users LOCK IN SHARE MODE;
  > EOF
  Feature LockInShareMode is not supported for dialect PostgreSQL (supported by: MySQL) at LOCK IN SHARE MODE
  Errors encountered, no code generated
  [1]

Test MySQL dialect with FULLTEXT INDEX (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE articles (
  >     id INT AUTO_INCREMENT PRIMARY KEY,
  >     title VARCHAR(255) NOT NULL,
  >     content TEXT,
  >     FULLTEXT INDEX idx_title_content (title, content)
  > );
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with FULLTEXT INDEX (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE articles (
  >     id INT AUTO_INCREMENT PRIMARY KEY,
  >     title VARCHAR(255) NOT NULL,
  >     content TEXT,
  >     FULLTEXT INDEX idx_title_content (title, content)
  > );
  > EOF
  Feature FulltextIndex is not supported for dialect PostgreSQL (supported by: MySQL) at FULLTEXT INDEX idx_title_content (title, content)
  Feature AutoIncrement is not supported for dialect PostgreSQL (supported by: SQLite, MySQL, TiDB) at AUTO_INCREMENT
  Errors encountered, no code generated
  [1]



Test MySQL dialect with UNSIGNED (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE counters (
  >     id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY
  > );
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with UNSIGNED (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE counters (
  >     id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY
  > );
  > EOF
  Feature AutoIncrement is not supported for dialect PostgreSQL (supported by: SQLite, MySQL, TiDB) at AUTO_INCREMENT
  Feature UnsignedTypes is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at INT UNSIGNED
  Errors encountered, no code generated
  [1]
Feature UnsignedTypes is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at INT UNSIGNED
Errors encountered, no code generated



Test SQLite dialect with JSON_EXTRACT operator (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE json_data (data JSON);
  > SELECT json_extract(data, '$.name') FROM json_data;
  > EOF
  $ echo $?
  0


Test SQLite dialect with AUTOINCREMENT (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (
  >     id INTEGER AUTOINCREMENT PRIMARY KEY,
  >     name TEXT
  > );
  > EOF
  $ echo $?
  0

Test PostgreSQL dialect with AUTOINCREMENT (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (
  >     id INTEGER AUTOINCREMENT PRIMARY KEY,
  >     name TEXT
  > );
  > EOF
  Feature AutoIncrement is not supported for dialect PostgreSQL (supported by: SQLite, MySQL, TiDB) at AUTOINCREMENT
  Errors encountered, no code generated
  [1]

Test MySQL dialect with LIMIT OFFSET syntax (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > SELECT * FROM users LIMIT 10 OFFSET 20;
  > EOF
  $ echo $?
  0

Test MySQL dialect with ROW LOCKING (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT);
  > SELECT * FROM users FOR UPDATE;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ROW LOCKING (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT);
  > SELECT * FROM users FOR UPDATE;
  > EOF
  Feature RowLocking is not supported for dialect SQLite (supported by: PostgreSQL, MySQL, TiDB) at FOR UPDATE
  Errors encountered, no code generated
  [1]

Test SQLite dialect with WHERE aliasing (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT, name TEXT, score INT);
  > SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100;
  > EOF
  $ echo $?
  0

Test MySQL dialect with WHERE aliasing (should fail):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT, score INT);
  > SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100;
  > EOF
  Failed : SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100
  Fatal error: exception Sqlgg.Sql.Schema.Error(_, "missing attribute : double_score")
  [2]

Test PostgreSQL dialect with WHERE aliasing (should fail):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT, score INT);
  > SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100;
  > EOF
  Failed : SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100
  Fatal error: exception Sqlgg.Sql.Schema.Error(_, "missing attribute : double_score")
  [2]

Test TiDB dialect with WHERE aliasing (should fail):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, name TEXT, score INT);
  > SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100;
  > EOF
  Failed : SELECT id, name, score * 2 as double_score FROM users WHERE double_score > 100
  Fatal error: exception Sqlgg.Sql.Schema.Error(_, "missing attribute : double_score")
  [2]

Fix issue 96 (should fail):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1
  > CREATE TABLE test (x INT, `key` VARBINARY(200));
  > SELECT x AS foo FROM test WHERE foo > 0;
  > EOF
  Failed : SELECT x AS foo FROM test WHERE foo > 0
  Fatal error: exception Sqlgg.Sql.Schema.Error(_, "missing attribute : foo")
  [2]

Fix issue 96 (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test (x INT, `key` VARBINARY(200));
  > SELECT x AS foo FROM test WHERE foo > 0;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT DO NOTHING (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test20250819 (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT DO NOTHING, table-level primary key (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test20250819 (id INT, name TEXT, PRIMARY KEY(id));
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT DO NOTHING, composite primary key (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test20250819 (id INT, name TEXT, PRIMARY KEY(id, name));
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id, name) DO NOTHING;
  > EOF
  $ echo $?
  0

Test SQLite dialect with ON CONFLICT DO NOTHING, unique constraint (should work):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE test20250819 (id INT, name TEXT, UNIQUE(id));
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  $ echo $?
  0

Test MySQL dialect with ON CONFLICT DO NOTHING (should fail):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE test20250819 (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  Feature OnConflict is not supported for dialect MySQL (supported by: SQLite, PostgreSQL) at ON CONFLICT(id) DO NOTHING
  Errors encountered, no code generated
  [1]

Test PostgreSQL dialect with ON CONFLICT DO NOTHING (should work):
  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' >/dev/null
  > CREATE TABLE test20250819 (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  $ echo $?
  0

Test TiDB dialect with ON CONFLICT DO NOTHING (should fail):
  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1 
  > CREATE TABLE test20250819 (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  Feature OnConflict is not supported for dialect TiDB (supported by: SQLite, PostgreSQL) at ON CONFLICT(id) DO NOTHING
  Errors encountered, no code generated
  [1]

Test SQLite dialect with ON CONFLICT DO NOTHING, non-existent primary key (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE test20250819 (id INT, name TEXT);
  > INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING;
  > EOF
  Failed : INSERT INTO test20250819 (id, name) VALUES (1, 'John') ON CONFLICT(id) DO NOTHING
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test SQLite dialect with ON CONFLICT DO NOTHING, non-existent composite constraint (should fail):
  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' 2>&1 
  > CREATE TABLE test20250819 (id INT, name TEXT, address TEXT, PRIMARY KEY(id, name));
  > INSERT INTO test20250819 (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO NOTHING;
  > EOF
  Failed : INSERT INTO test20250819 (id, name, address) VALUES (1, 'John', '123 Main St') ON CONFLICT(id, address) DO NOTHING
  Fatal error: exception Failure("Schema Error: ON CONFLICT clause (id, address) does not match the PRIMARY KEY or UNIQUE constraint for column: { Sql.cname = \"id\"; tname = None }")
  [2]

Test Single style for SELECT 1/0..1 and hide empty modules:
  $ sqlgg -gen caml_io -params unnamed -gen caml -no-header - <<'EOF' 2>&1 
  > CREATE TABLE test20250902 (id INT PRIMARY KEY, name TEXT);
  > SELECT COUNT(*) FROM test20250902;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_test20250902 db  =
      T.execute db ("CREATE TABLE test20250902 (id INT PRIMARY KEY, name TEXT)") T.no_params
  
    let select_1 db  =
      let get_row stmt =
        (T.get_column_Int stmt 0)
      in
      T.select_one db ("SELECT COUNT(*) FROM test20250902") T.no_params get_row
  
    module Single = struct
      let select_1 db  callback =
        let invoke_callback stmt =
          callback
            ~r:(T.get_column_Int stmt 0)
        in
        T.select_one db ("SELECT COUNT(*) FROM test20250902") T.no_params invoke_callback
  
    end (* module Single *)
  end (* module Sqlgg *)

Test MySQL CAST/CONVERT syntax (should work):
  $ sqlgg -gen caml -dialect=mysql - <<'EOF' >/dev/null
  > SELECT CAST('-9142586270102516767' AS UNSIGNED);
  > SELECT CONVERT('-9142586270102516767', UNSIGNED);
  > SELECT CONVERT(@a :: Text, UNSIGNED) + 2;
  > SELECT CAST('abc' AS CHAR(10));
  > SELECT CAST('0xdeadbeef' AS BINARY(16));
  > SELECT CAST('1.23' AS DECIMAL(10,2));
  > SELECT CAST('2024-01-01' AS DATE);
  > SELECT CAST('10:20:30' AS TIME);
  > SELECT CAST('2024-01-01 00:00:00' AS DATETIME);
  > SELECT CAST('{"a":1}' AS JSON);
  > EOF
  $ echo $?
  0

Test non_nullifiable when update:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL,
  >     updated_at_not_nullable DATETIME NOT NULL
  > );
  > UPDATE non_nullifiable SET updated_at = @updated_at WHERE name = 'example';
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_non_nullifiable db  =
      T.execute db ("CREATE TABLE non_nullifiable (\n\
      name TEXT,\n\
          updated_at DATETIME NULL,\n\
      updated_at_not_nullable DATETIME NOT NULL\n\
  )") T.no_params
  
    let update_non_nullifiable_1 db ~updated_at =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Datetime p updated_at;
        T.finish_params p
      in
      T.execute db ("UPDATE non_nullifiable SET updated_at = ? WHERE name = 'example'") set_params
  
  end (* module Sqlgg *)

Test non_nullifiable when update (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME,
  >     updated_at_not_nullable DATETIME NOT NULL
  > );
  > UPDATE non_nullifiable SET updated_at = @updated_at :: Datetime Null WHERE name = 'example';
  > EOF
  Failed : UPDATE non_nullifiable SET updated_at = @updated_at :: Datetime Null WHERE name = 'example'
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Datetime, Datetime?)")
  [2]

Test INSERT set NULL where NOT NULL col (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     id INT PRIMARY KEY,
  >     name TEXT NOT NULL
  > );
  > INSERT INTO non_nullifiable (id, name) VALUES (1, NULL);
  > EOF
  Failed : INSERT INTO non_nullifiable (id, name) VALUES (1, NULL)
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Text, Any?)")
  [2]

Test UPDATE set NULL where NOT NULL col (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     id INT PRIMARY KEY,
  >     x TEXT NOT NULL
  > );
  > UPDATE non_nullifiable SET x = @x + NULL WHERE id = 1;
  > EOF
  Failed : UPDATE non_nullifiable SET x = @x + NULL WHERE id = 1
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Text, Any?)")
  [2]


Test non_nullifiable when update set NULL (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL,
  >     updated_at_not_nullable DATETIME NOT NULL
  > );
  > UPDATE non_nullifiable SET updated_at = NULL WHERE name = 'example';
  > EOF
  Failed : UPDATE non_nullifiable SET updated_at = NULL WHERE name = 'example'
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Datetime, Any?)")
  [2]

Test non_nullifiable when update insert NULL:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE non_nullifiable (
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL,
  >     updated_at_not_nullable DATETIME NOT NULL
  > );
  > INSERT INTO non_nullifiable (name, updated_at) VALUES ('example', NULL);
  > EOF
  Failed : INSERT INTO non_nullifiable (name, updated_at) VALUES ('example', NULL)
  Fatal error: exception Failure("Fields: (updated_at_not_nullable) don't have a default value")
  [2]

Test non_nullifiable with multi-row INSERT (NULL allowed on insert):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_insert_multi (
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     published_at DATETIME
  > );
  > INSERT INTO nn_insert_multi (id, name, published_at) VALUES (1, 'a', NULL), (2, 'b', NULL);
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_nn_insert_multi db  =
      T.execute db ("CREATE TABLE nn_insert_multi (\n\
      id INT PRIMARY KEY,\n\
      name TEXT,\n\
          published_at DATETIME\n\
  )") T.no_params
  
    let insert_nn_insert_multi_1 db  =
      T.execute db ("INSERT INTO nn_insert_multi (id, name, published_at) VALUES (1, 'a', NULL), (2, 'b', NULL)") T.no_params
  
  end (* module Sqlgg *)

Test non_nullifiable with INSERT ... ON DUPLICATE KEY UPDATE (values nullable, NEW strict) (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_upsert (
  >     id INT PRIMARY KEY,
  >     -- [sqlgg] non_nullifiable=true
  >     column1 INT NULL,
  >     -- [sqlgg] non_nullifiable=true
  >     column2 VARCHAR(255) NULL
  > );
  > INSERT INTO nn_upsert (id, column1, column2)
  > VALUES (1, @v1, @v2)
  > ON DUPLICATE KEY UPDATE 
  >   column1 = VALUES(column1),
  >   column2 = VALUES(column2);
  > EOF
  Failed : INSERT INTO nn_upsert (id, column1, column2)
  VALUES (1, @v1, @v2)
  ON DUPLICATE KEY UPDATE 
    column1 = VALUES(column1),
    column2 = VALUES(column2)
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Int, Int?)")
  [2]

Test non_nullifiable with INSERT ... ON DUPLICATE KEY UPDATE (values nullable, NEW strict) (should work):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_upsert (
  >     id INT PRIMARY KEY,
  >     -- [sqlgg] non_nullifiable=true
  >     column1 INT NULL,
  >     -- [sqlgg] non_nullifiable=true
  >     column2 VARCHAR(255) NULL
  > );
  > INSERT INTO nn_upsert (id, column1, column2)
  > VALUES (1, @v1 :: Int, @v2 :: Text)
  > ON DUPLICATE KEY UPDATE 
  >   column1 = VALUES(column1),
  >   column2 = VALUES(column2);
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_nn_upsert db  =
      T.execute db ("CREATE TABLE nn_upsert (\n\
      id INT PRIMARY KEY,\n\
          column1 INT NULL,\n\
          column2 VARCHAR(255) NULL\n\
  )") T.no_params
  
    let insert_nn_upsert_1 db ~v1 ~v2 =
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Int p v1;
        T.set_param_Text p v2;
        T.finish_params p
      in
      T.execute db ("INSERT INTO nn_upsert (id, column1, column2)\n\
  VALUES (1, ?, ?)\n\
  ON DUPLICATE KEY UPDATE \n\
    column1 = VALUES(column1),\n\
    column2 = VALUES(column2)") set_params
  
  end (* module Sqlgg *)


Test non_nullifiable with INSERT ... SELECT (source may produce NULLs):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_src (
  >     id INT PRIMARY KEY,
  >     ts DATETIME
  > );
  > CREATE TABLE nn_dst (
  >     id INT PRIMARY KEY,
  >     -- [sqlgg] non_nullifiable=true
  >     published_at DATETIME
  > );
  > INSERT INTO nn_dst (id, published_at)
  > SELECT id, ts FROM nn_src;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_nn_src db  =
      T.execute db ("CREATE TABLE nn_src (\n\
      id INT PRIMARY KEY,\n\
      ts DATETIME\n\
  )") T.no_params
  
    let create_nn_dst db  =
      T.execute db ("CREATE TABLE nn_dst (\n\
      id INT PRIMARY KEY,\n\
          published_at DATETIME\n\
  )") T.no_params
  
    let insert_nn_dst_2 db  =
      T.execute db ("INSERT INTO nn_dst (id, published_at)\n\
  SELECT id, ts FROM nn_src") T.no_params
  
  end (* module Sqlgg *)

Test INSERT (.. ) VALUES @rows with non_nullifiable column (NULL allowed on insert):
  $ sqlgg -gen caml -no-header -params named -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_insert_param (
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     published_at DATETIME
  > );
  > INSERT INTO nn_insert_param (id, name, published_at) VALUES @rows;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_nn_insert_param db  =
      T.execute db ("CREATE TABLE nn_insert_param (\n\
      id INT PRIMARY KEY,\n\
      name TEXT,\n\
          published_at DATETIME\n\
  )") T.no_params
  
    let insert_nn_insert_param_1 db ~rows =
      ( match rows with [] -> IO.return { T.affected_rows = 0L; insert_id = None } | _ :: _ -> T.execute db ("INSERT INTO nn_insert_param (id, name, published_at) VALUES " ^ (let _sqlgg_b = Buffer.create 13 in List.iteri (fun _sqlgg_idx (id, name, published_at) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); Buffer.add_string _sqlgg_b (T.Types.Int.to_literal id); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match name with None -> "NULL" | Some v -> T.Types.Text.to_literal v); Buffer.add_string _sqlgg_b ", "; Buffer.add_string _sqlgg_b (match published_at with None -> "NULL" | Some v -> T.Types.Datetime.to_literal v); Buffer.add_char _sqlgg_b ')') rows; Buffer.contents _sqlgg_b)) T.no_params )
  
  end (* module Sqlgg *)

Test non_nullifiable with multi-table UPDATE (param must be non-nullable):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE nn_multi_t1 (
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     published_at DATETIME
  > );
  > CREATE TABLE nn_multi_t2 (
  >     id INT,
  >     ref TEXT
  > );
  > UPDATE nn_multi_t1 t1, nn_multi_t2 t2
  >   SET t1.published_at = @published_at
  > WHERE t2.ref = t1.name AND t2.id = 1;
  > EOF
  $ echo $?
  0

Test non_nullifiable with multi-table UPDATE forcing Null via type spec (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_multi_t3 (
  >     k TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     refreshed_at DATETIME
  > );
  > CREATE TABLE nn_multi_t4 (
  >     id INT,
  >     k TEXT
  > );
  > UPDATE nn_multi_t3 a INNER JOIN nn_multi_t4 b ON b.k = a.k
  >   SET a.refreshed_at = @refreshed_at :: Datetime Null
  > WHERE b.id = 42;
  > EOF
  Failed : UPDATE nn_multi_t3 a INNER JOIN nn_multi_t4 b ON b.k = a.k
    SET a.refreshed_at = @refreshed_at :: Datetime Null
  WHERE b.id = 42
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Datetime, Datetime?)")
  [2]

Test multi-table UPDATE set NULL where non_nullifiable col (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE nn_multi_t5 (
  >     k TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     locked_at DATETIME
  > );
  > CREATE TABLE nn_multi_t6 (
  >     id INT,
  >     k TEXT
  > );
  > UPDATE nn_multi_t5 a JOIN nn_multi_t6 b ON b.k = a.k
  >   SET a.locked_at = NULL
  > WHERE b.id = 7;
  > EOF
  Failed : UPDATE nn_multi_t5 a JOIN nn_multi_t6 b ON b.k = a.k
    SET a.locked_at = NULL
  WHERE b.id = 7
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Datetime, Any?)")
  [2]

Test non_nullifiable with UPDATE using subquery (should fail):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE nn_main (
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL
  > );
  > CREATE TABLE source_table (
  >     id INT PRIMARY KEY,
  >     ts DATETIME
  > );
  > UPDATE nn_main 
  > SET updated_at = (SELECT ts FROM source_table WHERE id = 1)
  > WHERE name = 'test';
  > EOF
  Failed : UPDATE nn_main 
  SET updated_at = (SELECT ts FROM source_table WHERE id = 1)
  WHERE name = 'test'
  Fatal error: exception Failure("Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Datetime, Datetime?)")
  [2]

Test non_nullifiable with UPDATE using subquery (should work):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' >/dev/null
  > CREATE TABLE nn_main (
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL
  > );
  > CREATE TABLE source_table (
  >     id INT PRIMARY KEY,
  >     ts DATETIME
  > );
  > UPDATE nn_main 
  > SET updated_at = COALESCE((SELECT ts FROM source_table WHERE id = 1), NOW())
  > WHERE name = 'test';
  > EOF
  $ echo $?
  0

Test non_nullifiable with UPDATE using subquery (should work when allow-write-notnull-null):
  $ sqlgg -gen caml -no-header -dialect=mysql -allow-write-notnull-null - <<'EOF' >/dev/null
  > CREATE TABLE nn_main (
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     -- [sqlgg] non_nullifiable=true
  >     updated_at DATETIME NULL
  > );
  > CREATE TABLE source_table (
  >     id INT PRIMARY KEY,
  >     ts DATETIME
  > );
  > UPDATE nn_main 
  > SET updated_at = (SELECT ts FROM source_table WHERE id = 1)
  > WHERE name = 'test';
  > EOF
  $ echo $?
  0

order by and limit are supported with update stmt + table alias:
  $ sqlgg -gen caml_io -params unnamed -gen caml -no-header - <<'EOF' 2>&1 
  > CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT);
  > UPDATE test t SET t.column_a = 'value' ORDER BY t.id DESC LIMIT 10;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_test db  =
      T.execute db ("CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT)") T.no_params
  
    let update_1 db  =
      T.execute db ("UPDATE test t SET t.column_a = 'value' ORDER BY t.id DESC LIMIT 10") T.no_params
  
  end (* module Sqlgg *)

parametrized order by and limit are supported with update stmt + table alias:
  $ sqlgg -gen caml_io -params unnamed -gen caml -no-header - <<'EOF' 2>&1 
  > CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT);
  > UPDATE test t SET t.column_a = 'value' ORDER BY @order{ Id {t.id} } @direction LIMIT @limit;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_test db  =
      T.execute db ("CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT)") T.no_params
  
    let update_1 db ~order ~direction ~limit =
      let set_params stmt =
        let p = T.start_params stmt (1 + (match order with `Id -> 0) + (match direction with `ASC -> 0 | `DESC -> 0)) in
        T.set_param_Int p limit;
        T.finish_params p
      in
      T.execute db ("UPDATE test t SET t.column_a = 'value' ORDER BY " ^ (match order with `Id -> "t.id") ^ " " ^ (match direction with `ASC -> "ASC" | `DESC -> "DESC") ^ " LIMIT ?") set_params
  
  end (* module Sqlgg *)

limit is supported with update stmt + table alias:
  $ sqlgg -gen caml_io -params unnamed -gen caml -no-header - <<'EOF' 2>&1 
  > CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT);
  > UPDATE test t SET t.column_a = 'value' LIMIT @limit;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_test db  =
      T.execute db ("CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT)") T.no_params
  
    let update_1 db ~limit =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p limit;
        T.finish_params p
      in
      T.execute db ("UPDATE test t SET t.column_a = 'value' LIMIT ?") set_params
  
  end (* module Sqlgg *)

order by and limit are supported with update stmt + table alias + join:
  $ sqlgg -gen caml_io -params unnamed -gen caml -no-header - <<'EOF' 2>&1 
  > CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT);
  > CREATE TABLE test2 (id INT PRIMARY KEY, column_a TEXT, test_id INT NOT NULL);
  > UPDATE test t JOIN test2 t2 ON t.id = t2.test_id SET t.column_a = @value, t2.column_a = @value ORDER BY @order{ Id_1 {t.id} | Id_2 {t2.id} } @direction LIMIT @limit;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_test db  =
      T.execute db ("CREATE TABLE test (id INT PRIMARY KEY, column_a TEXT)") T.no_params
  
    let create_test2 db  =
      T.execute db ("CREATE TABLE test2 (id INT PRIMARY KEY, column_a TEXT, test_id INT NOT NULL)") T.no_params
  
    let update_2 db ~value ~order ~direction ~limit =
      let set_params stmt =
        let p = T.start_params stmt (3 + (match order with `Id_1 -> 0 | `Id_2 -> 0) + (match direction with `ASC -> 0 | `DESC -> 0)) in
        begin match value with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        begin match value with None -> T.set_param_null p | Some v -> T.set_param_Text p v end;
        T.set_param_Int p limit;
        T.finish_params p
      in
      T.execute db ("UPDATE test t JOIN test2 t2 ON t.id = t2.test_id SET t.column_a = ?, t2.column_a = ? ORDER BY " ^ (match order with `Id_1 -> "t.id" | `Id_2 -> "t2.id") ^ " " ^ (match direction with `ASC -> "ASC" | `DESC -> "DESC") ^ " LIMIT ?") set_params
  
  end (* module Sqlgg *)
