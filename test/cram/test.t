Issue183 is fixed:
  $ cat registration_feedbacks.sql | sqlgg -gen caml - > output.ml

  $ grep -n "let p = T.start_params stmt" output.ml | head -1
  25:      let p = T.start_params stmt (1 + (match search with Some (_, _, xs, xss) -> 3 + (match xs with [] -> 0 | _ :: _ -> 0) + (match xss with `A _ -> 1 | `B _ -> 1) | None -> 0)) in

  $ sed -n '27,44p' output.ml
        begin match search with
        | None -> ()
        | Some (search,search2,xs,xss) ->
          T.set_param_Text p search;
          T.set_param_Text p search;
          T.set_param_Text p search2;
          begin match xs with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
          begin match xss with
          | `A (a) ->
            T.set_param_Text p a;
          | `B (b) ->
            T.set_param_Text p b;
          end;
        end;

  $ sed -n '49p' output.ml | grep -o "match search with Some (_, _, xs, xss)"
  match search with Some (_, _, xs, xss)

Support reusable queries as CTE:
  $ cat reusabe_queries_as_ctes.sql | sqlgg -gen caml - > output.ml

Compare test2 (with &abcd substitution) and test3 (without substitution):
  $ sed -n '125,139p' output.ml > test2_part.tmp
  $ sed -n '156,170p' output.ml > test3_part.tmp
  $ diff test2_part.tmp test3_part.tmp
  $ echo $?
  0

Compare SQL queries in test2 (with &abcd substitution) and test3 (without substitution) # 2:
  $ sed -n '140,150p' output.ml > test2_sql.tmp
  $ sed -n '171,181p' output.ml > test3_sql.tmp
  $ diff test2_sql.tmp test3_sql.tmp
  $ echo $?
  0

Implement set default feature:

  $ cat set_default_syntax.sql | sqlgg -params unnamed -gen caml - > output.ml

  $ sed -n '17,38p' output.ml
    let insert_registration_feedbacks_1 db ~user_message ~grant_types =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match user_message with Some _ -> 1 | None -> 0) + (match grant_types with Some (grant_types) -> 0 + (match grant_types with `A -> 0 | `B -> 0) | None -> 0)) in
        begin match user_message with
        | None -> ()
        | Some (user_message) ->
          T.set_param_Text p user_message;
        end;
        begin match grant_types with
        | None -> ()
        | Some (grant_types) ->
          begin match grant_types with
          | `A -> ()
          | `B -> ()
          end;
        end;
        T.finish_params p
      in
      T.execute db ("INSERT INTO `registration_feedbacks`\n\
  SET\n\
    `user_message` = " ^ (match user_message with Some _ -> " ( " ^ " CONCAT(" ^ "?" ^ ", '22222') " ^ " ) " | None -> " DEFAULT ") ^ ",\n\
    `grant_types` = " ^ (match grant_types with Some (grant_types) -> " ( " ^ " " ^ (match grant_types with `A -> "'2'" | `B -> "'2'") ^ " " ^ " ) " | None -> " DEFAULT ")) set_params

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
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT JSON_EXTRACT(col1, '$.name') FROM table1 WHERE id = 1
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Alice col2=Alice Johnson 
  [MOCK] get_column_Json_nullable[0] = Some null (default)
    -> Callback executed for test3 with result
    Row 1: col0=2 col1=Bob col2=Bob Smith 
  [MOCK] get_column_Json_nullable[0] = Some null (default)
    -> Callback executed for test3 with result
  [TEST 3] Result: completed
  
  [TEST 4] Testing SELECT with nested JSON path
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT JSON_UNQUOTE(JSON_EXTRACT(col1, '$.user.email')) FROM table1 WHERE id = 2
  [MOCK] Returning 1 rows
    Row 0: col0=2 col1=user@example.com 
  [MOCK] get_column_Text_nullable[0] = Some "mock_text" (default)
    -> Callback executed for test4 with result
  [TEST 4] Result: completed
  
  [TEST 5] Testing SELECT ONE with JSON path
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE table1 SET col1 = JSON_SET(col1, '$.last_login', NOW()) WHERE id = 3
  [MOCK] Execute result: affected_rows=1, insert_id=5
  [TEST 5] Result: completed
  
  [TEST 6] Testing simple SELECT with callback
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT JSON_CONTAINS(col1, '"value"') FROM table1 WHERE id = 4
  [MOCK] Returning 1 rows
    Row 0: col0=4 col1=active 
  [MOCK] get_column_Bool_nullable[0] = Some false (default)
    -> Callback executed for test6 with result
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
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT JSON_SEARCH(col1, 'one', 'admin') FROM table1 WHERE id = 5
  [MOCK] Returning 1 rows
    Row 0: col0="$.test[0]" 
  [MOCK] get_column_Json_nullable[0] = Some "$.test[0]"
    -> Callback executed for test10 with result
  [TEST 10] Result: completed
  
  === All JSON Path Tests Completed Successfully ===
  Summary:
  - Tests 1-8, 10: completed with inline mocked data
  - Test 9: returned execute_response with affected_rows=3
  - All SQL queries were logged
  - Each test setup its own mock data
  
  ============================================================
  All tests executed successfully!
