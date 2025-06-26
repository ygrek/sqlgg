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
