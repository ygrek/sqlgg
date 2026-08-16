RETURNING clause (PostgreSQL/SQLite): INSERT/UPDATE/DELETE gain a result row, so the
generated code uses T.select* with a row binder instead of T.execute. When the number
of returned rows is statically known (single tuple INSERT / INSERT ... SET) the
single-row variants are used instead of the many-rows callback.

  $ cat returning.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect postgresql - > output.ml
  $ diff output.ml returning.compare.ml

The generated module must typecheck; its inferred interface pins the cardinality of the
RETURNING rowset per statement shape (single tuple VALUES / SET return the row directly,
ON CONFLICT DO NOTHING returns an option, everything else takes a row callback) as well
as column nullability (option types):

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -i output.ml > output.mli
  $ diff output.mli returning.compare.mli

SQLite supports RETURNING as well:

  $ sqlgg -gen caml -dialect=sqlite - <<'EOF' >/dev/null
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') RETURNING id;
  > EOF
  $ echo $?
  0

MySQL does not support RETURNING (should fail):

  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') RETURNING id;
  > EOF
  Feature Returning is not supported for dialect MySQL (supported by: PostgreSQL, SQLite) at RETURNING id
  Errors encountered, no code generated
  [1]

  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > UPDATE users SET name = 'John' WHERE id = 1 RETURNING id;
  > EOF
  Feature Returning is not supported for dialect MySQL (supported by: PostgreSQL, SQLite) at RETURNING id
  Errors encountered, no code generated
  [1]

  $ sqlgg -gen caml -dialect=tidb - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > DELETE FROM users WHERE id = 1 RETURNING id;
  > EOF
  Feature Returning is not supported for dialect TiDB (supported by: PostgreSQL, SQLite) at RETURNING id
  Errors encountered, no code generated
  [1]

The check can be disabled like any other dialect feature check:

  $ sqlgg -gen caml -dialect=mysql -no-check=returning - <<'EOF' 2>&1 >/dev/null | grep -i "warning"
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users (id, name) VALUES (1, 'John') RETURNING id;
  > EOF
  Warning: Feature Returning is not supported for dialect MySQL, proceeding anyway at RETURNING id

RETURNING is not supported when the inserted columns are inferred : the generated
VALUES tuple / SET assignments are appended at the end of the statement, which would
put them after the RETURNING clause:

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users VALUES RETURNING id;
  > EOF
  Failed : INSERT INTO users VALUES RETURNING id
  At : RETURNING id
  Fatal error: exception Failure("RETURNING is not supported when inserted columns are inferred")
  [2]

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users SET RETURNING id, CONCAT(name, @suffix) AS tagged;
  > EOF
  Failed : INSERT INTO users SET RETURNING id, CONCAT(name, @suffix) AS tagged
  At : RETURNING id, CONCAT(name, @suffix) AS tagged
  Fatal error: exception Failure("RETURNING is not supported when inserted columns are inferred")
  [2]

For the same reason a conflict clause cannot follow inferred columns either:

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users VALUES ON CONFLICT(id) DO NOTHING;
  > EOF
  Failed : INSERT INTO users VALUES ON CONFLICT(id) DO NOTHING
  At : ON CONFLICT(id) DO NOTHING
  Fatal error: exception Failure("ON CONFLICT is not supported when inserted columns are inferred")
  [2]

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users SET ON CONFLICT(id) DO UPDATE SET name = excluded.name;
  > EOF
  Failed : INSERT INTO users SET ON CONFLICT(id) DO UPDATE SET name = excluded.name
  At : ON CONFLICT(id) DO UPDATE SET name = excluded.name
  Fatal error: exception Failure("ON CONFLICT is not supported when inserted columns are inferred")
  [2]

  $ sqlgg -gen caml -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > INSERT INTO users VALUES ON DUPLICATE KEY UPDATE name = 'x';
  > EOF
  Failed : INSERT INTO users VALUES ON DUPLICATE KEY UPDATE name = 'x'
  At : ON DUPLICATE KEY UPDATE name = 'x'
  Fatal error: exception Failure("ON DUPLICATE KEY UPDATE is not supported when inserted columns are inferred")
  [2]

Unknown columns in RETURNING are rejected:

  $ sqlgg -gen caml -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
  > DELETE FROM users WHERE id = 1 RETURNING nope;
  > EOF
  Failed : DELETE FROM users WHERE id = 1 RETURNING nope
  Fatal error: exception Sqlgg.Sql.Schema.Error(_, "missing attribute : nope")
  [2]
