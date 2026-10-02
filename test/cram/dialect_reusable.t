A reusable statement produces no query of its own, its dialect features are
still checked:

  $ sqlgg -no-header -dialect sqlite -gen caml - <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL);
  > -- @locked | include: reuse
  > SELECT id FROM users FOR UPDATE;
  > EOF
  Feature RowLocking is not supported for dialect SQLite (supported by: PostgreSQL, MySQL, TiDB) at FOR UPDATE
  Errors encountered, no code generated
  [1]
