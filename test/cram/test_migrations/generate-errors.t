Generation errors name what the user has to fix.

  $ cat > initial.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > SQL

A table that only the target schema has is created by neither the initial
schema nor a migration:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > CREATE TABLE posts (id INT NOT NULL PRIMARY KEY);
  > CREATE TABLE tags (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations --now 20261119000000 > migrations.ml
  the target schema has tables that neither the initial schema nor the migrations create: `posts`, `tags`
  Add them to the initial schema, or pass -ddl-as-migration (--ddl-as-migration for sqlgg migrations) to generate CREATE TABLE migrations.
  [1]
  $ test -e migrations.sql
  [1]

A manual migration that does not match the schema is reported by its id:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > SQL
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=20261119000000_rename_users
  > UPDATE users SET nickname = name;
  > UPDATE users SET name = nickname;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --manual manual.sql --module Migrations > migrations.ml
  Failed : UPDATE users SET nickname = name
  At : nickname
  migration 20261119000000_rename_users: missing attribute : nickname
  [1]
