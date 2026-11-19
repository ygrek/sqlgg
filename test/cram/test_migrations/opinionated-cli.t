`sqlgg migrations generate` is the opinionated workflow: one schema file in,
migrations out, with the state table, the runner and the command line provided
by sqlgg. It needs no Dune and no baseline schema.

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY,
  >   legacy TEXT
  > );
  > CREATE TABLE posts (
  >   id INT NOT NULL PRIMARY KEY,
  >   author_id INT NOT NULL
  > );
  > SQL

The migration log does not exist yet. With `--ddl-as-migration` every table of
the schema becomes a `CREATE TABLE` migration, recorded in the log:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations --now 20261112000000 --ddl-as-migration > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20261112000000_create_posts, 20261112000000_create_users)
  $ cat migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20261112000000_create_posts
  CREATE TABLE `posts` (`id` INT NOT NULL, `author_id` INT NOT NULL, PRIMARY KEY (`id`));
  DROP TABLE `posts`;
  
  -- [sqlgg] generated
  -- [sqlgg] id=20261112000000_create_users
  CREATE TABLE `users` (`id` INT NOT NULL, `legacy` TEXT, PRIMARY KEY (`id`));
  DROP TABLE `users`;

The runtime module exposes only its typed plan:

  $ grep -c '^  let migrations' migrations.ml
  0
  [1]
  $ grep -c '^  module IO' migrations.ml
  0
  [1]
  $ awk '/^  let baseline =/{printing=1} printing {print}' migrations.ml
    let baseline = None
  
    let migration_plan :
        ([ `WR ] T.connection -> T.execute_response T.IO.future)
        Sqlgg_migrations.Migration.t
        list =
      [
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261112000000_create_posts")
          ~apply:apply_20261112000000_create_posts
          ~revert:revert_20261112000000_create_posts;
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261112000000_create_users")
          ~apply:apply_20261112000000_create_users
          ~revert:revert_20261112000000_create_users;
      ]
  
  end (* module Migrations *)

Running it again changes nothing, in the log or in the module:

  $ cp migrations.sql recorded.sql
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations > again.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ cmp migrations.sql recorded.sql
  $ cmp migrations.ml again.ml

The module can be written directly without shell redirection:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations --output direct.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ cmp migrations.ml direct.ml

A schema change is appended as a new migration. Hand-written migrations come
from `--manual` and are merged by id; one without a down statement is
irreversible, and its bare numeric id differs from its OCaml name:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY
  > );
  > CREATE TABLE posts (
  >   id INT NOT NULL PRIMARY KEY,
  >   author_id INT NOT NULL,
  >   title TEXT NOT NULL
  > );
  > SQL
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] irreversible=true
  > -- [sqlgg] id=20261113
  > ALTER TABLE users DROP COLUMN legacy;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --manual manual.sql --module Migrations --now 20261114000000 > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20261114000000_alter_posts_add_col_title)
  $ tail -n 4 migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20261114000000_alter_posts_add_col_title
  ALTER TABLE `posts` ADD COLUMN `title` TEXT NOT NULL;
  ALTER TABLE `posts` DROP COLUMN `title`;
  $ awk '/^  let migration_plan/{printing=1} printing {print}' migrations.ml
    let migration_plan :
        ([ `WR ] T.connection -> T.execute_response T.IO.future)
        Sqlgg_migrations.Migration.t
        list =
      [
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261112000000_create_posts")
          ~apply:apply_20261112000000_create_posts
          ~revert:revert_20261112000000_create_posts;
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261112000000_create_users")
          ~apply:apply_20261112000000_create_users
          ~revert:revert_20261112000000_create_users;
        Sqlgg_migrations.Migration.irreversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261113")
          ~apply:apply_alter_users_2;
        Sqlgg_migrations.Migration.reversible
          (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261114000000_alter_posts_add_col_title")
          ~apply:apply_20261114000000_alter_posts_add_col_title
          ~revert:revert_20261114000000_alter_posts_add_col_title;
      ]
  
  end (* module Migrations *)
  $ grep -c 'revert_alter_users_2' migrations.ml
  0
  [1]

The language is explicit, and only languages with a migration runtime are
accepted:

  $ sqlgg migrations generate --dialect mysql --schema schema.sql --migrations migrations.sql --module Migrations
  Usage: sqlgg migrations generate [--help] [OPTION]…
  sqlgg migrations: required option --lang is missing
  [2]
  $ sqlgg migrations generate --dialect mysql --lang java --schema schema.sql --migrations migrations.sql --module Migrations
  sqlgg migrations: there is no migration runtime for --lang java yet
                    (supported: ocaml)
  [2]

The generated OCaml module name is checked at the command boundary:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module foo-bar
  Usage: sqlgg migrations generate [--help] [OPTION]…
  sqlgg migrations: option '--module': invalid OCaml module name "foo-bar"
  [2]

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations --now=-1
  Usage: sqlgg migrations generate [--help] [OPTION]…
  sqlgg migrations: option '--now': invalid migration timestamp "-1"
  [2]

So are the dialects the runtime can keep state for:

  $ sqlgg migrations generate --dialect postgresql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations
  sqlgg migrations: the migration runtime does not support --dialect postgresql
                    yet (supported: mysql, tidb)
  [2]
  $ sqlgg migrations generate --dialect sqlite --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations
  sqlgg migrations: the migration runtime does not support --dialect sqlite yet
                    (supported: mysql, tidb)
  [2]

An id that does not fit the state table is refused when generating, not when
the application starts:

  $ printf -- '-- [sqlgg] manual\n-- [sqlgg] id=20261115_%0200d\nALTER TABLE users ADD COLUMN x INT;\nALTER TABLE users DROP COLUMN x;\n' 0 > long.sql
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --manual long.sql --module Migrations > /dev/null
  migration id is too long: 209 bytes, the limit is 191
  [1]

Plain and unique indexes are part of the `CREATE TABLE` itself, so a table that
has them still fits one entry of the log:

  $ mkdir indexed && cd indexed
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE t (
  >   a INT NOT NULL,
  >   b INT NOT NULL,
  >   UNIQUE KEY uk (a, b),
  >   KEY idx_b (b)
  > );
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations --now 20261116000000 --ddl-as-migration > /dev/null
  appended 1 migration(s) to migrations.sql (id 20261116000000_create_t)
  $ cat migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20261116000000_create_t
  CREATE TABLE `t` (`a` INT NOT NULL, `b` INT NOT NULL, KEY `idx_b` (`b`), UNIQUE KEY `uk` (`a`, `b`));
  DROP TABLE `t`;

A database that already exists is described by `--initial`. Its tables are the
starting point, and only what the schema changes becomes a migration:

  $ mkdir ../adopted && cd ../adopted
  $ cat > initial.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations --now 20261117000000 > /dev/null
  appended 1 migration(s) to migrations.sql (id 20261117000000_alter_users_add_col_name)
  $ cat migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20261117000000_alter_users_add_col_name
  ALTER TABLE `users` ADD COLUMN `name` TEXT;
  ALTER TABLE `users` DROP COLUMN `name`;

Without `--ddl-as-migration` a new table is not a migration. The schema DDL
creates it, so it has to be in `initial.sql` too; a table that only the schema
has is refused:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > CREATE TABLE tags (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations > /dev/null 2> refused.txt
  [1]
  $ cat refused.txt
  the target schema has tables that neither the initial schema nor the migrations create: `tags`
  Add them to the initial schema, or pass -ddl-as-migration (--ddl-as-migration for sqlgg migrations) to generate CREATE TABLE migrations.
  $ cat >> initial.sql <<'SQL'
  > CREATE TABLE tags (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations > /dev/null
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ grep -c '^-- \[sqlgg\] id=' migrations.sql
  1

Squashing folds old migrations into `initial.sql`; `--baseline` records the
cutoff in the module so their rows in `sqlgg_migrations` remain valid:

  $ cp schema.sql initial.sql && : > migrations.sql
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --baseline 20261117000000_alter_users_add_col_name --migrations migrations.sql --module Migrations | grep -A 2 'let baseline ='
  nothing to do
    let baseline =
      Some (Sqlgg_migrations.Migration_id.parse_generated ~id:"20261117000000_alter_users_add_col_name")
  

A hand-written block without an `id` is refused before touching the log:

  $ cat > no_id.sql <<'SQL'
  > -- [sqlgg] manual
  > ALTER TABLE users ADD COLUMN age INT;
  > ALTER TABLE users DROP COLUMN age;
  > SQL
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT, age INT, bio TEXT);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --manual no_id.sql --module Migrations > /dev/null
  migration has no id: add -- [sqlgg] id=<timestamp>_<name>
  [1]
  $ test ! -s migrations.sql
