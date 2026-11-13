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

The migration log does not exist yet. Every table of the schema becomes a
`CREATE TABLE` migration, recorded in the log:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --module Migrations --now 20261112000000 > migrations.ml
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

Besides the legacy `migrations` triples the module carries a typed plan, which
is what the runtime consumes:

  $ grep -c '^    ("' migrations.ml
  2
  $ awk '/^  module Migration =/{printing=1} printing {print}' migrations.ml
    module Migration = Sqlgg_migrations.Migration.Make (T)
  
    let migration_plan =
      [
        Migration.make_generated_reversible
          ~id:"20261112000000_create_posts"
          ~apply:apply_20261112000000_create_posts
          ~revert:revert_20261112000000_create_posts;
        Migration.make_generated_reversible
          ~id:"20261112000000_create_users"
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
  $ awk '/^  let migration_plan =/{printing=1} printing {print}' migrations.ml
    let migration_plan =
      [
        Migration.make_generated_reversible
          ~id:"20261112000000_create_posts"
          ~apply:apply_20261112000000_create_posts
          ~revert:revert_20261112000000_create_posts;
        Migration.make_generated_reversible
          ~id:"20261112000000_create_users"
          ~apply:apply_20261112000000_create_users
          ~revert:revert_20261112000000_create_users;
        Migration.make_generated_irreversible
          ~id:"20261113"
          ~apply:apply_alter_users_2;
        Migration.make_generated_reversible
          ~id:"20261114000000_alter_posts_add_col_title"
          ~apply:apply_20261114000000_alter_posts_add_col_title
          ~revert:revert_20261114000000_alter_posts_add_col_title;
      ]
  
  end (* module Migrations *)

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

The generated module needs only the public runtime packages. An application
gets the runner from `sqlgg.migrations` and the command line from
`sqlgg.migrations.cmdliner`, and decides itself how to reach the database (here
a mock that prints what it is asked to run and reports an empty state table):

  $ cp ../print_impl.ml .
  $ cat > app.ml <<'ML'
  > module Runner =
  >   Sqlgg_migrations.Runner.Make (Print_impl) (Migrations.Migrations (Print_impl))
  > 
  > module Command =
  >   Sqlgg_migrations_cmdliner.Command.Make (Print_impl) (Runner)
  >     (struct
  >       type options = unit
  > 
  >       let options = Cmdliner.Term.const ()
  > 
  >       let with_connection () run =
  >         Print_impl.response_queue :=
  >           [
  >             Print_impl.MockExecute { affected_rows = 0L; insert_id = None };
  >             Print_impl.MockSelect [];
  >           ];
  >         run ()
  >     end)
  > 
  > let () = exit (Cmdliner.Cmd.eval' Command.command)
  > ML
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.migrations,sqlgg.migrations.cmdliner -linkpkg print_impl.ml migrations.ml app.ml -o app.exe
  $ ./app.exe pending
  [MOCK EXECUTE] Connection type: [> `WR ]
  [MOCK] PREPARE[1]: CREATE TABLE IF NOT EXISTS `sqlgg_migrations` (
    `migration_id` VARCHAR(191) NOT NULL PRIMARY KEY,
    `batch` BIGINT UNSIGNED NOT NULL,
    `state` ENUM(
      'applying',
      'applied',
      'reverting',
      'apply_failed',
      'revert_failed'
    ) NOT NULL,
    `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      ON UPDATE CURRENT_TIMESTAMP
  )
  [SQL] CREATE TABLE IF NOT EXISTS `sqlgg_migrations` (
    `migration_id` VARCHAR(191) NOT NULL PRIMARY KEY,
    `batch` BIGINT UNSIGNED NOT NULL,
    `state` ENUM(
      'applying',
      'applied',
      'reverting',
      'apply_failed',
      'revert_failed'
    ) NOT NULL,
    `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      ON UPDATE CURRENT_TIMESTAMP
  )
  [MOCK] Execute result: affected_rows=0, insert_id=None
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT `migration_id`, CAST(`batch` AS CHAR), `state` FROM `sqlgg_migrations`
  [SQL] SELECT `migration_id`, CAST(`batch` AS CHAR), `state` FROM `sqlgg_migrations`
  [MOCK] Returning 0 rows
  20261112000000_create_posts
  20261112000000_create_users
  20261113
  20261114000000_alter_posts_add_col_title
  [1]
