---
sidebar_position: 3
title: Migrations
description: Generate schema migrations from schema diffs
---

# Migrations

You keep the schema as plain [`CREATE TABLE`](./sql/ddl.md) statements and edit it as the project changes. sqlgg compares the old version with the new one and writes the up and down `ALTER TABLE` statements that get you from one to the other, covering columns, types and indices.

- **`sqlgg migrations`** is the complete workflow for OCaml on MySQL, MariaDB and TiDB: one schema file in, with the state table, the runner and the command line provided by sqlgg. Start here.
- **`-diff`** computes the delta between two schemas and prints it.
- **`-migrate`** appends new changes to a migrations file and regenerates the migration module from the recorded history. Use it when you bring your own runner or generate for another toolchain.

`-gen xml` writes the migrations out for another toolchain to pick up:

```bash
sqlgg -diff -gen xml -base initial.sql -target target.sql
```

```xml
<migrations>
 <migration name="20260101000000_alter_users_add_col_age" apply="ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL" revert="ALTER TABLE `users` DROP COLUMN `age`"/>
</migrations>
```

## The complete workflow: `sqlgg migrations`

Keep the whole schema in one file and edit it as the project changes:

```sql title="schema.sql"
CREATE TABLE users (
  id INT NOT NULL PRIMARY KEY,
  name TEXT NOT NULL
);
```

### Generate

```bash
sqlgg migrations generate --dialect mysql --lang ocaml \
  --schema schema.sql --migrations migrations.sql \
  --module Migrations > migrations.ml
```

```
appended 1 migration(s) to migrations.sql (id 20261112093000_create_users)
```

sqlgg replays `migrations.sql`, compares the result with `schema.sql`, appends the difference to `migrations.sql` and prints the migrations module. The first run starts from an empty database, so every table becomes a `CREATE TABLE` migration; `migrations.sql` is created if it does not exist. Running it again without touching the schema changes nothing. Commit `schema.sql`, `migrations.sql` and, unless a build rule regenerates it, `migrations.ml`.

This needs no build system. `--lang` is required: the generated module is tied to a migration runtime, and only `ocaml` has one so far. `--dialect` accepts `mysql` (also for MariaDB) and `tidb`.

Changes the differ must not guess, such as renames and data backfills, go into a hand-written file passed as `--manual manual.sql`. It uses the [block format described below](#manual-migrations) and is merged into the history by `id`.

### Apply

Add `sqlgg.migrations` and `sqlgg.migrations.cmdliner` to the libraries of your executable. `Db` below is your [traits module](./ocaml/traits.md) with control IO, for example one built with `Sqlgg_mariadb.Make_control_io`:

```ocaml
module Runner = Sqlgg_migrations.Runner.Make (Db) (Migrations.Migrations (Db))

module Migrate =
  Sqlgg_migrations_cmdliner.Command.Make (Db) (Runner)
    (struct
      (* whatever your command line needs to reach the database *)
      type options = string

      let options =
        Cmdliner.Arg.(required & opt (some string) None & info [ "db" ])

      (* run the future against one writable connection, to completion *)
      let with_connection url run = My_pool.with_connection url run
    end)

let () = exit (Cmdliner.Cmd.eval' Migrate.command)
```

`Migrate.command` is a Cmdliner group; mount it next to the other commands of your application. sqlgg never opens a connection itself.

| Command | Does | Exit code |
|---------|------|-----------|
| `migrate` | Applies every pending migration as one batch | `0`, or `2` on failure |
| `rollback` | Reverts the most recent batch, newest migration first | `0`, or `2` on failure |
| `pending` | Lists the migrations that are not applied yet | `0` if none, `1` if some, `2` on failure |

Without Cmdliner, call `Runner.migrate`, `Runner.rollback` and `Runner.pending` directly; they return the affected ids or a structured error.

### The state table

The runner creates and owns one table, `sqlgg_migrations`:

```sql
CREATE TABLE IF NOT EXISTS `sqlgg_migrations` (
  `migration_id` VARCHAR(191) NOT NULL PRIMARY KEY,
  `batch` BIGINT UNSIGNED NOT NULL,
  `state` ENUM('applying', 'applied', 'reverting', 'apply_failed', 'revert_failed') NOT NULL,
  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)
```

Before doing anything the runner checks that the applied migrations are exactly a prefix of the compiled ones. A stored id the application does not know, a migration missing in the middle, or a row that is not `applied` stops it without touching the database.

DDL commits implicitly on the MySQL family, so a migration cannot be wrapped in a transaction. The runner writes the row as `applying` before the migration runs and marks it `applied` after. If the migration fails, the row becomes `apply_failed` (`revert_failed` for a rollback) and every later run refuses to continue. Nothing is retried, because a failed `ALTER` may have been partly executed. To recover, inspect the database, bring the schema to the state before or after that migration by hand, then either delete the row (the migration will run again) or set its `state` to `applied`.

`rollback` refuses, before reverting anything, if the latest batch contains an irreversible migration.

### Dune (optional)

For a Dune project sqlgg prints the rules itself:

```bash
sqlgg migrations dune --dialect mysql --lang ocaml \
  --schema schema.sql --migrations migrations.sql \
  --generated migrations.ml --module Migrations > sqlgg_migrations.inc
```

```lisp title="dune"
(include sqlgg_migrations.inc)
```

The rules regenerate `migrations.ml` on every build from a copy of `migrations.sql`, which must exist (start with an empty file). After a schema change the copy has migrations the committed log lacks: `dune build @sqlgg-migrations` (also part of `@check`) shows them as a diff and `dune promote` accepts them. `migrations.ml` is a build target here and is not committed.

Until you promote, each build stamps the new migration with a fresh id. Promote before applying it to a database you keep.

## Generate migrations with `-diff`

Take a schema and put it into two files, `initial.sql` and `target.sql`, identical for now:

```sql
CREATE TABLE users (
  id INT NOT NULL,
  name TEXT
);
```

`initial.sql` is the state of the database at the moment you adopt the tool. All further edits to existing tables happen in `target.sql`. Add a column there:

```diff title="target.sql"
 CREATE TABLE users (
   id INT NOT NULL,
-  name TEXT
+  name TEXT,
+  age INT NOT NULL
 );
```

Run sqlgg on the two files to generate the migration, collecting the output in `migrations.sql`:

```bash
sqlgg -dialect mysql -diff -gen sql -base initial.sql -target target.sql >> migrations.sql
```

```sql title="migrations.sql"
-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_age
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
ALTER TABLE `users` DROP COLUMN `age`;
```

A migration is an up statement, its down statement right after it, and a sortable `id` of the form `<timestamp>_<descriptive_name>`. Use `-now YYYYMMDDHHMMSS` to pin the timestamp (useful for reproducible builds and tests).

The next edit to `target.sql` changes the column type and adds an index:

```diff title="target.sql"
 CREATE TABLE users (
   id INT NOT NULL,
-  name TEXT,
-  age INT NOT NULL
+  name VARCHAR(128) NOT NULL,
+  age INT NOT NULL,
+  INDEX idx_age (age)
 );
```

Since `-diff` relies on the previous state, save the current target before making further edits:

```bash
cp target.sql previous.sql
# edit target.sql...
sqlgg -dialect mysql -diff -gen sql -base previous.sql -target target.sql >> migrations.sql
```

```diff title="migrations.sql"
 -- [sqlgg] generated
 -- [sqlgg] id=20260101000000_alter_users_add_col_age
 ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
 ALTER TABLE `users` DROP COLUMN `age`;
+-- [sqlgg] generated
+-- [sqlgg] id=20260115000000_alter_users_change_col_name_add_index_idx_age
+ALTER TABLE `users` CHANGE COLUMN `name` `name` VARCHAR(128) NOT NULL, ADD INDEX `idx_age` (`age`);
+ALTER TABLE `users` CHANGE COLUMN `name` `name` TEXT, DROP INDEX `idx_age`;
```

Both edits landed in one migration, with changes to the same table folded into a single `ALTER TABLE`. Tracking the previous target by hand gets tedious. The [`-migrate`](#keep-a-migration-log-with--migrate) command automates it.

While plain SQL output works with any migration runner, sqlgg can also generate ready-to-use OCaml migration modules. Support for generating code in other languages is planned. This uses `-migrate`:

```bash
sqlgg -dialect mysql -migrate -gen caml -name mig \
  -initial initial.sql -migrations-file migrations.sql -target target.sql
```

```ocaml
module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_add_col_age db =
    T.execute db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL") T.no_params

  let revert_20260101000000_alter_users_add_col_age db =
    T.execute db ("ALTER TABLE `users` DROP COLUMN `age`") T.no_params

  let apply_20260115000000_alter_users_change_col_name_add_index_idx_age db =
    T.execute db ("ALTER TABLE `users` CHANGE COLUMN `name` `name` VARCHAR(128) NOT NULL, ADD INDEX `idx_age` (`age`)") T.no_params

  let revert_20260115000000_alter_users_change_col_name_add_index_idx_age db =
    T.execute db ("ALTER TABLE `users` CHANGE COLUMN `name` `name` TEXT, DROP INDEX `idx_age`") T.no_params

  let migrations = [
    ("20260101000000_alter_users_add_col_age",
     apply_20260101000000_alter_users_add_col_age,
     revert_20260101000000_alter_users_add_col_age);
    ("20260115000000_alter_users_change_col_name_add_index_idx_age",
     apply_20260115000000_alter_users_change_col_name_add_index_idx_age,
     revert_20260115000000_alter_users_change_col_name_add_index_idx_age);
  ]

end
```

`migrations` is an ordered list of `(id, apply, revert)` triples with the entire history, both rounds of edits in order. Feed it to your own runner: track applied ids in a table, apply the missing ones in order.

## Keep a migration log with `-migrate`

A typical project layout using `-migrate`:

- `initial.sql`: the schema as it was when you adopted the tool. Depending on your approach, it either gets updated with new tables or stays completely frozen (see [New tables](#new-tables)).
- `target.sql`: the current desired schema. All schema edits happen here.
- `migrations.sql`: the log of generated migrations. sqlgg writes it and only ever appends, do not edit it by hand. Manual migrations belong in `extends.sql`.
- `extends.sql`: optional manual migrations, merged into the history by `id`.

Run `-migrate` to find what changed in `target.sql`. It appends the new `ALTER` statements to `migrations.sql` and updates the OCaml module:

```bash
sqlgg -dialect mysql -migrate -gen caml -name migrations \
  -initial initial.sql \
  -migrations-file migrations.sql \
  -extends extends.sql \
  -target target.sql > migrations.ml
```

```
appended 2 migration(s) to migrations.sql (id 20260101000000_alter_users_add_col_name, ...)
```

If nothing changed in `target.sql`, the command does nothing:

```
nothing new to migrate; regenerated code from 6 recorded migration(s)
```

Edit `target.sql`, run `-migrate`, and commit `target.sql` together with the appended `migrations.sql` and the regenerated code.

### Manual migrations

Some changes must not be left to the differ. Rename a column and the diff sees one column gone and another one added:

```sql
-- what -diff generates for a rename
ALTER TABLE `users` DROP COLUMN `name`, ADD COLUMN `full_name` VARCHAR(128) NOT NULL;
```

This applies cleanly and destroys all data in the column. Write the rename by hand in the `-extends` file instead:

```sql title="extends.sql"
-- [sqlgg] id=20260301000000_rename_users_name
ALTER TABLE users RENAME COLUMN name TO full_name;
ALTER TABLE users RENAME COLUMN full_name TO name;
```

Use the same mechanism for data backfills and other changes that cannot be inferred from a schema diff.

The statement tagged with `id` is the up. The next untagged statement is its down, here the reverse rename. Blocks from `-extends` and `-migrations-file` are merged and ordered by `id`. An `id` may be a full `<timestamp>_<name>` or just a date, a bare `20251231` sorts as the start of that day.

Block annotations:

| Annotation | Meaning |
|------------|---------|
| `-- [sqlgg] id=<id>` | Migration id; determines ordering. The next untagged statement becomes the down |
| `-- [sqlgg] irreversible` | No down migration exists for this block |
| `-- [sqlgg] generated` | Marks blocks written by sqlgg (do not add manually) |

If a diff step is irreversible (e.g. a charset conversion that loses information), sqlgg marks it `irreversible`; the generated code will refuse to revert past it.

## Materializing the schema

`-gen sql` with `-base` files (without `-diff`) replays the history and prints the resulting schema as clean DDL. Use this to bootstrap a fresh database or check the current state:

```bash
sqlgg -dialect mysql -gen sql -base initial.sql -base extends.sql -base migrations.sql
```

The output is verified to round-trip: replaying the printed DDL must reproduce the same schema.

## New tables

By default a new table in the target is **not** turned into a migration. If `target.sql` has a table that nothing in the history creates, sqlgg refuses to guess and fails with `migration verification failed`. Pick one of two strategies for creating tables.

### Initialize the database from schema files (recommended)

Use [`CREATE TABLE IF NOT EXISTS`](./sql/ddl.md#keeping-the-schema-in-sync). Run the schema files directly against the database. Add new tables to both `initial.sql` and `target.sql`. Repeated runs are safe. Migrations will only contain changes to existing tables.

### Everything is a migration

Pass `-ddl-as-migration` and a new table in the target becomes a regular migration, `CREATE TABLE` as the up and `DROP TABLE` as the down, appended to the log like any other change:

```sql
-- [sqlgg] generated
-- [sqlgg] id=20260201000000_create_orders
CREATE TABLE `orders` (`id` INT NOT NULL, `user_id` INT NOT NULL);
DROP TABLE `orders`;
```

With this strategy `initial.sql` is truly frozen and migrations are the only channel to the database.
