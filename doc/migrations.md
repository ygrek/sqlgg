---
sidebar_position: 3
title: Migrations
description: Generate schema migrations from schema diffs
---

# Migrations

sqlgg compares a desired schema with its migration history and generates ordered
up and down SQL. OCaml applications can also use the migration runtime to apply
that SQL, store its state and roll back the latest batch.

- **`sqlgg migrations generate`** is the complete OCaml workflow for MySQL,
  MariaDB and TiDB.
- **`-diff`** prints the difference between two schemas.
- **`-migrate`** exposes the lower-level migration generator for every supported
  output format.

`-gen xml` writes the migrations out for another toolchain to pick up:

```bash
sqlgg -diff -gen xml -base initial.sql -target target.sql
```

```xml
<migrations>
 <migration name="20260101000000_alter_users_add_col_age" apply="ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL" revert="ALTER TABLE `users` DROP COLUMN `age`"/>
</migrations>
```

## OCaml migration workflow

The workflow uses these committed files:

- `schema.sql` is the desired schema.
- `migrations.sql` is the append-only generated history.
- `manual.sql` optionally contains hand-written migrations.
- `migrations.ml` is the generated OCaml module.

Start with the current schema:

```sql title="schema.sql"
CREATE TABLE users (
  id INT NOT NULL PRIMARY KEY,
  name TEXT NOT NULL
);
```

### Generate the migration

```bash
sqlgg migrations generate --dialect mysql --lang ocaml \
  --schema schema.sql --migrations migrations.sql \
  --module Migrations --ddl-as-migration --output migrations.ml
```

```
appended 1 migration(s) to migrations.sql (id 20261112093000_create_users)
```

The command starts from `--initial` when supplied, replays generated and manual
migrations in id order, and compares the result with `--schema`. It appends the
new difference to `migrations.sql` and regenerates `migrations.ml`. The manual
file is read-only. Without `--output`, the generated module goes to standard
output. With `--output`, `--line-directives` adds `# <line> "<file.sql>"`
directives, so that OCaml and merlin report errors in `apply_*` and `revert_*`
on the line of the migration in its SQL file.

`--lang ocaml` selects the OCaml migration runtime. `--dialect mysql` covers
MySQL and MariaDB; use `--dialect tidb` for TiDB.

The generated module is a functor over the database traits. The runner and the
command take it as is:

```ocaml
module Migrations : Sqlgg_migrations_state_table.Generated_migrations
```

It contains the optional baseline and the typed migration plan. The main inputs
are:

- `--initial initial.sql` sets the schema from before the migration log.
- `--manual manual.sql` reads renames, data backfills and other
  [manual migrations](#manual-migrations).
- `--baseline <id>` records a squash cutoff in the generated module. Stored
  migrations through that id remain valid as archived rows.
- `--ddl-as-migration` records new tables as `CREATE TABLE` migrations.

The command creates `migrations.sql` on the first run. See
[New tables](#new-tables) for the two ways to handle new tables.

### Build the migration command

Add `sqlgg.migrations.state_table` and `sqlgg.migrations.cmdliner` to the
libraries of the migration executable. `Db` is a [traits
module](./ocaml/traits.md) with control IO, such as one built with
`Sqlgg_mariadb.Make_control_io`.

`Command.Make` combines the generated migrations, the strict-order state-table
runner and the command:

```ocaml
module Migrate =
  Sqlgg_migrations_cmdliner.Command.Make
    (Db)
    (Migrations.Migrations)
    (Connection)

let () = exit (Cmdliner.Cmd.eval' Migrate.command)
```

`Connection` is an application module. It matches
`Sqlgg_migrations_cmdliner.Command.Connection` for the connections and futures
of `Db`:

```ocaml
module Connection : sig
  type options
  type connection_error

  val options : options Cmdliner.Term.t
  val connection_error_to_string : connection_error -> string

  val with_connection :
    options ->
    ([ `RO | `WR ] Db.connection -> 'a Db.IO.future) ->
    ('a, connection_error) result
end
```

`with_connection` must keep the connection open until the returned future
finishes. `Migrate.command` can be the main Cmdliner command or a subgroup of
another command.

To call the runner directly, instantiate it without Cmdliner:

```ocaml
module Runner =
  Sqlgg_migrations_state_table.Make_runner (Db) (Migrations.Migrations)
```

Call `Runner.migrate`, `Runner.rollback` and `Runner.pending` directly. They
return the affected ids or a structured error. `Make_runner` requires the
applied migrations to form a prefix of the compiled history and uses the
`sqlgg_migrations` state table. For another history policy, table name or state
store, compose `Sqlgg_migrations.Runner.Make_with_store` with a store such as
`Sqlgg_migrations_state_table.Make_state_store`. To expose that runner through
Cmdliner, pass it to `Command.Make_with_runner`.

### Run migrations

`pending` prints migrations that are waiting and exits with 1:

```console
$ ./migrate.exe pending
20261112093000_create_users
$ echo $?
1
```

`migrate` applies every pending migration as one batch:

```console
$ ./migrate.exe migrate
applied 20261112093000_create_users
```

The database is now current:

```console
$ ./migrate.exe pending
$ echo $?
0
```

`rollback` reverts the latest batch, newest migration first:

```console
$ ./migrate.exe rollback
reverted 20261112093000_create_users
```

Connection and runner failures exit with 2. Invalid command-line arguments use
Cmdliner's standard CLI error code.

### State and failures

The runner stores migration state in `sqlgg_migrations`:

```sql
CREATE TABLE IF NOT EXISTS `sqlgg_migrations` (
  `migration_id` VARBINARY(191) NOT NULL PRIMARY KEY,
  `batch` BIGINT NOT NULL,
  `state` ENUM('applying', 'applied', 'reverting', 'apply_failed', 'revert_failed') NOT NULL,
  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)
```

Migration ids have a 191-byte limit. sqlgg shortens generated ids to fit and
rejects an oversized manual id.

`Planner.Linear` requires applied migrations to form a prefix of the compiled
migration list. Stored ids through `baseline` may be absent from the compiled
plan because they are archived. Any other unknown id, a missing migration or a
state other than `applied` stops the command before it executes SQL.

MySQL and MariaDB commit DDL implicitly, so the runner cannot wrap a migration
in a transaction. The runner records `applying` before executing a migration
and `applied` after success. Failures remain in `apply_failed` or
`revert_failed`. Later commands stop on that row.

To recover, inspect the database and finish or undo the failed statement. Mark
the row `applied` only if the up migration is complete; otherwise delete it so
that the runner applies the migration again.

`rollback` returns an error without executing SQL when the latest batch
contains an irreversible migration.

## Manual migrations

Schema diff cannot infer intent. For example, a column rename looks like a
destructive drop followed by an add:

```sql
ALTER TABLE `users`
  DROP COLUMN `name`,
  ADD COLUMN `full_name` VARCHAR(128) NOT NULL;
```

Write the rename in `manual.sql` instead:

```sql title="manual.sql"
-- [sqlgg] manual
-- [sqlgg] id=20260301000000_rename_users_name
ALTER TABLE users RENAME COLUMN name TO full_name;
ALTER TABLE users RENAME COLUMN full_name TO name;
```

Pass this file as `--manual manual.sql` to `sqlgg migrations generate`, or as
`-extends manual.sql` to the lower-level `-migrate` command. The statement with
the id is the up migration; the following untagged statement is its down.
Generated and manual blocks are merged in id order.

For a migration with no safe down statement, mark it as irreversible:

```sql
-- [sqlgg] manual
-- [sqlgg] irreversible=true
-- [sqlgg] id=20260302000000_backfill_users_name
UPDATE users SET full_name = name WHERE full_name = '';
```

An id may be `<timestamp>_<name>` or a bare number such as `20260302`. Manual
ids must fit the 191-byte state-table limit. Names may contain ASCII letters,
digits, underscores and hyphens.

## Lower-level generation

The OCaml workflow above combines generation with the migration runtime.
`-diff` and `-migrate` expose the generator separately for custom runners and
other output formats.

### Generate SQL migrations with `-diff`

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

### Keep a migration log with `-migrate`

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

### Materialize the schema

`-gen sql` with `-base` files (without `-diff`) replays the history and prints the resulting schema as clean DDL. Use this to bootstrap a fresh database or check the current state:

```bash
sqlgg -dialect mysql -gen sql -base initial.sql -base extends.sql -base migrations.sql
```

The output is verified to round-trip: replaying the printed DDL must reproduce the same schema.

## New tables

By default, neither `-migrate` nor `sqlgg migrations generate` turns a new table
into a migration. If the desired schema contains a table that neither the
initial schema nor the migration history creates, generation stops and names
the table. Choose how the table will be created:

### Initialize the database from schema files (recommended)

Use [`CREATE TABLE IF NOT EXISTS`](./sql/ddl.md#keeping-the-schema-in-sync).
Add new tables to both the initial schema and the desired schema, then run the
schema DDL when provisioning the database. Migrations contain only changes to
existing tables.

### Everything is a migration

Pass `--ddl-as-migration` to `sqlgg migrations generate`, or
`-ddl-as-migration` to `-migrate`. A new table becomes a regular migration with
`CREATE TABLE` as the up statement and `DROP TABLE` as the down:

```sql
-- [sqlgg] generated
-- [sqlgg] id=20260201000000_create_orders
CREATE TABLE `orders` (`id` INT NOT NULL, `user_id` INT NOT NULL);
DROP TABLE `orders`;
```

With this strategy `initial.sql` is truly frozen and migrations are the only channel to the database.
