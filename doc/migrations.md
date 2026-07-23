---
sidebar_position: 3
title: Migrations
description: Generate schema migrations from schema diffs
---

# Migrations

You keep the schema as plain [`CREATE TABLE`](./sql/ddl.md) statements and edit it as the project changes. sqlgg compares the old version with the new one and writes the up and down `ALTER TABLE` statements that get you from one to the other, covering columns, types and indices.

- **`-diff`** computes the delta between two schemas and prints it.
- **`-migrate`** appends new changes to a migrations file and regenerates the migration module from the recorded history.

`-gen xml` writes the migrations out for another toolchain to pick up:

```bash
sqlgg -diff -gen xml -base initial.sql -target target.sql
```

```xml
<migrations>
 <migration name="20260101000000_alter_users_add_col_age" apply="ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL" revert="ALTER TABLE `users` DROP COLUMN `age`"/>
</migrations>
```

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
