The reverse of diff-add-unique-inline.t: dropping an inline column-level `UNIQUE`.

It must emit a plain `DROP INDEX` (up) / `ADD UNIQUE INDEX` (down) on the implicit
single-column unique index, with NO spurious `CHANGE COLUMN` to strip/restore the
column-level `UNIQUE`. The synthetic index name follows the MySQL default (the
column name).

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_drop_index_email
  ALTER TABLE `users` DROP INDEX `email`;
  ALTER TABLE `users` ADD UNIQUE INDEX `email` (`email`);
