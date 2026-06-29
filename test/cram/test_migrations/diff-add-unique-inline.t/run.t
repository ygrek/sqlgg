Adding an inline column-level `UNIQUE` (no named index) must behave exactly like
adding a single-column `UNIQUE KEY` (see diff-add-unique-single.t): on a real
server the inline `UNIQUE` is backed by an implicit single-column unique index,
so the delta must be emitted once as `ADD UNIQUE INDEX`, never as a
`CHANGE COLUMN ... UNIQUE` whose `down` (a plain `CHANGE COLUMN`) cannot drop the
index the server silently created.

The synthetic index name follows the MySQL default (the column name).

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_add_unique_email
  ALTER TABLE `users` ADD UNIQUE INDEX `email` (`email`);
  ALTER TABLE `users` DROP INDEX `email`;
