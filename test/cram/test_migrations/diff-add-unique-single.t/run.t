Adding a single-column UNIQUE index to a pre-existing column must emit the
uniqueness exactly once, as `ADD UNIQUE INDEX`. It must NOT also re-emit it as an
inline `UNIQUE` in a `CHANGE COLUMN`: on a real MySQL server the inline `UNIQUE`
creates a second, auto-named unique index, so the table ends up with two unique
indexes on `email`, and the generated `down` (which only drops `email_idx`) cannot
restore the original schema.

The CREATE-table path already deduplicates this (see diff-create-unique-index.t);
the ALTER/diff path must behave the same way.

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_add_unique_email_idx
  ALTER TABLE `users` ADD UNIQUE INDEX `email_idx` (`email`);
  ALTER TABLE `users` DROP INDEX `email_idx`;
