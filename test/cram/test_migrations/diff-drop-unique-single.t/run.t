The reverse of diff-add-unique-single.t: dropping a single-column UNIQUE index.

It must emit a plain `DROP INDEX` (up) and `ADD UNIQUE INDEX` (down), with NO
spurious `CHANGE COLUMN` to strip/restore the column-level `UNIQUE` that
Tables.mark_unique_columns attaches. Dropping the index already removes that
uniqueness (Tables.index_drop), so the round-trip verification round-trips cleanly.

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_drop_index_email_idx
  ALTER TABLE `users` DROP INDEX `email_idx`;
  ALTER TABLE `users` ADD UNIQUE INDEX `email_idx` (`email`);
