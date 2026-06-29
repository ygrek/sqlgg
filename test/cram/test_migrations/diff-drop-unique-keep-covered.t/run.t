Two single-column UNIQUE indexes cover the same column; one of them is dropped.

This exercises the guard in Tables.index_drop: the column-level `UNIQUE` must be
kept (the remaining index uq_b still enforces it), so dropping uq_a only emits a
plain `DROP INDEX` / `ADD UNIQUE INDEX` and must NOT strip `UNIQUE` from the column.
If the guard were missing, replaying `DROP INDEX uq_a` would drop the column's
uniqueness while uq_b is still present, and the round-trip verification (which
compares column fragments including UNIQUE) would fail instead of producing this.

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_drop_index_uq_a
  ALTER TABLE `users` DROP INDEX `uq_a`;
  ALTER TABLE `users` ADD UNIQUE INDEX `uq_a` (`email`);
