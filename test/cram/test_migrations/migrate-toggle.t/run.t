-migrate is stateful and append-only: every run diffs the new target against the
CURRENT materialized schema (initial + all migrations so far), then appends the
delta. So toggling a column back and forth does NOT collapse - each step becomes
its own migration. (A bare `-diff` of two snapshots would instead show only the
net difference; see case 01.)

  $ : > migrations.sql

Add `age`:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE `users` (`id` INT NOT NULL, `name` TEXT, `age` INT NOT NULL);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name m -initial initial.sql -migrations-file migrations.sql -target target.sql > m.ml
  appended 1 migration(s) to migrations.sql (id 20260101000000_alter_users_add_col_age)

Drop `age` again:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE `users` (`id` INT NOT NULL, `name` TEXT);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name m -initial initial.sql -migrations-file migrations.sql -target target.sql > m.ml
  appended 1 migration(s) to migrations.sql (id 20260101000001_alter_users_drop_col_age)

Add `age` once more:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE `users` (`id` INT NOT NULL, `name` TEXT, `age` INT NOT NULL);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name m -initial initial.sql -migrations-file migrations.sql -target target.sql > m.ml
  appended 1 migration(s) to migrations.sql (id 20260101000002_alter_users_add_col_age)

Three distinct migrations accumulated - ADD, DROP, ADD - each with its inverse:

  $ diff expected.sql migrations.sql
