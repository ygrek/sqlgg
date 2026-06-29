Full automatic generation cycle through the declarative -migrate tool, organised
the same way a real project would: a frozen initial.sql baseline, one hand-written
manual block in extends.sql, and a target.sql that a human edits over several
iterations. Each run replays initial + every recorded block (merged by id), diffs
against the current target, verifies the new up/down round-trips, and appends only
the genuinely new delta to migrations.sql.

This cycle exercises a PRIMARY KEY appearing mid-stream and a column type change on
a PK column (id INT -> BIGINT): the key must survive the diff/replay round-trip.

Frozen baseline:

  $ cat initial.sql
  CREATE TABLE users (id INT NOT NULL);
  CREATE TABLE posts (id INT NOT NULL);

One hand-written manual migration lives in extends.sql, pinned by hand to just a
day (20251231 - a human need not spell out hours/minutes/seconds; it is compared
as the start of that day, i.e. before the generated 20260101* stamps):

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20251231
  ALTER TABLE users ADD COLUMN bio TEXT;
  ALTER TABLE users DROP COLUMN bio;

Start from an empty, writable migrations log:

  $ : > migrations.sql

Iteration 1 - the human adds a column to each table:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT, bio TEXT);
  > CREATE TABLE posts (id INT NOT NULL, title TEXT);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20260101000000_alter_posts_add_col_title, 20260101000000_alter_users_add_col_name)

Iteration 2 - a PRIMARY KEY is introduced on users.id, plus more columns:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT, bio TEXT, email VARCHAR(255), PRIMARY KEY (id));
  > CREATE TABLE posts (id INT NOT NULL, title TEXT, body TEXT);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20260101000001_alter_posts_add_col_body, 20260101000001_alter_users_add_col_email_add_pk)

Iteration 3 - users.id is widened INT -> BIGINT (still the PK) and email gets a
unique index. The PK must persist across the CHANGE COLUMN, otherwise the up/down
would not round-trip and verification would fail. email is VARCHAR (not TEXT) so
the UNIQUE index needs no key length:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id BIGINT NOT NULL, name TEXT, bio TEXT, email VARCHAR(255), PRIMARY KEY (id), UNIQUE KEY email_idx (email));
  > CREATE TABLE posts (id INT NOT NULL, title TEXT, body TEXT);
  > EOF
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260101000002_alter_users_change_col_id_add_unique_email_idx)

The complete generated log (the manual day-pinned block stays in extends.sql):

  $ diff expected.sql migrations.sql

Materialise the final schema by replaying initial + extends + migrations: the
PRIMARY KEY and the widened BIGINT id are both present. current.sql is the golden
snapshot (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base extends.sql -base migrations.sql | diff current.sql -

Re-running against the same target is a clean no-op - the loop is closed:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 6 recorded migration(s)
