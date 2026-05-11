Migration ids have "memory": uniqueness and monotonicity do NOT come from the
clock, they come from the migrations file itself. Before generating, -migrate
reads the existing ids and picks max(now, max_existing_id + 1). So -now is only a
way to pin the clock for reproducible output (e.g. tests) - even if two runs use
the SAME -now, the second one still bumps to the next free id instead of clashing.

Start from an empty log:

  $ : > migrations.sql

Run 1 - add `age`, clock pinned to 20260101120000:

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, age INT NOT NULL);
  > EOF

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name mig -now 20260101120000 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  appended 1 migration(s) to migrations.sql (id 20260101120000_alter_users_add_col_age)

Run 2 - add `city`, the SAME pinned clock 20260101120000. The id does not repeat:
because 20260101120000 is already taken, next_base falls back to max_id + 1.

  $ cat > target.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, age INT NOT NULL, city INT NOT NULL);
  > EOF

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name mig -now 20260101120000 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  appended 1 migration(s) to migrations.sql (id 20260101120001_alter_users_add_col_city)

Run 3 - target unchanged: nothing to append (idempotent, the file is the state):

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name mig -now 20260101120000 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)

The recorded ids are strictly increasing despite the frozen clock:

  $ grep '^-- \[sqlgg\] id=' migrations.sql
  -- [sqlgg] id=20260101120000_alter_users_add_col_age
  -- [sqlgg] id=20260101120001_alter_users_add_col_city
