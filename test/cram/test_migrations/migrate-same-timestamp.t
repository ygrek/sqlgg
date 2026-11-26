Migrations that share a timestamp keep the order in which the migration files
declare them: the generated log first, then the manual file. The name after the
timestamp takes no part in the order. Here the parent table has to stay ahead
of the child that references it, although `create_job_items` sorts before
`create_jobs`:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE jobs (id INT NOT NULL PRIMARY KEY);
  > CREATE TABLE job_items (
  >   id INT NOT NULL PRIMARY KEY,
  >   job_id INT NOT NULL,
  >   CONSTRAINT job_items_job_id_fk FOREIGN KEY (job_id) REFERENCES jobs (id)
  > );
  > SQL
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=20260710000000_create_jobs
  > CREATE TABLE jobs (id INT NOT NULL PRIMARY KEY);
  > DROP TABLE jobs;
  > 
  > -- [sqlgg] manual
  > -- [sqlgg] id=20260710000000_create_job_items
  > CREATE TABLE job_items (
  >   id INT NOT NULL PRIMARY KEY,
  >   job_id INT NOT NULL,
  >   CONSTRAINT job_items_job_id_fk FOREIGN KEY (job_id) REFERENCES jobs (id)
  > );
  > DROP TABLE job_items;
  > SQL

`-migrate` lists them in that order:

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -extends manual.sql -target schema.sql > function_list.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ sed -n '/let migrations = \[/,/\]/p' function_list.ml
    let migrations = [
      ("20260710000000_create_jobs", apply_20260710000000_create_jobs, revert_20260710000000_create_jobs);
      ("20260710000000_create_job_items", apply_20260710000000_create_job_items, revert_20260710000000_create_job_items);
    ]

So does the typed plan of `sqlgg migrations generate`:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --manual manual.sql --module Migrations > typed_plan.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ grep -o 'id:"[^"]*"' typed_plan.ml
  id:"20260710000000_create_jobs"
  id:"20260710000000_create_job_items"

`--baseline` archives every migration of its timestamp, so none of them can
stay in the log:

  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --manual manual.sql --baseline 20260710000000_create_job_items --module Migrations > /dev/null
  migration 20260710000000_create_jobs shares its timestamp with --baseline 20260710000000_create_job_items, which archives every migration of that timestamp
  [1]

A block of the generated log comes before a manual block of the same timestamp.
The manual `add_default_job` needs the column that the generated
`alter_jobs_add_col_title` adds:

  $ mkdir mixed && cd mixed
  $ cat > initial.sql <<'SQL'
  > CREATE TABLE jobs (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE jobs (id INT NOT NULL PRIMARY KEY, title TEXT);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations --now 20260711000000 > /dev/null
  appended 1 migration(s) to migrations.sql (id 20260711000000_alter_jobs_add_col_title)
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=20260711000000_add_default_job
  > INSERT INTO jobs (id, title) VALUES (1, 'default');
  > DELETE FROM jobs WHERE id = 1;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --manual manual.sql --module Migrations > typed_plan.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ grep -o 'id:"[^"]*"' typed_plan.ml
  id:"20260711000000_alter_jobs_add_col_title"
  id:"20260711000000_add_default_job"

`--ddl-as-migration` gives every new table of one run the same timestamp. A
table is recorded after the tables its foreign keys reference, and reading the
log back keeps that order:

  $ mkdir ../created && cd ../created
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema ../schema.sql --migrations migrations.sql --module Migrations --now 20260712000000 --ddl-as-migration > typed_plan.ml
  appended 2 migration(s) to migrations.sql (id 20260712000000_create_jobs, 20260712000000_create_job_items)
  $ grep 'id=' migrations.sql
  -- [sqlgg] id=20260712000000_create_jobs
  -- [sqlgg] id=20260712000000_create_job_items
  $ grep -o 'id:"[^"]*"' typed_plan.ml
  id:"20260712000000_create_jobs"
  id:"20260712000000_create_job_items"
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema ../schema.sql --migrations migrations.sql --module Migrations > again.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)
  $ cmp typed_plan.ml again.ml

Dropped tables go the other way round, the referencing table first. The order
follows the foreign keys rather than the schema file, which here lists the
tables alphabetically:

  $ mkdir ../dropped && cd ../dropped
  $ cat > initial.sql <<'SQL'
  > CREATE TABLE job_items (
  >   id INT NOT NULL PRIMARY KEY,
  >   job_id INT NOT NULL,
  >   CONSTRAINT job_items_job_id_fk FOREIGN KEY (job_id) REFERENCES jobs (id)
  > );
  > CREATE TABLE jobs (id INT NOT NULL PRIMARY KEY);
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY);
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --initial initial.sql --migrations migrations.sql --module Migrations --now 20260713000000 > typed_plan.ml
  appended 2 migration(s) to migrations.sql (id 20260713000000_drop_job_items, 20260713000000_drop_jobs)
  $ grep 'id=' migrations.sql
  -- [sqlgg] id=20260713000000_drop_job_items
  -- [sqlgg] id=20260713000000_drop_jobs
  $ grep -o 'id:"[^"]*"' typed_plan.ml
  id:"20260713000000_drop_job_items"
  id:"20260713000000_drop_jobs"
