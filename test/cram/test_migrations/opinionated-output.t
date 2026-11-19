`sqlgg migrations generate --output FILE` writes the module to a file instead
of printing it.

  $ G="sqlgg migrations generate --dialect mysql --lang ocaml --module Migrations --ddl-as-migration"
  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY
  > );
  > SQL

The file gets exactly what standard output would get, the log is recorded the
same way, and nothing is printed to standard output:

  $ mkdir printed written
  $ $G --schema schema.sql --migrations printed/migrations.sql --now 20261112000000 > printed/migrations.ml
  appended 1 migration(s) to printed/migrations.sql (id 20261112000000_create_users)
  $ $G --schema schema.sql --migrations written/migrations.sql --now 20261112000000 --output written/migrations.ml > written/stdout.txt
  appended 1 migration(s) to written/migrations.sql (id 20261112000000_create_users)
  $ test ! -s written/stdout.txt
  $ cmp printed/migrations.ml written/migrations.ml
  $ cmp printed/migrations.sql written/migrations.sql

Running it again writes the same module:

  $ cd written
  $ cp migrations.ml first.ml
  $ $G --schema ../schema.sql --migrations migrations.sql --output migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ cmp first.ml migrations.ml

Previous content is replaced as a whole, also when it was longer:

  $ cat first.ml first.ml > migrations.ml
  $ $G --schema ../schema.sql --migrations migrations.sql --output migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ cmp first.ml migrations.ml

The permission bits of an existing file are kept:

  $ chmod 600 migrations.ml
  $ $G --schema ../schema.sql --migrations migrations.sql --output migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ ls -l migrations.ml | cut -c 1-10
  -rw-------
  $ chmod 644 migrations.ml

A symbolic link is written through, not replaced:

  $ printf 'stale\n' > migrations.ml
  $ ln -s migrations.ml link.ml
  $ $G --schema ../schema.sql --migrations migrations.sql --output link.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ test -L link.ml
  $ cmp first.ml migrations.ml

A schema change reaches both the log and the module:

  $ cat > ../schema.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY,
  >   email TEXT
  > );
  > SQL
  $ $G --schema ../schema.sql --migrations migrations.sql --now 20261113000000 --output migrations.ml
  appended 1 migration(s) to migrations.sql (id 20261113000000_alter_users_add_col_email)
  $ grep -c '^-- \[sqlgg\] id=' migrations.sql
  2
  $ grep -o 'parse_generated ~id:"[^"]*"' migrations.ml
  parse_generated ~id:"20261112000000_create_users"
  parse_generated ~id:"20261113000000_alter_users_add_col_email"

A generation error leaves the file and the log as they were. Here a
hand-written block has no id:

  $ cat > no_id.sql <<'SQL'
  > -- [sqlgg] manual
  > ALTER TABLE users ADD COLUMN age INT;
  > ALTER TABLE users DROP COLUMN age;
  > SQL
  $ cat > with_age.sql <<'SQL'
  > CREATE TABLE users (
  >   id INT NOT NULL PRIMARY KEY,
  >   email TEXT,
  >   age INT
  > );
  > SQL
  $ cp migrations.ml second.ml
  $ cp migrations.sql second.sql
  $ $G --schema with_age.sql --migrations migrations.sql --manual no_id.sql --output migrations.ml
  migration has no id: add -- [sqlgg] id=<timestamp>_<name>
  [1]
  $ cmp second.ml migrations.ml
  $ cmp second.sql migrations.sql

The same error does not create a file that was not there:

  $ $G --schema with_age.sql --migrations migrations.sql --manual no_id.sql --output absent.ml
  migration has no id: add -- [sqlgg] id=<timestamp>_<name>
  [1]
  $ test ! -e absent.ml

A path that cannot be written is an error. The migrations are in the log by
then, and the next run writes the module from it:

  $ cd ..
  $ mkdir unwritable
  $ $G --schema schema.sql --migrations unwritable/migrations.sql --now 20261112000000 --output unwritable/missing/migrations.ml
  appended 1 migration(s) to unwritable/migrations.sql (id 20261112000000_create_users)
  cannot write unwritable/missing/migrations.ml: unwritable/missing/migrations.ml: No such file or directory
  [2]
  $ test ! -e unwritable/missing
  $ $G --schema schema.sql --migrations unwritable/migrations.sql --output unwritable/migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ $G --schema schema.sql --migrations unwritable/migrations.sql 2> /dev/null | cmp - unwritable/migrations.ml

A directory is not a file to write to:

  $ mkdir unwritable/directory.ml
  $ $G --schema schema.sql --migrations unwritable/migrations.sql --output unwritable/directory.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  cannot write unwritable/directory.ml: unwritable/directory.ml: Is a directory
  [2]

A read-only file is refused and keeps its content. Root may write to any file,
so root skips this case:

  $ printf 'keep me\n' > readonly.ml
  $ chmod 444 readonly.ml
  $ refused() {
  >   $G --schema schema.sql --migrations unwritable/migrations.sql --output readonly.ml 2> refusal.txt
  >   test $? -eq 2 &&
  >   grep -q -x 'cannot write readonly.ml: readonly.ml: Permission denied' refusal.txt &&
  >   test "$(cat readonly.ml)" = 'keep me'
  > }
  $ test "$(id -u)" -eq 0 || refused

A file in a read-only directory is still written, because it is written in
place:

  $ mkdir locked
  $ printf 'stale\n' > locked/migrations.ml
  $ chmod 555 locked
  $ $G --schema schema.sql --migrations unwritable/migrations.sql --output locked/migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)
  $ cmp unwritable/migrations.ml locked/migrations.ml
  $ chmod 755 locked
