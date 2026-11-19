Migration ids are <timestamp>_<name>. Siblings of one run share the timestamp, so
the name is all that tells them apart, and it is stored in a finite name column.
`-max-migration-id-length` caps the id by building the name up to the cap instead
of letting the database chop it.

Two tables with a shared prefix, four columns added to each. Without the flag
an id may take the 191 bytes of the state table's id column, enough for these:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -base initial.sql -target target.sql | grep -o 'id=.*'
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column_add_col_second_additional_column_add_col_third_additional_column_add_col_fourth_additional_column
  id=20260101120000_alter_some_extremely_long_table_specification_add_col_first_additional_column_add_col_second_additional_column_add_col_third_additional_column_add_col_fourth_additional_column

Tightening the cap, one of them shrinks step by step. Actions drop off the tail
whole; the one that no longer fits goes in as its bare verb `add_col`; then the
table name gives up whole words. Nothing is ever cut mid-word:

  $ for n in 160 150 125 90 80 58; do sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length $n -base initial.sql -target target.sql | grep -o 'id=.*' | head -1; done
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column_add_col_second_additional_column_add_col_third_additional_column
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column_add_col_second_additional_column_add_col
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column_add_col_second_additional_column
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col
  id=20260101120000_alter_some_extremely_long_table_identifier

Cut past the word that differs and the two names collide. Nothing is appended to
force them apart, so the batch is refused rather than recorded:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 50 -base initial.sql -target target.sql
  two migrations get the same id "20260101120000_alter_some_extremely_long_table". increase -max-migration-id-length
  [1]

Below the timestamp width no name is left at all, caught by the same check:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 15 -base initial.sql -target target.sql
  two migrations get the same id "20260101120000". increase -max-migration-id-length
  [1]

Shortening is deterministic, so -migrate stays idempotent:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260101120000 -max-migration-id-length 80 -initial initial.sql -migrations-file migrations.sql -target target.sql > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20260101120000_alter_some_extremely_long_table_identifier_add_col, 20260101120000_alter_some_extremely_long_table_specification_add_col)

  $ grep -o 'id=.*' migrations.sql
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col
  id=20260101120000_alter_some_extremely_long_table_specification_add_col

Generated code names follow the id:

  $ grep -o 'let apply_[a-z0-9_]*' migrations.ml
  let apply_20260101120000_alter_some_extremely_long_table_identifier_add_col
  let apply_20260101120000_alter_some_extremely_long_table_specification_add_col

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260102120000 -max-migration-id-length 80 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)

Recorded ids are never rewritten, so changing the cap later leaves applied
migrations untouched:

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260102120000 -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)

The cap is there without the flag too. A fifth column would make an id of 219
bytes, so its action is left out:

  $ cat > one.sql <<'EOF'
  > CREATE TABLE some_extremely_long_table_identifier (id INT NOT NULL PRIMARY KEY);
  > EOF
  $ cat > wider.sql <<'EOF'
  > CREATE TABLE some_extremely_long_table_identifier (
  >   id INT NOT NULL PRIMARY KEY,
  >   first_additional_column INT NOT NULL,
  >   second_additional_column INT NOT NULL,
  >   third_additional_column INT NOT NULL,
  >   fourth_additional_column INT NOT NULL,
  >   fifth_additional_column INT NOT NULL
  > );
  > EOF
  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -base one.sql -target wider.sql | grep -o 'id=.*'
  id=20260101120000_alter_some_extremely_long_table_identifier_add_col_first_additional_column_add_col_second_additional_column_add_col_third_additional_column_add_col_fourth_additional_column

The flag can only tighten the cap:

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 192 -base initial.sql -target target.sql
  -max-migration-id-length 192 is above 191, the width of the migration id column
  [1]
  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -max-migration-id-length 13 -base initial.sql -target target.sql
  -max-migration-id-length 13 cannot hold migration ordinal 20260101120000
  [1]

A hand-written id is held to the same width, and nothing is recorded when it
does not fit:

  $ printf -- '-- [sqlgg] id=20251230_%0200d\nALTER TABLE some_extremely_long_table_identifier ADD COLUMN x INT;\nALTER TABLE some_extremely_long_table_identifier DROP COLUMN x;\n' 0 > long.sql
  $ : > fresh.sql
  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -now 20260101120000 -initial initial.sql -migrations-file fresh.sql -extends long.sql -target target.sql > /dev/null
  migration id is too long: 209 bytes, the limit is 191
  [1]
  $ cat fresh.sql
