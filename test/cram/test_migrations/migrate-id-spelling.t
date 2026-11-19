A hand-written id is the id exactly as written, leading zeros included: that is
the text the runner stores and looks up.

  $ cat > initial.sql <<'EOF'
  > CREATE TABLE t (id INT NOT NULL PRIMARY KEY);
  > EOF
  $ cat > schema.sql <<'EOF'
  > CREATE TABLE t (id INT NOT NULL PRIMARY KEY, x INT);
  > EOF
  $ cat > manual.sql <<'EOF'
  > -- [sqlgg] manual
  > -- [sqlgg] id=0001_init
  > ALTER TABLE t ADD COLUMN x INT;
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  $ : > migrations.sql
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --initial initial.sql --manual manual.sql --module Migrations 2> /dev/null | grep -o 'parse_generated ~id:"[^"]*"'
  parse_generated ~id:"0001_init"

Generated names preserve the stored id:

  $ sqlgg -no-header -dialect mysql -migrate -gen caml -name migrations -initial initial.sql -extends manual.sql -target schema.sql 2> /dev/null | grep -o '("[^"]*", apply_[a-z0-9_]*'
  ("0001_init", apply_0001_init

Two ids that differ only in leading zeros are distinct:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE t (id INT NOT NULL PRIMARY KEY, x INT, y INT);
  > SQL
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=0001_init
  > ALTER TABLE t ADD COLUMN x INT;
  > ALTER TABLE t DROP COLUMN x;
  > 
  > -- [sqlgg] manual
  > -- [sqlgg] id=1_init
  > ALTER TABLE t ADD COLUMN y INT;
  > ALTER TABLE t DROP COLUMN y;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --initial initial.sql --manual manual.sql --module Migrations 2> /dev/null | grep -o 'parse_generated ~id:"[^"]*"'
  parse_generated ~id:"0001_init"
  parse_generated ~id:"1_init"

A repeated exact id is rejected:

  $ sed 's/id=0001_init/id=1_init/' manual.sql > duplicate.sql
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --initial initial.sql --manual duplicate.sql --module Migrations > /dev/null
  duplicate migration id "1_init"
  [1]

Characters allowed in a stored id are made safe in the generated OCaml name:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE t (id INT NOT NULL PRIMARY KEY, x INT);
  > SQL
  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=2_add-email
  > ALTER TABLE t ADD COLUMN x INT;
  > ALTER TABLE t DROP COLUMN x;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --initial initial.sql --manual manual.sql --module Migrations 2> /dev/null | grep 'let apply_'
    let apply_2_add_email db  =

A trailing separator is not an empty name, it is not an id at all:

  $ cat > manual.sql <<'SQL'
  > -- [sqlgg] manual
  > -- [sqlgg] id=7_
  > ALTER TABLE t ADD COLUMN x INT;
  > ALTER TABLE t DROP COLUMN x;
  > SQL
  $ sqlgg migrations generate --dialect mysql --lang ocaml --schema schema.sql --migrations migrations.sql --initial initial.sql --manual manual.sql --module Migrations > /dev/null
  bad migration block id "7_" (expected integer or <timestamp>_<name>)
  [1]
