`-open` makes definitions from a file available without generating code for it.

  $ cat > defs.sql <<'EOF'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     name TEXT NOT NULL
  > );
  > -- @get_persons | include: reuse
  > SELECT * FROM person WHERE name LIKE @name;
  > -- @delete_person
  > DELETE FROM person WHERE id = @id;
  > EOF

  $ cat > queries.sql <<'EOF'
  > -- @count_persons
  > SELECT count(*) FROM person;
  > -- @list_persons
  > WITH p AS &get_persons
  > SELECT * FROM p;
  > EOF

  $ sqlgg -no-header -open defs.sql -gen caml queries.sql > out.ml
  $ grep -q 'let count_persons' out.ml && echo generated
  generated
  $ grep -q 'let list_persons' out.ml && echo generated
  generated

no code for anything from the opened file:

  $ grep -E 'get_persons|delete_person|CREATE TABLE' out.ml
  [1]

Nothing is generated for `-open`ed files alone:

  $ sqlgg -no-header -gen caml -open defs.sql

`-open x.sql x.sql` is the same as `x.sql` -- the file is parsed once and its code
is generated once:

  $ cat > all.sql <<'EOF'
  > CREATE TABLE t (
  >     id INT PRIMARY KEY,
  >     v TEXT NOT NULL
  > );
  > -- @get_t
  > SELECT * FROM t WHERE id = @id;
  > EOF

  $ sqlgg -no-header -gen caml all.sql > plain.ml

  $ sqlgg -no-header -gen caml -open all.sql all.sql > open_then_input.ml
  $ diff plain.ml open_then_input.ml

Repeating `-open` for the same file processes it only once.

  $ sqlgg -no-header -gen caml -open all.sql -open all.sql all.sql > double_open.ml
  $ diff plain.ml double_open.ml

The file is recognized under a different spelling of the same path.

  $ sqlgg -no-header -gen caml -open ./all.sql all.sql > spelled_open.ml
  $ diff plain.ml spelled_open.ml

`-open` only makes sense for code generation.

  $ sqlgg -diff -open all.sql -base all.sql -target all.sql
  -open is not supported with -diff/-migrate
  [1]
