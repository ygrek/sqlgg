Uses of a relation slot that cannot be supported whatever relation is chosen:

  $ cat > ddl.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > CREATE TABLE logs (id INT NOT NULL, msg TEXT);
  > SQL
  $ check() { printf '%s\n' "$1" > q.sql; sqlgg -no-header -gen none ddl.sql -gen caml q.sql 2>&1 >/dev/null | grep -E '^At|exception' | sed 's/^Fatal error: exception //'; }

  $ check 'SELECT COUNT(1) FROM @s;'
  At : @s
  relation slot @s requires an alias: FROM @s AS alias
  $ check 'SELECT * FROM @s AS s;'
  At : *
  SELECT * cannot expand a relation slot, list the columns explicitly
  $ check 'SELECT s.* FROM @s AS s;'
  At : s.*
  s.* cannot expand a relation slot, list the columns explicitly
  $ check 'SELECT 1 FROM users u NATURAL JOIN @s AS s;'
  At : NATURAL JOIN @s AS s
  NATURAL JOIN and JOIN ... USING are not supported with relation slots, use ON
  $ check 'SELECT 1 FROM users u JOIN @s AS s USING (id);'
  At :  JOIN @s AS s USING (id)
  NATURAL JOIN and JOIN ... USING are not supported with relation slots, use ON
  $ check 'WITH RECURSIVE r AS (SELECT s.id FROM @s AS s) SELECT id FROM r;'
  At : @s
  relation slots are not supported inside recursive CTEs
  $ check "UPDATE users u JOIN @s AS s ON s.id = u.id SET u.name = 'x';"
  At : @s
  relation slots are supported only in SELECT statements
  $ check 'SELECT name FROM @s AS s;'
  At : name
  column name of a relation slot must be qualified with the slot alias
  $ check 'SELECT s.id FROM @s AS s WHERE s.id = @s;'
  At : @s
  parameter s is used both as a relation slot and as a value
  $ check 'SELECT s.id FROM @s AS s (a);'
  At : s
  column aliases are not supported for relation slots

When no relation fits, the reasons are listed in declaration order:

  $ check 'SELECT s.nope FROM @s AS s;'
  At : @s
  Failure("no relation is compatible with relation slot @s\n  users: missing attribute : nope\n  logs: missing attribute : nope")

Types that depend on the relation chosen can only be result columns and named parameters:

  $ printf 'CREATE TABLE tags (id TEXT NOT NULL, name TEXT);\n' >> ddl.sql
  $ check 'SELECT COUNT(1) FROM @s AS s WHERE s.id IN @ids;'
  At : @ids
  the type of the values of this list depends on a relation slot, not supported yet
  $ check 'SELECT COUNT(1) FROM @s AS s WHERE s.id = ?;'
  At : ?
  name this parameter, its type depends on a relation slot

A statement with several slots has to type-check while they are all open:

  $ check 'SELECT COUNT(1) FROM (SELECT a.id FROM @a AS a WHERE (a.id, a.name) IN @pairs) t WHERE EXISTS (SELECT 1 FROM @b AS b WHERE b.id = t.id);'
  At : @a
  Failure("with several relation slots the statement has to type-check before any relation is chosen: If you need to have a field as parameter in the left part you should specify a type")

Only the OCaml generators support relation slots:

  $ printf 'SELECT s.id FROM @s AS s;\n' > q.sql
  $ for g in xml cxx java csharp; do sqlgg -no-header -gen none ddl.sql -gen $g q.sql 2>&1 >/dev/null | grep exception | sed 's/^Fatal error: exception //'; done
  Failure("relation slot @s is supported only by the OCaml generators, not by xml")
  Failure("relation slot @s is supported only by the OCaml generators, not by cxx")
  Failure("relation slot @s is supported only by the OCaml generators, not by java")
  Failure("relation slot @s is supported only by the OCaml generators, not by csharp")

Relation names that are not plain identifiers are quoted for the dialect, quotes inside them are doubled:

  $ printf 'CREATE TABLE `odd``name` (id INT NOT NULL);\nCREATE TABLE `order` (id INT NOT NULL);\n' > quoted.sql
  $ sqlgg -no-header -gen none quoted.sql -gen caml q.sql | grep -o 'Table_[a-z_]* -> Sqlgg_template.*' | sort -u
  Table_odd_name -> Sqlgg_template.text "`odd``name`"
  Table_order -> Sqlgg_template.text "`order`"
  $ sqlgg -no-header -dialect postgresql -gen none quoted.sql -gen caml q.sql | grep -o 'Table_[a-z_]* -> Sqlgg_template.*' | sort -u
  Table_odd_name -> Sqlgg_template.text "\"odd`name\""
  Table_order -> Sqlgg_template.text "\"order\""
