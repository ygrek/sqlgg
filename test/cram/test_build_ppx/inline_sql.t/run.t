Inline SQL with [%sql]: queries live in OCaml source, the schema in .sql files
that sqlgg.ppx loads at compile time (in -schema order: more_schema.sql alters
the table created by schema.sql).

queries.mli pins the inferred types (labelled parameters typed from the schema,
a nullable column as an option, the enum of the second schema file as a
polymorphic variant, a row callback for a multi-row SELECT); two [%sql] share
that enum, and each expansion generates its own module for it. [@@deriving sqlgg]
still expands next to [%sql] in both queries.ml and queries.mli. The build is in
dune's default dev profile, so the expansion is also warning-free:

  $ cp ../../print_impl.ml .
  $ dune build --root . ./main.exe 2>&1

The generated code behaves exactly like the CLI-generated module: same SQL sent,
same parameters bound, same rows decoded. Below, only the SQL the mock driver
receives (generated code drops the indentation of continuation lines, like the
CLI does) and what the queries return:

  $ sent() { awk '/^\[SQL\]/ { sql = 1 } /^\[MOCK/ { sql = 0 } sql || /^[a-z_]+:/' "$@"; }

  $ ./_build/default/main.exe inline > inline.out
  $ ./_build/default/main.exe cli > cli.out
  $ diff inline.out cli.out
  $ sent inline.out
  [SQL] SELECT id, name
  FROM users
  WHERE id = 1
  find_user: 1, name is NULL
  [SQL] SELECT id, name
  FROM users
  WHERE id = 2
  find_user: 2, name Ann
  [SQL] INSERT INTO users (id, name, email) VALUES (3, NULL, 'bob@example.com')
  add_user: 1 row(s)
  [SQL] UPDATE users SET name = 'Robert', nickname = NULL WHERE id = 3
  rename_user: 1 row(s)
  [SQL] SELECT p.title, u.nickname
  FROM posts p
  JOIN users u ON u.id = p.author_id
  WHERE p.author_id = 3 AND p.state = 'published'
  titles_by: Hello by <no nickname>
  titles_by: Again by bob
  [SQL] UPDATE posts SET state = 'published' WHERE id = 7
  publish: 1 row(s)

Compile-time errors point into the SQL inside the OCaml string literal.

An unknown table is reported on the statement:

  $ dune build --root . ./errors/unknown_table.exe 2>&1
  File "errors/unknown_table.ml", line 1, characters 24-47:
  1 | let accounts = [%sql {| SELECT id FROM accounts |}]
                              ^^^^^^^^^^^^^^^^^^^^^^^
  Error: sqlgg: no such table accounts
  [1]

An unknown column is reported on the column itself:

  $ dune build --root . ./errors/unknown_column.exe 2>&1
  File "errors/unknown_column.ml", line 4, characters 17-21:
  4 |       SELECT id, nick
                       ^^^^
  Error: sqlgg: missing attribute : nick
  [1]

The same holds for a plain "..." literal:

  $ dune build --root . ./errors/plain_string.exe 2>&1
  File "errors/plain_string.ml", line 1, characters 25-29:
  1 | let nick = [%sql "SELECT nick FROM users"]
                               ^^^^
  Error: sqlgg: missing attribute : nick
  [1]

but escape sequences break the byte mapping, so the whole literal is reported:

  $ dune build --root . ./errors/escaped_string.exe 2>&1
  File "errors/escaped_string.ml", line 1, characters 18-41:
  1 | let nick = [%sql "SELECT\tnick FROM users"]
                        ^^^^^^^^^^^^^^^^^^^^^^^
  Error: sqlgg: missing attribute : nick
  [1]

A syntax error is reported on the offending token, on its own line of the
literal, after a comment that sqlgg strips before parsing:

  $ dune build --root . ./errors/syntax_error.exe 2>&1
  File "errors/syntax_error.ml", line 6, characters 59-60:
  6 |       FROM users /* a comment shifts nothing */ WHERE id = = @id
                                                                 ^
  Error: sqlgg: syntax error
  [1]

A type error found by sqlgg's inference:

  $ dune build --root . ./errors/type_error.exe 2>&1
  File "errors/type_error.ml", line 1, characters 27-56:
  1 | let clear_email = [%sql {| UPDATE users SET email = NULL |}]
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: sqlgg: Cannot assign nullable value to a non-nullable column 'a -> 'a -> 'a applied to (Text, Any?)
  [1]

DDL belongs to the -schema files, not to [%sql]:

  $ dune build --root . ./errors/ddl_rejected.exe 2>&1
  File "errors/ddl_rejected.ml", line 1, characters 22-49:
  1 | let create = [%sql {| CREATE TABLE audit (id INT) |}]
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: sqlgg: [%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file
  [1]

Exactly one statement per [%sql]:

  $ dune build --root . ./errors/two_statements.exe 2>&1
  File "errors/two_statements.ml", line 1, characters 42-64:
  1 | let both = [%sql {| SELECT id FROM users; SELECT name FROM users |}]
                                                ^^^^^^^^^^^^^^^^^^^^^^
  Error: sqlgg: [%sql] takes exactly one statement, this is another one
  [1]

The SQL must be a string literal, never a runtime value:

  $ dune build --root . ./errors/not_a_literal.exe 2>&1
  File "errors/not_a_literal.ml", line 2, characters 14-25:
  2 | let dynamic = [%sql text]
                    ^^^^^^^^^^^
  Error: sqlgg: [%sql] takes exactly one string literal, e.g. [%sql {| SELECT ... |}]
  [1]

The expansion uses the module T of the enclosing functor (T : Sqlgg_traits.M_io):

  $ dune build --root . ./errors/missing_traits.exe 2>&1
  File "errors/missing_traits.ml", line 1, characters 27-60:
  1 | let outside_of_a_functor = [%sql {| SELECT id FROM users |}]
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Signature mismatch:
         Modules do not match: (module T) is not included in Sqlgg_traits.M_io
         Unbound module T
  [1]

-schema files are replayed in the given order, so the ALTER TABLE in
more_schema.sql fails when it comes first; the error points into that file:

  $ dune build --root . ./errors/schema_order.exe 2>&1
  File "errors/../more_schema.sql", line 1, characters 0-42:
  Error: sqlgg: no such table users
  [1]
