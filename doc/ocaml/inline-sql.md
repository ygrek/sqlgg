---
sidebar_position: 4
title: Inline SQL
description: Write a query inside OCaml with [%sql] and have it checked at compile time
---

# Inline SQL with `[%sql]`

`sqlgg.ppx` can check a query written directly in OCaml source against your
schema at compile time, and expand it into the same function the sqlgg OCaml
generator emits for that query. No database is contacted at any point.

## 1. Describe the schema

```sql title="schema.sql"
CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT,
  email TEXT NOT NULL
);
```

## 2. Point the PPX at it

```lisp title="dune"
(library
 (name queries)
 (libraries sqlgg.traits)
 (preprocess
  (pps sqlgg.ppx -- -dialect mysql -schema schema.sql))
 (preprocessor_deps schema.sql))
```

`(preprocessor_deps schema.sql)` makes dune rebuild the library when the schema
changes. `-schema %{dep:schema.sql}` does the same in one place.

## 3. Write the query

```ocaml title="queries.ml"
module Make (T : Sqlgg_traits.M_io) = struct
  let find_user =
    [%sql {|
      SELECT id, name
      FROM users
      WHERE id = @id
    |}]

  let add_user =
    [%sql {| INSERT INTO users (id, name, email) VALUES (@id, @name, @email) |}]
end
```

The enclosing `let` names the OCaml function. A SQL `-- @name` annotation is
optional here; when present, it only sets the query name seen by the driver and
in logs.

The expansion uses the module `T` in scope, so `[%sql]` goes inside a functor
over `Sqlgg_traits.M_io`, just like the module the CLI generates.

## 4. Call it

Apply the functor to your driver's traits (see [Traits](traits.md), e.g. a
module built with `Sqlgg_mariadb.Default`):

```ocaml
module Db = Queries.Make (Mariadb_traits)

let show db =
  match Db.find_user db ~id:1L with
  | Some (id, Some name) -> Printf.printf "%Ld: %s\n" id name
  | Some (id, None) -> Printf.printf "%Ld has no name\n" id
  | None -> print_endline "no such user"
```

The types come from the schema: `id` is `INT PRIMARY KEY`, so the query returns
at most one row (`option`), and `name` is nullable (`string option`):

```ocaml
val find_user :
  [> `RO ] T.connection -> id:T.Types.Int.t ->
  (T.Types.Int.t * T.Types.Text.t option) option T.IO.future
```

A mistake in the SQL is a compile error that points inside the literal:

```text
File "queries.ml", line 6, characters 17-21:
6 |       SELECT id, nick
                     ^^^^
Error: sqlgg: missing attribute : nick
```

## Reference

`[%sql]` takes exactly one string literal holding exactly one `SELECT`,
`INSERT`, `UPDATE` or `DELETE`, and expands to the function the CLI generates
for it at the top of the generated module. The SQL is the same as in a `.sql`
file: `@param` parameters, `-- @name` annotations and comments all work. DDL
belongs in the schema files. The `subst`, `noparse`, `include` and
`dynamic_select` properties are rejected; use a `.sql` file and the CLI for
those.

PPX flags, passed after `--` in `pps`:

- `-schema <file.sql>`: DDL to load before checking queries. Repeat it for
  several files; they are replayed in the given order. Relative paths are
  resolved against the directory of the preprocessed file.
- `-dialect <mysql|tidb|sqlite|postgresql>`: as `sqlgg -dialect` (default
  `mysql`).
- `-params <named|unnamed|oracle|postgresql|none>`: as `sqlgg -params`
  (default: from `-dialect`, or `none` without it, as for the CLI).

Every `[%sql]` starts from the schema exactly as loaded, so one query never sees
the effects of another.
