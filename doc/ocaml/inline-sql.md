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
module Make (Database : Sqlgg_traits.M_io) = struct
  let find_user =
    [%sql.Database {|
      SELECT id, name
      FROM users
      WHERE id = @id
    |}]

  let add_user =
    [%sql.Database
      {| INSERT INTO users (id, name, email) VALUES (@id, @name, @email) |}]
end
```

The enclosing `let` names the OCaml function. A SQL `-- @name` annotation is
optional here; when present, it only sets the query name seen by the driver and
in logs.

The extension path names the traits module. Omitting it defaults to a module
named `T`, so `[%sql {| ... |}]` remains available as a shorthand.

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
  [> `RO ] Database.connection -> id:Database.Types.Int.t ->
  (Database.Types.Int.t * Database.Types.Text.t option) option
    Database.IO.future
```

A mistake in the SQL is a compile error that points inside the literal:

```text
File "queries.ml", line 6, characters 17-21:
6 |       SELECT id, nick
                     ^^^^
Error: sqlgg: missing attribute : nick
```

## Requests that do not depend on a driver

`[%sql.Request]` expands to a value that describes the query instead of a
function that runs it. No traits module is needed where it is written:

```ocaml title="requests.ml"
let find_user = [%sql.Request {| SELECT id, name FROM users WHERE id = @id |}]

let rename_user =
  [%sql.Request {| UPDATE users SET name = @name WHERE id = @id |}]

let count_users = [%sql.Request {| SELECT COUNT(*) FROM users |}]
```

```ocaml
val find_user :
  id:'t Sqlgg_request.int_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val rename_user :
  name:'t Sqlgg_request.text_of option -> id:'t Sqlgg_request.int_of ->
  (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t

val count_users : ('t Sqlgg_request.int_of, 't, Sqlgg_request.exactly_one) Sqlgg_request.t
```

Parameters are labelled arguments, as in generated modules; applying them
gives the request. Its type arguments are the row, the driver's types and how
many rows come back. A row is `unit` for no columns, the bare value for one, a
tuple for several. A nullable column is an `option` and an `ENUM` is a closed
polymorphic variant (`[ `Draft | `Published ]` for `ENUM('draft', 'published')`).

The host types are not fixed where the request is written: `'t` stands for the
record of a driver's `Types`, and `'t Sqlgg_request.int_of` is its `Int.t`,
`'t Sqlgg_request.text_of` its `Text.t`, and so on for `bool_of`, `float_of`,
`decimal_of`, `datetime_of`, `json_of`, `json_path_of` and `any_of` (a column or
parameter whose type sqlgg cannot infer, such as `NULL AS x`). `Sqlgg_request.Make`
fixes `'t` to the driver it is applied to, so the same `find_user` returns
`int64 * string option` under a driver with `Int.t = int64` and `Text.t = string`,
and `int * Bytes.t option` under one with `Int.t = int` and `Text.t = Bytes.t`, with
the driver's own `DATETIME` representation in both cases. `BLOB` is `string` and
`BIGINT UNSIGNED` is `Unsigned.UInt64.t`, as the traits fix them.

Run a request by applying `Sqlgg_request.Make` to any traits module:

```ocaml
module Run = Sqlgg_request.Make (Mariadb_traits)

let show db =
  match Run.find_opt db (find_user ~id:1L) with
  | Some (id, Some name) -> Printf.printf "%Ld: %s\n" id name
  | Some (id, None) -> Printf.printf "%Ld has no name\n" id
  | None -> print_endline "no such user"

let rename db = Run.execute db (rename_user ~name:(Some "Ann") ~id:1L)
let total db = Run.find db count_users
```

The executor follows the last type argument: `find_opt` takes `zero_or_one`
(a primary-key lookup), `find` takes `exactly_one` (such as `COUNT(*)`), `iter`
takes `many`, and `execute` takes `affected_rows` (`INSERT`, `UPDATE`,
`DELETE`). Passing a request to the wrong one does not compile.

The same request runs unchanged on different drivers, because the driver does
the conversion to and from its own types inside `Make`.

A request is interpreted at run time, so it costs a little more than the code
`[%sql.Database]` generates: about 5 ns per column read (roughly 20 ns for a
row of three columns, 60 ns for twelve) and about 80 ns to bind twelve
parameters, measured against a driver that does nothing. That is a few times
the generated code, but a real driver spends microseconds on each row, so the
difference is far below one percent of a query.

Column codecs from the schema ([`-- [sqlgg] module=`](../sql/metadata.md), with
`get_column=` and `set_param=` to rename the functions) apply to requests as
they do to generated modules, for result columns, parameters and the values of
`IN @ids` and `VALUES @rows`:

```sql title="schema.sql"
CREATE TABLE ships (
  -- [sqlgg] module=Ship_id
  id INT PRIMARY KEY,
  -- [sqlgg] module=Ship_id
  parent_id INT
);
```

```ocaml
let find_ship = [%sql.Request {| SELECT id, parent_id FROM ships WHERE id = @id |}]

val find_ship :
  id:Ship_id.t -> (Ship_id.t * Ship_id.t option, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t
```

Such a column has the codec's own type under every driver, not a `'t`
projection. `Ship_id.set_param`, `Ship_id.get_column` and
`Ship_id.get_column_nullable` convert to and from the fixed runtime type of the
SQL type (`int64` for `INT`, `string` for `TEXT`, `float` for `DATETIME`), and
the driver reads and writes that value through its `get_column_int64` and
`set_param_int64` family, whatever its own `Int.t` is.

Lists and choices take the same argument types as in `[%sql]`: `IN @ids` takes a list, `VALUES @rows` and tuple `IN` take a
list of tuples, and choices and optional fragments take the same polymorphic
variants. Values spliced into the statement text (lists and rows) are quoted by
the driver, as with generated modules. An empty list gives `FALSE` (`TRUE` for
`NOT IN`), and an `INSERT` with no rows runs nothing and reports zero affected
rows without reaching the driver. `Run.sql request` returns the statement text
the driver would send, or `None` when there is nothing to run.

Limits:

- Dynamic selects, shared fragments (`include=reuse`) and DDL are rejected, as
  in `[%sql]`.
- `Run.find_opt db` applied to a connection alone has a weak type; apply it to
  a request, or write `fun request -> Run.find_opt db request`.

## Reference

`[%sql]` (or `[%sql.Database]` with an explicit traits module) takes exactly
one string literal holding exactly one `SELECT`, `INSERT`, `UPDATE` or `DELETE`,
and expands to the function the CLI generates for it at the top of the generated
module. The SQL is the same as in a `.sql` file: `@param` parameters,
`-- @name` annotations and comments all work. DDL belongs in the schema files.
The `subst`, `noparse`, `include` and
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
