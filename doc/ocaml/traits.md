---
sidebar_position: 1
title: Traits
description: OCaml traits system for type-safe database access
---

# Traits (OCaml-specific)

:::info
Traits are supported in **OCaml** and **C++**. Other target languages currently have hardcoded type mappings.
:::

## What Are Traits?

In sqlgg, **traits** is the OCaml generator architecture where generated code is parameterized by a module `T` that defines:

- Which OCaml types correspond to SQL types
- How to read result columns
- How to bind prepared-statement parameters
- How to execute queries (select/execute)
- Which async abstraction is used (`IO.future`)

When you see things like `T.Types.Float.t`, `id:T.num -> name:T.text -> ...`, or `'a IO.future` in generated code — that's the traits system at work.

## Generated Code Structure

The OCaml generator emits a functor:

```ocaml
(* Blocking/synchronous, -gen caml *)
module Sqlgg (T : Sqlgg_traits.M) = struct
  (* ... generated query functions ... *)
end

(* Async, -gen caml_io *)
module Sqlgg (T : Sqlgg_traits.M_io) = struct
  (* ... generated query functions with IO.future ... *)
end
```

You instantiate this functor with a concrete trait implementation for your database driver.

## SQL Type Wrappers

Each SQL type is represented by a `Types.<X>` module:

| SQL Type | Trait Module | Purpose |
|----------|--------------|---------|
| `INT` / `BIGINT` | `T.Types.Int` | Integer values |
| `TEXT` / `VARCHAR` | `T.Types.Text` | String values |
| `REAL` / `FLOAT` | `T.Types.Float` | Floating-point values |
| `DECIMAL` | `T.Types.Decimal` | Precise decimal values |
| `DATETIME` / `TIMESTAMP` | `T.Types.Datetime` | Date/time values |

### Why `T.Types.Float.t` Instead of `float`?

This wrapper provides two important properties:

1. **Dialect-correct literals** — `to_literal` centralizes SQL literal rendering/escaping
2. **Room for domain types** — `Decimal.t` doesn't have to be `float`; `Datetime.t` can be a library-specific time type

So `T.Types.Float.t` is intentionally **not required** to be an OCaml `float`.

## Result Rows

For `SELECT` queries, sqlgg generates functions that take a row handler callback:

```ocaml
val get_users : 
  T.connection -> 
  (id:T.num -> name:T.text -> email:T.text option -> 'a) -> 
  'a list
```

Where:
- `T.num` is typically an alias of `T.Types.Int.t`
- `T.text` is typically an alias of `T.Types.Text.t`
- `option` indicates nullable columns

Internally, the generated code:
- Reads values via `T.get_column_*`
- Handles SQL `NULL` via `*_nullable` variants
- Calls your callback with **labeled arguments** (column names)

## Parameters

For prepared-statement binding, generated code delegates to `T`:

```ocaml
T.start_params stmt n
T.set_param_Int p value
T.set_param_Text p value
T.set_param_null p
T.finish_params p
```

This makes the same generated code work with different drivers and type representations.

## Async vs Blocking

:::tip
For a blocking example, see [Getting Started](/docs/getting-started#running-the-example). This section focuses on the async (Lwt) setup.
:::

### Blocking Mode

For blocking mode, use `-gen caml`:

```bash
sqlgg -gen caml -params unnamed -static-header file.sql
```

Uses `Sqlgg_io.Blocking` where `'a future = 'a`:

```ocaml
module Blocking_db = Sqlgg_mariadb.Default(Sqlgg_io.Blocking)(struct 
  include Mariadb.Blocking
  type 'a future = 'a
end)
```

### Async Mode (Lwt Example)

For async mode, use `-gen caml_io`:

```bash
sqlgg -gen caml_io -params unnamed -static-header file.sql
```

The dune rule changes accordingly:

```lisp
(rule
 (target sql_file.ml)
 (mode promote)
 (action
  (with-stdout-to
   %{target}
   (run sqlgg -gen caml_io -params unnamed -static-header %{deps})))
 (deps file.sql))
```

For the nonblocking MariaDB setup, we need to create the `Mariadb.Nonblocking.Make` module. This part is taken directly from the [ocaml-mariadb Lwt example](https://github.com/ocaml-community/ocaml-mariadb/blob/master/examples/lwt/nonblocking_lwt_example.ml):

```ocaml
open Lwt.Infix

module IO_lwt = struct
  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind

  let bracket x dtor f =
    let%lwt x = x in
    (f x) [%finally dtor x]
end

module S = Mariadb.Nonblocking.Status
module M' = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = 'a Lwt.t
    let (>>=) = (>>=)
    let return = Lwt.return
  end

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt = if S.read status then Lwt_unix.wait_read fd else idle in
    let wt = if S.write status then Lwt_unix.wait_write fd else idle in
    let tt =
      match S.timeout status, Mariadb.Nonblocking.timeout mariadb with
      | true, 0 -> Lwt.return ()
      | true, tmout -> Lwt_unix.timeout (float tmout)
      | false, _ -> idle in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [rt; wt; tt] >>= fun _ ->
        Lwt.return @@
          S.create
            ~read:(Lwt_unix.readable fd)
            ~write:(Lwt_unix.writable fd)
            ())
      (function
      | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
      | e -> Lwt.fail e)
end)
```

Now we can create the sqlgg async trait implementation:

```ocaml
module X = Sqlgg_mariadb.Default(IO_lwt)(struct
  type 'a future = 'a IO_lwt.future
  include M'
end)

module Db = Sql_file.Sqlgg(X)
```

### Full Async Example

```ocaml
let main () =
  let open Lwt.Syntax in
  let* c = M'.connect ~host:"localhost" ~port:3306 
                      ~user:"user" ~pass:"pass" ~db:"mydb" () in
  match c with
  | Error _ -> Lwt.fail_with "Failed to connect to database"
  | Ok c ->
    let* (_: X.execute_response) = Db.create_table_users c in
    let* (_: X.execute_response) = Db.insert_sample c in
    let* cnt = Db.Fold.select_all c (fun ~id:_ ~name:_ ~age:_ acc -> acc + 1) 0 in
    let* result = Db.List.select_all c (fun ~id ~name ~age -> (id, name, age)) in
    prerr_endline @@ Printf.sprintf "Total rows: %d" cnt;
    List.iter (fun (id, name, age) ->
      prerr_endline @@ Printf.sprintf "ID: %Ld, Name: %s, Age: %s" 
        id name (match age with Some a -> Int64.to_string a | None -> "NULL")
    ) result;
    Lwt.return_unit

let () = Lwt_main.run (main ())
```

All operations (`select`, `execute`, `finish_params`, …) return `'a IO.future`.

## Built-in Trait Implementations

sqlgg ships ready-to-use trait implementations for common OCaml database drivers:

| Database | Module | Location |
|----------|--------|----------|
| SQLite3 | `Sqlgg_sqlite3` | `impl/ocaml/sqlite3/` |
| MySQL | `Sqlgg_mysql` | `impl/ocaml/mysql/` |
| MariaDB | `Sqlgg_mariadb` | `impl/ocaml/mariadb/` |

Each implements `Sqlgg_traits.M` / `M_io` on top of a concrete DB library.

## Custom Type Overrides

Sometimes you want generated code to use your domain types instead of defaults. For example:
- SQL `BOOL` → your custom `True | False` type
- SQL `DATETIME` → your project's datetime type
- SQL `ANY` → custom handling for unknown types

This is done by providing a custom `Types` module to `Sqlgg_mariadb.Make`.

### Full Example: Override `Bool` and `Any`

:::note
Yes, `True | False` instead of `bool` is a silly example — but it demonstrates the mechanism. In practice you'd override types like `Datetime` with your preferred time library, or `Decimal` with a precise decimal type.
:::

```ocaml
open Lwt.Infix

module IO_lwt = struct
  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
  let bracket x dtor f =
    let%lwt x = x in
    (f x) [%finally dtor x]
end

module S = Mariadb.Nonblocking.Status
module M' = Mariadb.Nonblocking.Make(struct
  module IO = struct
    type 'a future = 'a Lwt.t
    let (>>=) = (>>=)
    let return = Lwt.return
  end

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt = if S.read status then Lwt_unix.wait_read fd else idle in
    let wt = if S.write status then Lwt_unix.wait_write fd else idle in
    let tt =
      match S.timeout status, Mariadb.Nonblocking.timeout mariadb with
      | true, 0 -> Lwt.return ()
      | true, tmout -> Lwt_unix.timeout (float tmout)
      | false, _ -> idle in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [rt; wt; tt] >>= fun _ ->
        Lwt.return @@
          S.create ~read:(Lwt_unix.readable fd) ~write:(Lwt_unix.writable fd) ())
      (function
      | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
      | e -> Lwt.fail e)
end)

module M = struct
  type 'a future = 'a IO_lwt.future
  include M'
end

(* Custom Bool type *)
type bool' = True | False

(* Type signature with overrides *)
module type Types' =
  Sqlgg_mariadb.Types
    with type field = M'.Field.t
     and type value = M'.Field.value
     and type Bool.t = bool'
     and type Any.t = string

module Default_types = Sqlgg_mariadb.Default_types(M)

module Types = struct

  let int_of_field x = match M.Field.value x with
    | `Int v -> v
    | _ -> failwith "Invalid field type"

  module Any = struct
    type t = string
    type field = M.Field.t
    type value = M.Field.value

    let of_field x = match M.Field.value x with
      | `String v -> v
      | _ -> x |> int_of_field |> Int.to_string

    let to_value x = `String x
    let to_literal _ = failwith "to_literal Any"
  end
  
  module Bool = struct
    type t = bool'
    type field = M.Field.t
    type value = M.Field.value

    let of_field field = if int_of_field field <> 0 then True else False
    let to_value = function True -> `Int 1 | False -> `Int 0
    let to_literal = function True -> "true" | False -> "false"
  end

  include (Default_types : Types' with module Bool := Bool and module Any := Any)
end

(* Create customized trait implementation *)
module Sqlgg_mariadb = Sqlgg_mariadb.Make (IO_lwt) (M) (Types)

module Db = Sql_file.Sqlgg(Sqlgg_mariadb)
```

### SQL Query

```sql
-- @select_users_over_30
SELECT name, age > 30 AS over_30 FROM users;
```

### Usage

```ocaml
let () = 
  let main () =
    let open Lwt.Syntax in
    let* c = M'.connect ~host:"localhost" ~port:3306 
                        ~user:"user" ~pass:"pass" ~db:"mydb" () in
    match c with
    | Error _ -> Lwt.fail_with "Failed to connect to database"
    | Ok c ->
      Db.select_users_over_30 c (fun ~name ~over_30 ->
        prerr_endline @@ Printf.sprintf "%s: %s" name
          (match over_30 with 
           | Some True -> "over 30" 
           | Some False -> "30 or under" 
           | None -> "unknown"))
  in
  Lwt_main.run (main ())
```

:::tip
Without `Db.List.` or `Db.Fold.` the callback returns `unit` and rows are processed directly — no need to build a list first and then iterate.
:::

The key idea: generated code always talks to `T.Types.Bool.t`, `T.get_column_Bool`, `T.set_param_Bool`, etc. By swapping `Types`, you swap the concrete representation without changing any generated query code.

