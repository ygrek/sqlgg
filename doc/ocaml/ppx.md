---
sidebar_position: 3
title: PPX
description: Map query results to OCaml records with [@@deriving sqlgg]
---

# Deriving Record Projections

Take two [Dynamic Select](../sql/dynamic-select.md) queries that select different columns but share a couple of them:

```sql
CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- [sqlgg] dynamic_select=true
-- @q1
SELECT id, name, price, category FROM products WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- @q2
SELECT stock, id, name FROM products WHERE stock > @min_stock;
```

Each query gets a module. Every column of its select list becomes a value there, and the same columns come packed a second time into an object called `cols`:

```ocaml
module Q1 : sig
  type 'a t

  val id : int64 t
  val name : string option t
  val price : float option t
  val category : string option t

  val cols :
    < id : int64 t
    ; name : string option t
    ; price : float option t
    ; category : string option t >

  val select : [> `RO ] T.connection -> 'a t -> id:int64 -> 'a option T.io_future
end
```

Building a record out of the columns is an applicative chain of `let+` and `and+`:

```ocaml
type who = { id : int64; name : string option }

let who_from_q1 = Q1.(let+ id = id and+ name = name in { id; name })
```

Written this way the record is bound to `Q1`. `id` and `name` resolve in that module and nowhere else, so `Q2`, which selects both columns as well, gets its own copy of the chain:

```ocaml
let who_from_q2 = Q2.(let+ id = id and+ name = name in { id; name })
```

`[@@deriving sqlgg]` derives the chain from the record type instead, once for both queries:

```ocaml
type who = { id : int64; name : string option } [@@deriving sqlgg]
```

It writes out the same chain, except the columns are now read off a `cols` object instead of a module:

```ocaml
let who_of_cols cols =
  let open Sqlgg_scope in
  let+ id = cols#id
  and+ name = cols#name in
  { id; name }
```

which is what makes one function serve both queries:

```ocaml
let who_from_q1 = Q1.(who_of_cols cols)
let who_from_q2 = Q2.(who_of_cols cols)
```

The derived function is named after the type, so `who` gives `who_of_cols` (and a type named `t` gives plain `of_cols`).

Only the record's fields reach the `SELECT` list, and [unused JOINs](../sql/dynamic-select.md#unused-join-elimination) go with them.

## Setup

Add the preprocessor to your `dune` file:

```lisp
(library
 (name my_queries)
 (libraries sqlgg.traits ...)
 (preprocess (pps sqlgg.ppx)))
```

## Running a Query

The derived function is an ordinary column value, so it goes straight into `select`:

```ocaml
let q1 db = Q1.(select db (who_of_cols cols) ~id:1L)

let q2 db =
  Q2.(select db (who_of_cols cols) ~min_stock:10L
    (fun (w : who) -> print_endline (Option.value w.name ~default:"?")))
```

Both calls run `SELECT id, name` and return `who` records. `q2` never fetches `stock`, since the record does not ask for it.

## Query Isolation

A column belongs to the query it came from:

```ocaml
let bad db = Q2.select db Q1.Cols.name ~min_stock:10L (fun _ -> ())
```

This does not compile. `Q1.Cols.name` is not something `Q2.select` accepts.

## Composition

A derived function returns an ordinary column. Chain it with other derived records and the result still fits any query:

```ocaml
type ident = { id : int64 } [@@deriving sqlgg]
type naming = { name : string option } [@@deriving sqlgg]

let ident_and_naming cols =
  let open Sqlgg_scope in
  let+ i = ident_of_cols cols
  and+ n = naming_of_cols cols in
  (i, n)

let q db = Q1.(select db (ident_and_naming cols) ~id:1L)
```

And with the query's own columns. Take a third query joining a table neither record knows about:

```sql
CREATE TABLE stock_info (
    product_id INT NOT NULL PRIMARY KEY,
    warehouse TEXT NULL
);

-- [sqlgg] dynamic_select=true
-- @q3
SELECT p.id, p.name, s.warehouse
FROM products p
LEFT JOIN stock_info s ON s.product_id = p.id
WHERE p.id = @id;
```

```ocaml
let with_warehouse db =
  Q3.(select db
    (let+ i, n = ident_and_naming cols
     and+ wh = warehouse in
     (i, n, wh))
    ~id:1L)
```

`ident_and_naming` needs `cols` to fit any query. `warehouse` is already in `Q3`, so it is taken as is.

Drop `warehouse` from the chain and the join to `stock_info` is [eliminated](../sql/dynamic-select.md#unused-join-elimination), nothing reads from it anymore.

## `[@sqlgg.col "column_name"]`

When a record field name does not match the SQL column name, override it per field:

```ocaml
type renamed = {
  id : int64;
  productName : string option; [@sqlgg.col "name"]
}
[@@deriving sqlgg]

let q db = Q1.(select db (renamed_of_cols cols) ~id:3L)
```

Here the `productName` field is read from the `name` column.
