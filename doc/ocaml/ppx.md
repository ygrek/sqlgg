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
type product = { id : int64; name : string option }

let product_from_q1 = Q1.(let+ id = id and+ name = name in { id; name })
```

Written this way the record is bound to `Q1`. `id` and `name` resolve in that module and nowhere else, so `Q2`, which selects both columns as well, gets its own copy of the chain:

```ocaml
let product_from_q2 = Q2.(let+ id = id and+ name = name in { id; name })
```

`[@@deriving sqlgg]` derives the chain from the record type instead, once for both queries:

```ocaml
type product = { id : int64; name : string option } [@@deriving sqlgg]
```

It writes out the same chain, except the columns are now read off a `cols` object instead of a module:

```ocaml
let product_of_cols cols =
  let open Sqlgg_scope in
  let+ id = cols#id
  and+ name = cols#name in
  { id; name }
```

which is what makes one function serve both queries:

```ocaml
let product_from_q1 = Q1.(product_of_cols cols)
let product_from_q2 = Q2.(product_of_cols cols)
```

The derived function is named after the type, so `product` gives `product_of_cols` (and a type named `t` gives plain `of_cols`).

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
let q1 db = Q1.(select db (product_of_cols cols) ~id:1L)

let q2 db =
  Q2.(select db (product_of_cols cols) ~min_stock:10L
    (fun (w : product) -> print_endline (Option.value w.name ~default:"?")))
```

Both calls run `SELECT id, name` and return `product` records. `q2` never fetches `stock`, since the record does not ask for it.

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

## Per-Field Attributes

An attribute on a field changes where its value comes from, or what happens to
it on the way in or out.

### `[@sqlgg.col "column_name"]`

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

### `[@sqlgg.map f]`

`f` runs on the value read from the column. The column is whatever `f` takes,
the field is whatever it gives back:

```ocaml
type counts = {
  id : int64;
  reply_count : int; [@sqlgg.map Int64.to_int]
}
[@@deriving sqlgg]
```

```ocaml
val counts_of_cols : < id : int64 col; reply_count : int64 col; .. > -> counts col
```

Use it when the conversion belongs to *this* record rather than to the column,
for instance when two queries want the same column read differently. A
conversion that belongs to the *column* is better declared once in the schema
with [`-- [sqlgg] module=`](../sql/metadata.md), which then applies to every
query that touches it.

### `[@sqlgg.by]`

Leaves the conversion open and takes it as a labelled argument, so one record
serves call sites whose raw column types differ:

```ocaml
type counts = { id : int64; n : int [@sqlgg.by] } [@@deriving sqlgg]
```

```ocaml
val counts_of_cols : n:('raw -> int) -> < id : int64 col; n : 'raw col; .. > -> counts col
```

```ocaml
let from_number = Q_num.(counts_of_cols ~n:Int64.to_int cols)
let from_label  = Q_txt.(counts_of_cols ~n:String.length cols)
```

The argument is mandatory, and deliberately so. An optional argument needs a
default, and any concrete default would fix `'raw` to whatever that one
function takes, for every call site at once. That is exactly the freedom the
attribute exists to give. **Invariant: `[@sqlgg.by]` is never optional.**

### `[@sqlgg.default v]`

Sugar for `[@sqlgg.map (fun x -> Option.value x ~default:v)]`: the column is
nullable, the field is not.

```ocaml
type counts = { id : int64; hits : int64 [@sqlgg.default 0L] } [@@deriving sqlgg]
```

```ocaml
val counts_of_cols : < id : int64 col; hits : int64 option col; .. > -> counts col
```

The default replaces `NULL`, so the field cannot be an option.

### `[@sqlgg.nested]`

A field whose type is another derived record is read from the same `cols`:

```ocaml
type content = { text : string option [@sqlgg.col "body"] } [@@deriving sqlgg]

type post = {
  id : int64;
  content : content; [@sqlgg.nested]
  reply_count : int; [@sqlgg.map Int64.to_int]
}
[@@deriving sqlgg]
```

The reader calls the inner one on the same object:

```ocaml
let post_of_cols cols =
  let+ id = cols#id
  and+ content = content_of_cols cols
  and+ reply_count = cols#reply_count in
  { id; content; reply_count = Int64.to_int reply_count }
```

So the query must supply `content`'s columns too. The row type says that with a
second constraint. It does not name those columns.

```ocaml
type (..., 'cols) post_cols = 'cols
  constraint 'cols = < id : int64 col; reply_count : int64 col; .. >
  constraint 'cols = (..., 'cols) content_cols

val post_of_cols : (..., 'cols) post_cols -> post col
```

Nest one more record and you get one more constraint. A record from another
module works the same way: `channel : Feed.channel [@sqlgg.nested]` calls
`Feed.channel_of_cols`.

#### Over an outer join

Columns from the right side of an outer join arrive as options, so a record read
straight off one gets an option per column:

```sql
-- [sqlgg] dynamic_select=true
-- @q4
SELECT p.id, p.name, s.product_id, s.place, s.warehouse
FROM products p
LEFT JOIN stock_info s ON s.product_id = p.id
WHERE p.id = @id;
```

Make the field an option and the attribute puts the option on the relation
instead:

```ocaml
type stock = { product_id : int64; place : string; warehouse : string option }
[@@deriving sqlgg ~nullable_cols]

type product = {
  id : int64;
  name : string option;
  stock : stock option; [@sqlgg.nested]
}
[@@deriving sqlgg]

let with_stock db = Q4.(select db (product_of_cols cols) ~id:1L)
```

The field's type picks the reading:

```ocaml
stock : stock          [@sqlgg.nested]   (* columns as declared *)
stock : stock option   [@sqlgg.nested]   (* columns from an outer join *)
```

Pick the wrong one and it does not typecheck against that query.

#### Present, absent, and neither

A **strict** field is one that is not an option and has no
[`[@sqlgg.default]`](#sqlggdefault-v). `[@sqlgg.nested]` reads the relation like
this:

```ocaml
match product_id, place, warehouse with
| Some product_id, Some place, _ -> Some { product_id; place; warehouse }
| None, None, None -> None
| None, _, _ -> failwith "sqlgg: stock.product_id is NULL"
| _, None, _ -> failwith "sqlgg: stock.place is NULL"
```

A missing match nulls every column at once, so the failing arms are not a
missing match. `default_none` gives `None` there instead:

```ocaml
Option.bind product_id (fun product_id ->
  Option.bind place (fun place -> Some { product_id; place; warehouse }))
```

Optional and defaulted fields never raise. Conversions run only once the
relation is there.

#### `~nullable_cols`

The two functions above are not derived by default. Ask for them:

```ocaml
type stock = { product_id : int64; place : string; warehouse : string option }
[@@deriving sqlgg ~nullable_cols]
```

```ocaml
type (..., 'cols) stock_nullable_cols

val stock_of_nullable_cols     : (..., 'cols) stock_nullable_cols -> stock option col
val stock_of_nullable_cols_exn : (..., 'cols) stock_nullable_cols -> stock option col
```

`[@sqlgg.nested]` calls the second, `default_none` the first. You can also call
them yourself, which is the way to read a relation without inventing a record
for it:

```ocaml
let product_and_stock cols =
  let+ p = product_of_cols cols
  and+ s = stock_of_nullable_cols_exn cols in
  (p, s)
```

```ocaml
val product_and_stock : < ... > -> (product * stock option) col
```

A tuple, and the folding still happens. Adding a `stock option` field to
`product` would have changed every place that reads a product.

Put the flag on the **inner** record. A record cannot see who will nest it. The
deriver gets one type declaration at a time, so `stock` is expanded before the
compiler reaches `product`. Forget the flag and the error points at the nesting:

```
Error: Unbound type constructor "stock_nullable_cols"
```

A record read as a relation reads its own nested fields the same way, so they
need the flag too.
