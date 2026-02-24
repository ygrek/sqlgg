---
sidebar_position: 12
title: Dynamic Select
description: Runtime-composable SELECT columns with type safety
---

# Dynamic Select

:::caution Experimental
Dynamic Select is currently supported **only in OCaml** code generation (`-gen caml` / `-gen caml_io`).
:::

## Overview

Dynamic Select allows you to choose which columns to SELECT at runtime while maintaining full type safety. Just write a regular `SELECT` query and add the `dynamic_select=true` metadata flag — every column in the select list becomes a composable field that can be picked or combined at runtime using applicative combinators.

## Basic Syntax

Add a metadata comment before a regular SELECT query:

```sql
-- [sqlgg] dynamic_select=true
-- @select_product
SELECT id, name, price, category FROM products WHERE id = @id;
```

This generates a module `Select_product_col` with a field for each column:

```ocaml
module Select_product_col : sig
  type 'a t
  val pure : 'a -> 'a t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val id : int64 t
  val name : string t
  val price : float t
  val category : string t
end
```

The generated query function takes a `~col` parameter:

```ocaml
val select_product : db -> col:'a Select_product_col.t -> id:int64 -> 'a option IO.m
```

## Usage with Combinators

### Single Field

```ocaml
let* result = Db.select_product conn ~col:Select_product_col.name ~id:1L
(* result : string option *)
```

### Combined Fields with `let+` / `and+`

```ocaml
open Select_product_col

let combined =
  let+ n = name
  and+ p = price in
  (n, p)

let* result = Db.select_product conn ~col:combined ~id:1L
(* result : (string * float) option *)
```

### Three or More Fields

```ocaml
let all_fields =
  let+ n = name
  and+ p = price
  and+ c = category in
  (n, p, c)
```

### With Transformation

```ocaml
let doubled_price =
  let+ p = price in
  p *. 2.0
```

### Constant Values

```ocaml
let with_constant =
  let+ n = name
  and+ label = pure "fixed_label" in
  (n, label)
```

## Multiple Rows (Callback)

Dynamic select works the same for queries returning multiple rows. The callback receives `~col`:

```sql
-- [sqlgg] dynamic_select=true
-- @list_products
SELECT id, name, price FROM products WHERE stock > @min_stock;
```

```ocaml
open List_products_col

let combined =
  let+ i = id
  and+ n = name
  and+ p = price in
  (i, n, p)

let () = Db.list_products conn ~col:combined ~min_stock:5L (fun ~col ->
  let (i, n, p) = col in
  printf "id=%Ld, name=%s, price=%.2f\n" i n p
)
```

## Aliased Expressions

For computed expressions, use `AS` to name the generated field:

```sql
-- [sqlgg] dynamic_select=true
-- @product_info
SELECT CONCAT(name, ' - ', category) AS label, price * stock AS total_value
FROM products WHERE id = @id;
```

```ocaml
module Product_info_col : sig
  ...
  val label : string t
  val total_value : float t
end
```

## Literal Values

String literals become fields too:

```sql
-- [sqlgg] dynamic_select=true
-- @with_verbatim
SELECT id, name, 'N/A' AS fallback, category FROM products WHERE id = @id;
```

```ocaml
val fallback : string t  (* always returns 'N/A' *)
```

## Columns with Parameters

When a column expression contains parameters, the generated field becomes a function:

```sql
-- [sqlgg] dynamic_select=true
-- @with_param
SELECT id, name, @custom_value :: Text AS custom FROM products WHERE id = @id;
```

```ocaml
module With_param_col : sig
  ...
  val id : int64 t
  val name : string t
  val custom : string -> string t
end

(* Usage *)
let* r = Db.with_param conn ~col:(With_param_col.custom "Hello") ~id:1L

(* Combine with other fields *)
let combined =
  let+ i = id
  and+ n = name
  and+ c = custom "Hello" in
  (i, n, c)
```

### Arithmetic Parameters

```sql
-- [sqlgg] dynamic_select=true
-- @with_arith
SELECT id, price + @tax AS add_tax FROM products WHERE id = @id;
```

```ocaml
val add_tax : float -> float t
```

### Multiple Parameters

```sql
-- [sqlgg] dynamic_select=true
-- @with_two_params
SELECT id, (@min <= price) AND (price <= @max) AS in_range FROM products WHERE id = @id;
```

```ocaml
val in_range : float -> float -> bool t
```

### CASE Expression

```sql
-- [sqlgg] dynamic_select=true
-- @with_case
SELECT id, CASE WHEN id = @id2 THEN @v ELSE 0 END AS casey FROM products WHERE id = @id;
```

```ocaml
val casey : int64 -> int64 -> int64 t
```

### Subqueries with IN Lists

Subqueries need `AS` alias:

```sql
-- [sqlgg] dynamic_select=true
-- @with_in
SELECT id, (SELECT 1 FROM products WHERE price IN @prices LIMIT 1) AS filtered
FROM products WHERE id = @id;
```

```ocaml
val filtered : float list -> int64 t
```

### Option Actions

```sql
-- [sqlgg] dynamic_select=true
-- @with_opt
SELECT id, (SELECT 1 FROM products WHERE { price > @min_price }? LIMIT 1) AS opt
FROM products WHERE id = @id;
```

```ocaml
val opt : float option -> int64 t
```

### Tuple Lists

```sql
-- [sqlgg] dynamic_select=true
-- @with_tuples
SELECT id, (SELECT 1 FROM products WHERE (id, stock) IN @pairs LIMIT 1) AS pairs
FROM products WHERE id = @id;
```

```ocaml
val pairs : (int64 * int64) list -> int64 t
```

### Mixed Parameters and IN Lists

```sql
-- [sqlgg] dynamic_select=true
-- @with_param_and_in
SELECT id, CONCAT(name, @suffix) IN @names AS match_ FROM products WHERE id = @id;
```

```ocaml
val match_ : string -> string list -> bool t
```

## Complex Subqueries

All SQL constructs work inside dynamic columns — subqueries, CASE, option-actions, IN lists, tuple lists:

```sql
-- [sqlgg] dynamic_select=true
-- @ultimate_combo
SELECT
    id,
    stock AS plain,
    (SELECT COUNT(*) FROM products WHERE id IN @ids) AS with_in_list,
    (SELECT stock FROM products WHERE { stock > @min_stock2 }? LIMIT 1) AS with_optional,
    CASE WHEN @mode = 1
        THEN (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
        ELSE (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
    END AS with_case,
    (SELECT 1 FROM products WHERE (id, stock) IN @id_stock_pairs LIMIT 1) AS with_tuple_list,
    (SELECT COUNT(*)
     FROM products p2
     WHERE { p2.id = @filter_id }?
       AND p2.name IN @name_list
       AND p2.price > @min_price) AS full_combo
FROM products WHERE id = @id;
```

Each column becomes a field — simple ones like `id` and `plain` have no arguments, while columns with parameters become functions:

```ocaml
val id : int64 t
val plain : int64 t
val with_in_list : int64 list -> int64 t
val with_optional : int64 option -> int64 t
val with_case : int64 -> string list -> int64 t
val with_tuple_list : (int64 * int64) list -> int64 t
val full_combo : int64 option -> string list -> float -> int64 t
```

Pick any subset and combine:

```ocaml
let combined =
  let+ i = id
  and+ p = plain
  and+ il = with_in_list [1L; 2L]
  and+ fc = full_combo (Some 5L) ["x"] 100.0 in
  (i, p, il, fc)
```

## Star Expansion

`SELECT *` is supported — all table columns become dynamic fields:

```sql
-- [sqlgg] dynamic_select=true
-- @all_cols
SELECT * FROM products WHERE id = @id;
```

You can also mix `*` with extra expressions:

```sql
-- [sqlgg] dynamic_select=true
-- @all_plus_expr
SELECT *, id + 2 AS id_plus FROM products WHERE id = @id;
```

## Module-Wrapped Columns

Works with `[sqlgg] module=` annotations on columns:

```sql
CREATE TABLE products_wrapped (
    -- [sqlgg] module=Product_id
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2)
);

-- [sqlgg] dynamic_select=true
-- @with_module
SELECT id, name, price FROM products_wrapped WHERE id = @id;
```

The `id` field will use the `Product_id` module type.

## Records for Readability

You don't have to return tuples — use records:

```ocaml
type product_info = { name: string; price: float }

let info_col =
  let open Select_product_col in
  let+ n = name
  and+ p = price in
  { name = n; price = p }

let* result = Db.select_product conn ~col:info_col ~id:1L
(* result : product_info option *)
```

## How It Works

Each field generates a record with:

```ocaml
type 'a t = {
  set: params -> unit;             (* set prepared statement params *)
  read: row -> int -> 'a * int;   (* read from row, return next index *)
  column: string;                  (* SQL fragment to insert *)
  count: int;                      (* number of ? parameters *)
}
```

The `apply` combinator merges two fields:
- Concatenates `column` strings (with `, ` separator)
- Sequences `set` calls
- Chains `read` with index threading
- Sums `count` values

The generated query builds SQL dynamically:

```ocaml
T.select db ("SELECT " ^ col.column ^ " FROM products WHERE id = ?") ...
```

## See Also

- [OCaml Traits](../ocaml/traits.md) — `DynamicSelect` module definition
- [Expressions](./expressions.md) — `@choice` and other expressions
