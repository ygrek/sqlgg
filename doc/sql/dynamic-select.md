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

Dynamic Select allows you to choose which columns to SELECT at runtime while maintaining full type safety. Unlike regular `@choice` which selects between SQL fragments, Dynamic Select generates a module with composable field selectors that can be combined using applicative combinators.

## Basic Syntax

Enable with a metadata comment before the query:

```sql
-- [sqlgg] dynamic_select=true
-- @select_product
SELECT id, @col { Name { name } | Price { price } | Category { category } }
FROM products WHERE id = @id;
```

This generates a module `Select_product_col` with field constructors:

```ocaml
module Select_product_col : sig
  include Sqlgg_traits.DynamicSelect
  
  val name : ([`Name], string option, 'row, 'params) t
  val price : ([`Price], float option, 'row, 'params) t  
  val category : ([`Category], string option, 'row, 'params) t
end
```

## Usage with Combinators

### Single Field

```ocaml
let* result = Db.select_product conn ~col:Select_product_col.name ~id:1L
(* result : (int64 * string option) option *)
```

### Combined Fields with `let+` / `and+`

```ocaml
open Select_product_col

let combined = 
  let+ n = name
  and+ p = price in
  (n, p)

let* result = Db.select_product conn ~col:combined ~id:1L
(* result : (int64 * (string option * float option)) option *)
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
  Option.map (fun x -> x *. 2.0) p
```

### Constant Values

```ocaml
let with_constant =
  let+ n = name
  and+ label = return "fixed_label" in
  (n, label)
```

## Branches with Parameters

Parameters inside branches become function arguments:

```sql
-- [sqlgg] dynamic_select=true
-- @with_param
SELECT id, @col { 
    Static { name } 
  | Dynamic { @prefix :: Text } 
  | Computed { price + @tax }
} FROM products WHERE id = @id;
```

```ocaml
module With_param_col = struct
  val static : ([`Static], ...) t                      (* no args *)
  val dynamic : string -> ([`Dynamic], ...) t          (* prefix arg *)
  val computed : float option -> ([`Computed], ...) t  (* tax arg *)
end

(* Usage *)
let* r = Db.with_param conn ~col:(dynamic "Mr. ") ~id:1L
let* r = Db.with_param conn ~col:(computed (Some 0.2)) ~id:1L
```

## Complex Nested Expressions

Dynamic Select supports all sqlgg expressions inside branches:

### IN Lists

```sql
@col { Filtered { (SELECT COUNT(*) FROM t WHERE id IN @ids) } }
```

```ocaml
val filtered : int64 list -> ([`Filtered], ...) t
```

### Option Actions

```sql
@col { Optional { (SELECT x FROM t WHERE { price > @min }? LIMIT 1) } }
```

```ocaml
val optional : float option -> ([`Optional], ...) t
```

### Nested Choices

```sql
@col { 
  WithChoice { 
    CASE WHEN @mode { Sum { 1 } | Avg { 2 } } = 1 
    THEN SUM(x) ELSE AVG(x) END 
  } 
}
```

```ocaml
val withchoice : [`Sum | `Avg] -> ([`WithChoice], ...) t
```

### Tuple Lists

```sql
@col { Pairs { (SELECT 1 FROM t WHERE (a, b) IN @pairs LIMIT 1) } }
```

```ocaml
val pairs : (int64 * int64 option) list -> ([`Pairs], ...) t
```

## Multiple Dynamic Columns

You can have multiple dynamic columns in one query:

```sql
-- [sqlgg] dynamic_select=true  
-- @multi
SELECT @x { A { name } | B { category } }, 
       @y { C { price } | D { stock } }
FROM products WHERE id = @id;
```

```ocaml
let* result = Db.multi conn 
  ~x:Multi_x.(let+ a = a and+ b = b in (a, b))
  ~y:Multi_y.c
  ~id:1L
```

You don't have to return tuples — use records for better readability:

```ocaml
type product_info = { name: string option; price: float option }

let info_col = 
  let open Select_product_col in
  let+ n = name
  and+ p = price in
  { name = n; price = p }

let* result = Db.select_product conn ~col:info_col ~id:1L
(* result : (int64 * product_info) option *)
```

## Dynamic Column at First Position

When dynamic column is first, static columns after it use computed indices:

```sql
-- [sqlgg] dynamic_select=true
-- @first_dynamic  
SELECT @col { Name { name } | Price { price } }, id, stock
FROM products WHERE id = @id;
```

The generated code correctly handles column index offsets based on how many columns the dynamic field contributes.

## How It Works

Each field generates a record with:

```ocaml
type ('tag, 'value, 'row, 'params) field_data = {
  set: 'params -> unit;        (* set prepared statement params *)
  read: 'row -> int -> 'value * int;  (* read from row, return next index *)
  column: string;              (* SQL fragment to insert *)
  count: int;                  (* number of ? parameters *)
}
```

The `both` combinator merges two fields:
- Concatenates `column` strings (with `, ` separator)
- Sequences `set` calls
- Chains `read` with index threading
- Sums `count` values

## Comparison with `@choice`

Although the syntax looks similar, `@choice` and Dynamic Select serve different purposes:

**`@choice`** is pattern matching for a **single expression**. All branches must return the **same type** — just like `match` in OCaml. You pick one branch at runtime:

```sql
SELECT * FROM products 
WHERE price > @filter { Cheap { 10 } | Expensive { 100 } };
-- Both branches return INT, used in same position
```

**Dynamic Select** is for choosing **which columns** to include. Each branch can have a **different type** because they represent different columns. You can also combine multiple branches:

```sql
-- [sqlgg] dynamic_select=true
SELECT id, @col { Name { name } | Price { price } } FROM products;
-- Name returns TEXT, Price returns DECIMAL — different types OK
-- Can combine: let+ n = name and+ p = price in (n, p)
```

| Feature | `@choice` | Dynamic Select |
|---------|-----------|----------------|
| Purpose | Conditional expression | Column selection |
| Type per branch | Must be same | Can differ |
| Combine branches | ✗ (pick one) | ✓ (with `and+`) |
| Transform results | ✗ | ✓ (with `let+`) |
| Generated code | Inline match | Separate module |

## See Also

- [OCaml Traits](../ocaml/traits.md) — `DynamicSelect` module definition
- [Expressions](./expressions.md) — `@choice` and other expressions
