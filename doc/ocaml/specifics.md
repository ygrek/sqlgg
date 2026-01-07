---
sidebar_position: 2
title: Expressions
description: OCaml representation of sqlgg expressions
---

# OCaml Expression Representations

How sqlgg expressions map to OCaml types.

## `@choice` → Polymorphic Variants {#choice-expression-types}

[`@choice` expressions](../sql/expressions.md#choice--conditional-branches) become OCaml [polymorphic variants](https://v2.ocaml.org/manual/polyvariant.html).

```sql
@choice { A { 1 } | B { @b + 2 } | E { @x - 2 } }
```

```ocaml
[`A | `B of int | `E of int]
```

- `` `A `` — no payload (constant branch)
- `` `B of int `` — carries the `@b` parameter
- `` `E of int `` — carries the `@x` parameter

### Multiple Parameters → Tuple

```sql
@choice { C { col_c BETWEEN @start AND @end } | D { 1 = 1 } }
```

```ocaml
[`C of (string * string) | `D]
```

### Column References (No Parameters) {#parameterless-choice-types}

```sql
@choice { A { col_a } | B { col_b } | C { col_c } }
```

```ocaml
[`A | `B | `C]
```

### Full Example {#select-with-choice-parameter}

```sql
-- @search_products
SELECT * FROM products 
WHERE price > @choice { Cheap { 10.0 } | Expensive { @min_price } | Free { 0.0 } };
```

```ocaml
val search_products : 
  T.connection ->
  choice:[< `Cheap | `Expensive of float | `Free ] ->
  (id:int64 -> name:string -> price:float option -> category:string option -> 'a) ->
  'a list IO.future

(* Usage *)
let* cheap = search_products db `Cheap callback
let* expensive = search_products db (`Expensive 100.0) callback
```

### Nested Choice → Nested Variants {#nested-choice}

[Nested `@choice` expressions](../sql/expressions.md#nested-choice-expressions) produce nested polymorphic variants:

```sql
CREATE TABLE products (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    category VARCHAR(64) NOT NULL,
    price DECIMAL(12, 2) NOT NULL,
    size VARCHAR(8),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- @filter_products
SELECT * FROM products
WHERE @category {
    Electronics { 
        category = 'electronics' AND
        @price_range {
            Premium { price > 1000 }
            | Budget { price <= 200 }
        }
    }
    | Clothing {
        category = 'clothing' AND
        @size {
            Large { size IN ('L', 'XL', 'XXL') }
            | Small { size IN ('XS', 'S', 'M') }
        }
    }
};
```

Generated signature:

```ocaml
val filter_products :
  T.connection ->
  category:[< `Electronics of [< `Premium | `Budget ]
            | `Clothing of [< `Large | `Small ] ] ->
  (id:int64 -> category:string -> price:float -> size:string option -> 
   created_at:T.datetime option -> 'a) ->
  'a list IO.future
```


## `{...}?` → `option` {#option-actions-in-ocaml}

[Optional clauses](../sql/expressions.md#--optional-clauses) map parameters to `option`:

```sql
-- @find_users
SELECT * FROM users WHERE { age > @min_age }?;
```

```ocaml
val find_users : T.connection -> min_age:int64 option -> ... -> 'a list IO.future

(* Usage *)
let* users = find_users db ~min_age:(Some 18) callback  (* with filter *)
let* users = find_users db ~min_age:None callback       (* no filter *)
```

## `IN @list` → `list` {#list-parameters}

```sql
SELECT * FROM users WHERE id IN @ids;
```

```ocaml
ids:int64 list
```

## `IN @tuple_list` → `tuple list` {#tuple-list-parameters}

```sql
SELECT * FROM user_roles WHERE (user_id, role_id) IN @pairs;
```

```ocaml
pairs:(int64 * int64) list
```

## Type Mapping Summary

| SQL Expression | OCaml Type |
|----------------|------------|
| `IN @list` | `'a list` |
| `(a, b) IN @tuple_list` | `('a * 'b) list` |
| `@choice { A \| B of ... }` | `` [< `A \| `B of ... ] `` |
| `{ @param }?` | `'a option` |
| Nullable column | `'a option` |

Basic SQL types (`INT`, `TEXT`, `FLOAT`, etc.) map to corresponding OCaml types — see [traits](./traits.md) for the full mapping.

For JSON handling, see [JSON in OCaml](./json.md).
