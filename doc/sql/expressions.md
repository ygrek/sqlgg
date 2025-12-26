---
sidebar_position: 4
title: Expressions
description: Choice expressions, list parameters, and optional clauses
---

# Expressions

sqlgg extends SQL with special expression syntax for dynamic queries. These expressions allow you to build flexible, type-safe queries that adapt at runtime while maintaining full static type checking.

## `@choice` — Conditional Branches

Pattern matching-like conditional logic that generates different SQL fragments based on runtime selection.

```sql
@choice { A { expr1 } | B { expr2 } | C { expr3 } }
```

The `@choice` expression creates a **variant type** of SQL expressions. Each branch is named (like `A`, `B`, `C`) and contains an SQL expression. At runtime, you select which branch to use.

### Basic Example

```sql
-- @search_products
SELECT * FROM products 
WHERE price > @choice { 
    Cheap { 10.0 } | 
    Expensive { @min_price } |
    All { 0.0 }
};
```

- `Cheap` — no parameter, literal `10.0`
- `Expensive` — takes `@min_price` parameter
- `All` — no parameter, literal `0.0`

### With Different Condition Types

Each branch can have completely different conditions with different parameter types:

```sql
-- @filter
SELECT * FROM test_table 
WHERE @choice {
    ById { id = @id } |
    ByName { name LIKE @pattern } |
    ByRange { created_at BETWEEN @start AND @end } |
    All { 1 = 1 }
};
```

- `ById`: takes `int` parameter
- `ByName`: takes `string` parameter
- `ByRange`: takes `(timestamp * timestamp)` tuple
- `All`: no parameter (always true)

### In ORDER BY

Use `@choice` to select sorting column dynamically:

```sql
-- @sorted
SELECT * FROM products 
ORDER BY @choice { ByPrice { price } | ByName { name } | ById { id } } DESC;
```

All branches reference columns — no parameters needed.

### Nested Choice Expressions

`@choice` expressions can be nested for hierarchical filtering:

```sql
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

First you choose the category (`Electronics` or `Clothing`), then a sub-filter within that category. See [OCaml specifics](/docs/ocaml/specifics) for how this maps to nested variant types.

### Choice with EXISTS Subqueries

Branches can contain complex expressions including `EXISTS`:

```sql
SELECT t2.col_2_1, t2.col_2_4
FROM tbl_2 t2
WHERE col_2_1 IN (
    SELECT col_2_1 
    FROM tbl_2
    WHERE @var_2 {
        Pat_3 { t2.col_2_4 = 'stat_1' }
        | Pat_4 {
            EXISTS (
                SELECT 1 
                FROM tbl_3 t3
                JOIN tbl_1 t1 ON t3.col_3_3 = t1.col_1_1
                WHERE t3.col_3_2 = t2.col_2_1
                AND t1.col_1_3 = 'cat_a'
            )
        }
    }
);
```

### Choice for NULL Handling

Handle nullable values explicitly:

```sql
INSERT INTO test (k) VALUES (@k { None { NULL } | Some { @k } });
```

- `None` — inserts SQL `NULL`
- `Some` — inserts the provided value

**OCaml representation:** [Polymorphic Variants](../ocaml/specifics.md#choice-expression-types)

---

## `IN @list` — List Parameters

Dynamic IN clauses with type-safe list parameters:

```sql
-- @find_by_ids
SELECT * FROM users WHERE id IN @user_ids;
```

`@user_ids`: `int list`

```sql
-- @exclude_departments
SELECT * FROM users WHERE department NOT IN @excluded;
```

`@excluded`: `string list`

### Multiple List Parameters

You can use multiple list parameters in the same query:

```sql
-- @find2
SELECT * FROM foo
WHERE (id IN @ids) AND foo NOT IN @foos;
```

### List Parameters with Expressions

The left side of `IN` can be any expression:

```sql
-- @find_with_bar
SELECT * FROM foo
WHERE CONCAT(foo, @suffix) IN @foos_with_suffix;
```

Here `@suffix` is a regular string parameter, while `@foos_with_suffix` is a list parameter.

### List Parameters in JOINs

```sql
-- @join
SELECT *
FROM foo f JOIN bar b ON f.id = b.foo_id
WHERE b.baz IN @bazz 
  AND b.baz NOT IN @notbazz 
  AND LENGTH(f.foo) IN @lengths;
```

### List Parameters in Nested Subqueries

```sql
UPDATE test30 t30
SET t30.column_a = 'the value'
WHERE t30.column_c_31 IN (
    SELECT t31.id
    FROM test31 t31
    WHERE t31.column_c_32 IN (
        SELECT t32.id
        FROM test32 t32
        WHERE t32.column_f IN @c_f_ids
    )
);
```

**OCaml representation:** [List Parameters](../ocaml/specifics.md#list-parameters)

---

## `IN @tuple_list` — Tuple List Parameters

Multi-column IN with tuples:

```sql
-- @find_roles
SELECT * FROM user_roles 
WHERE (user_id, role_id) IN @pairs;
```

`@pairs`: `(int * int) list`

### Complex Tuple Matching

Tuples can include expressions and type casts:

```sql
SELECT col1, col2
FROM table1
WHERE (1, col1, col2, col1 + col1, 6, 11, (@test3 :: Text)) IN @in_ 
  AND col2 > 3;
```

The tuple structure `(Int, Text, Int, Text, Int, Int, Text)` is inferred from the columns and expressions.

### Combining with Choice Expressions

```sql
SELECT id FROM random_table_1
WHERE 
  @id {
    Another { (id, id2) IN @ids } |
    AndAnother { @x > 2 } |
    Lol { id IN @idss }
  };
```

**OCaml representation:** [Tuple List Parameters](../ocaml/specifics.md#tuple-list-parameters)

---

## `{...}?` — Optional Clauses

Conditional query parts that can be omitted at runtime:

```sql
-- @find_users
SELECT * FROM users
WHERE { age > @min_age }?
  AND { department = @dept }?;
```

- If `@min_age` is provided (`Some`/non-null) → includes `age > ?`
- If `@min_age` is absent (`None`/null) → becomes `TRUE`

The exact representation depends on the target language (e.g. `option` in OCaml, `Option` in Rust, nullable in C#).

### Multiple Optional Conditions

```sql
-- @select_all_test30
SELECT *
FROM test30
LEFT JOIN test31 on test31.c = @test31a
WHERE { c = @choice2 }? OR { r = @choice3 }? OR { c = @choice4 }?
GROUP BY b;
```

Each optional clause can be independently enabled or disabled.

### Complex Optional Clauses

Optional clauses can contain multiple conditions and parameters:

```sql
SELECT * FROM registration_feedbacks WHERE
  id = @id AND
  { 
    user_message = @search 
    OR user_message = @search 
    OR user_message_2 = @search2 
    OR user_message_2 IN @xs
    OR @xss { A { user_message_2 = @a } | B { user_message_2 = @b } }
  }?;
```

This combines optional clauses with list parameters (`IN @xs`) and choice expressions (`@xss`).

### Combining Optional with Tuple Lists

```sql
SELECT id FROM random_table_1
WHERE 
  @id {
    Another { (id, id2) IN @ids } |
    AndAnother { @x > 2 } |
    Lol { id IN @idss }
  } 
  OR { (id, id2) IN @ids2 }?;
```

**OCaml representation:** [Optional Parameters](../ocaml/specifics.md#option-actions-in-ocaml)

---

## `{...}??` — Optional with DEFAULT

For INSERT/UPDATE statements with DEFAULT column values:

```sql
CREATE TABLE tbl (
    id INTEGER PRIMARY KEY,
    value TEXT DEFAULT 'default'
);

-- @insert
INSERT INTO tbl SET value = { CONCAT(@prefix, 'suffix') }??;
```

- If `@prefix` is `Some` → uses CONCAT expression
- If `@prefix` is `None` → uses column's DEFAULT value

### With Choice Inside Optional Default

```sql
INSERT INTO registration_feedbacks
SET
  user_message = { CONCAT(@user_message, '22222') }??,
  grant_types = { @grant_types { A {'2'} | B {'2'} } }??;
```

The `??` syntax works with complex expressions including choice expressions inside.

**OCaml representation:** [Optional Parameters](../ocaml/specifics.md#option-actions-in-ocaml)

---

## Expression Combinations

These expressions can be combined for powerful dynamic queries:

### Choice + Optional + List

```sql
SELECT id FROM table1
WHERE 
  @filter {
    ByIds { id IN @ids } |
    ByName { name LIKE @pattern } |
    All { 1 = 1 }
  } 
  OR { status IN @statuses }?;
```

### Reusable Queries with Expressions

Expressions work with [reusable queries (CTEs)](./cte.md):

```sql
-- @base_query | include: reuse
SELECT * FROM products
WHERE @param { None { TRUE } | Some { FALSE } };

-- @extended
WITH base AS &base_query
SELECT * FROM base WHERE price > @min_price;
```

---

**See also:** [OCaml Expression Representations](../ocaml/specifics.md) for complete type mapping examples.
