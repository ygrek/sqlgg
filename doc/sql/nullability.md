---
sidebar_position: 5
title: Nullability
description: Nullability inference rules in sqlgg
---

# Nullability Inference

sqlgg tracks nullability through expressions, JOINs, subqueries, and aggregates.

## Quick Reference

| Context | Rule |
|---------|------|
| `PRIMARY KEY` / `NOT NULL` | strict |
| `NULL` or no constraint | nullable |
| Any nullable operand | result nullable |
| `COALESCE` / `IFNULL` | removes nullability |
| `CASE` without `ELSE` | nullable |
| LEFT JOIN right side | nullable |
| RIGHT JOIN left side | nullable |
| FULL OUTER JOIN | both sides nullable |
| Scalar subquery | nullable (can return no rows) |
| `COUNT(*)` | always strict |

## Expressions

"Nullable wins" — any nullable operand makes the result nullable:

```sql
SELECT 
    name,                       -- strict (NOT NULL column)
    email,                      -- nullable
    CONCAT(name, email),        -- nullable (email is nullable)
    COALESCE(email, 'default')  -- strict (COALESCE removes nullability)
FROM users;
```

## JOINs

### INNER JOIN

Preserves original nullability:

```sql
SELECT u.name, p.bio
FROM users u
INNER JOIN profiles p ON u.id = p.user_id;
-- u.name: strict, p.bio: strict (if defined as NOT NULL)
```

### LEFT JOIN

Right side becomes nullable:

```sql
SELECT u.name, p.bio
FROM users u
LEFT JOIN profiles p ON u.id = p.user_id;
-- u.name: strict
-- p.bio: nullable (even if column is NOT NULL)
```

### RIGHT JOIN

Left side becomes nullable:

```sql
SELECT u.name, p.bio
FROM users u
RIGHT JOIN profiles p ON u.id = p.user_id;
-- u.name: nullable
-- p.bio: strict
```

### FULL OUTER JOIN

Both sides nullable:

```sql
SELECT u.name, p.bio
FROM users u
FULL OUTER JOIN profiles p ON u.id = p.user_id;
-- u.name: nullable
-- p.bio: nullable
```

## Subqueries

Scalar subqueries are nullable (can return no rows):

```sql
SELECT 
    name,
    (SELECT SUM(amount) FROM orders WHERE user_id = u.id) as total
FROM users u;
-- total: nullable
```

Exception — `COUNT` is always strict:

```sql
SELECT 
    name,
    (SELECT COUNT(*) FROM orders WHERE user_id = u.id) as order_count
FROM users u;
-- order_count: strict (COUNT never returns NULL)
```

## Aggregates

Without `GROUP BY` — most aggregates nullable (empty table returns NULL):

```sql
SELECT SUM(amount), AVG(amount), COUNT(*) FROM orders;
-- SUM: nullable, AVG: nullable, COUNT: strict
```

With `GROUP BY` — strict inputs stay strict:

```sql
SELECT region, SUM(amount), COUNT(*)
FROM orders
GROUP BY region;
-- region: strict (if NOT NULL)
-- SUM: strict (each group has rows)
-- COUNT: strict
```

Nullable inputs stay nullable:

```sql
SELECT region, SUM(bonus) FROM orders GROUP BY region;
-- SUM(bonus): nullable (if bonus column is nullable)
```

See [Aggregation & Nullability](./aggregation-nullability.md) for detailed rules.

**OCaml representation:** [Nullability → option](../ocaml/specifics.md#nullability-in-ocaml)

