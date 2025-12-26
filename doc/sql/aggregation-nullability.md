---
sidebar_position: 5
title: Aggregation & Nullability
description: Detailed nullability rules for aggregate and window functions
---

# Aggregation & Window Functions: Nullability

:::info Deep Dive
This page provides detailed implementation notes on nullability inference. For most users, sqlgg handles this automatically — you can skip this section unless you want to understand the internals.
:::

## Overview

This page documents **standard SQL nullability semantics** around aggregates and window functions, and how sqlgg reflects them in inferred result types.

In short: whether an aggregate result can be `NULL` depends on whether the query can end up with **no input rows for the aggregate**, and on whether the aggregate's **input expression is nullable**.

## Aggregate Functions and Nullability

### SQL semantics summary

Aggregation nullability depends on whether the query can produce "no rows" (or "no rows for a given group"):

- **Simple aggregation (no `GROUP BY`)**: aggregate results are often nullable because the input set can be empty
- **`GROUP BY` aggregation**: for each returned group, aggregates over **NOT NULL** inputs can stay strict; aggregates over nullable inputs remain nullable
- **Window functions (`... OVER (...)`)**: the expression is computed per-row, but if an outer query can eliminate all rows, you still need to reason about "no output rows" at the call site (sqlgg will infer correctly for the query you give it)

### Simple aggregation examples

```sql
-- DDL Schema
CREATE TABLE sales (
    id INTEGER PRIMARY KEY,      -- Strict
    amount DECIMAL NOT NULL,     -- Strict
    region TEXT NOT NULL         -- Strict
);

-- Case 1: Simple aggregation (no GROUP BY) - always returns one row, but can be NULL
-- @get_total_sales
SELECT SUM(amount) FROM sales;
```

Result is `decimal option` because `SUM` can return `NULL` if the table is empty.

```sql
-- @get_statistics
SELECT SUM(amount), AVG(amount), MAX(amount), MIN(amount), COUNT(*) FROM sales;
```

**Generated types:**
- `SUM(amount)` → `decimal option` (nullable if table empty)
- `AVG(amount)` → `float option` (nullable if table empty)
- `MAX(amount)` → `decimal option` (nullable if table empty)
- `MIN(amount)` → `decimal option` (nullable if table empty)
- `COUNT(*)` → `int` (strict - `COUNT` never returns NULL)

### GROUP BY aggregation examples

```sql
-- Case 2: GROUP BY aggregation - each returned group has at least one row
-- @get_sales_by_region
SELECT region, SUM(amount), COUNT(*)
FROM sales
GROUP BY region;
```

**Generated types:**
- `region` → `string` (strict - grouping column, guaranteed to exist **if region is NOT NULL**)
- `SUM(amount)` → `decimal` (strict - each group has rows, input is NOT NULL)
- `COUNT(*)` → `int` (strict - `COUNT` never returns NULL)

:::info Important
The above assumes `region` is `NOT NULL`. If `region` were nullable, the types would be different.
:::

```sql
-- Mixed column nullability with GROUP BY
-- @get_sales_with_nullable_cols
SELECT region, SUM(nullable_bonus), MAX(nullable_bonus)
FROM sales_extended
GROUP BY region;
```

If `nullable_bonus` is a nullable column:
- `SUM(nullable_bonus)` → `decimal option` (nullable input stays nullable)
- `MAX(nullable_bonus)` → `decimal option` (nullable input stays nullable)

### COUNT is special

`COUNT` has unique behavior across contexts:

```sql
-- COUNT always returns strict int, regardless of context
SELECT COUNT(*), COUNT(nullable_col), COUNT(1) FROM sales;          -- All: int (strict)
SELECT COUNT(*), COUNT(nullable_col) FROM sales GROUP BY region;    -- All: int (strict)
SELECT COUNT(*) OVER() FROM sales WHERE FALSE;                      -- int (strict)
SELECT COUNT(nullable_col) OVER() FROM sales WHERE FALSE;           -- int (strict)
```

**COUNT rules:**
- `COUNT(*)` → always `int` (strict)
- `COUNT(column)` → always `int` (strict), even if column is nullable
- `COUNT(expression)` → always `int` (strict)

### Function-specific summary (common aggregates)

**In simple aggregation (no `GROUP BY`):**
- `SUM`, `AVG`, `MAX`, `MIN` → nullable (might return NULL if table empty)
- `COUNT` → strict (always returns 0 or positive integer)

**In `GROUP BY` aggregation:**
- `SUM`, `AVG`, `MAX`, `MIN` of strict inputs → strict (each group exists)
- `SUM`, `AVG`, `MAX`, `MIN` of nullable inputs → nullable (inherit input nullability)
- `COUNT` → always strict
- **Grouping columns**: strict if `NOT NULL`, nullable if nullable (SQL `NULL` grouping behavior)

### Detailed GROUP BY examples

```sql
-- DDL with mixed nullability
CREATE TABLE sales_detailed (
    id INTEGER PRIMARY KEY,          -- Strict
    region TEXT NOT NULL,           -- Strict
    optional_region TEXT,           -- Nullable
    amount DECIMAL NOT NULL,        -- Strict
    bonus DECIMAL,                  -- Nullable
    commission DECIMAL              -- Nullable
);

-- @get_sales_stats_by_strict_region
SELECT
    region,                         -- Strict (NOT NULL grouping column)
    SUM(amount) as total_amount,    -- Strict (strict input, GROUP BY context)
    AVG(amount) as avg_amount,      -- Strict (strict input, GROUP BY context)
    SUM(bonus) as total_bonus,      -- Nullable (nullable input)
    COUNT(*) as row_count           -- Strict (COUNT always strict)
FROM sales_detailed
GROUP BY region;                    -- region is NOT NULL

-- @get_sales_stats_by_nullable_region
SELECT
    optional_region,                -- Nullable (nullable grouping column)
    SUM(amount) as total_amount,    -- Strict (strict input, GROUP BY context)
    SUM(bonus) as total_bonus,      -- Nullable (nullable input)
    COUNT(*) as row_count           -- Strict (COUNT always strict)
FROM sales_detailed
GROUP BY optional_region;           -- optional_region is nullable
```

**Generated types for strict region grouping:**
- `region` → `string` (strict - NOT NULL grouping column)
- `total_amount`, `avg_amount` → `decimal` (strict - strict input in GROUP BY)
- `total_bonus` → `decimal option` (nullable - nullable input)
- `row_count` → `int` (strict - COUNT always strict)

**Generated types for nullable region grouping:**
- `optional_region` → `string option` (nullable - nullable grouping column)
- `total_amount` → `decimal` (strict - strict input in GROUP BY)
- `total_bonus` → `decimal option` (nullable - nullable input)
- `row_count` → `int` (strict - COUNT always strict)

### Multiple GROUP BY columns

```sql
-- @get_sales_by_region_and_optional_category
SELECT
    region,           -- Strict (NOT NULL)
    category,         -- Nullable (nullable column)
    SUM(amount)       -- Strict (strict input)
FROM sales_detailed
GROUP BY region, category;  -- Mixed nullability in GROUP BY
```

**Generated types:**
- `region` → `string` (strict)
- `category` → `string option` (nullable)
- `SUM(amount)` → `decimal` (strict)

### GROUP BY with NULL values behavior

```sql
-- SQL groups NULL values together in GROUP BY
-- This query will have one group for each distinct non-null value of optional_region,
-- plus one group for all NULL values (if any exist)

SELECT optional_region, COUNT(*)
FROM sales_detailed
GROUP BY optional_region;
```

The nullable grouping column can return:
- Multiple rows with distinct non-NULL values
- One row with NULL value (representing all rows where `optional_region IS NULL`)
- No row with NULL if no NULL values exist in the data

### Mixed aggregation example

```sql
-- Combining strict and nullable columns in expressions
-- @get_complex_stats
SELECT
    region,
    SUM(amount + COALESCE(bonus, 0)) as total_with_bonus,    -- Strict (COALESCE makes it strict)
    SUM(amount + bonus) as total_with_nullable_bonus,        -- Nullable (arithmetic with nullable)
    AVG(CASE WHEN bonus IS NOT NULL THEN amount END) as avg_when_bonus -- Nullable (CASE without ELSE)
FROM sales_detailed
GROUP BY region;
```

**Generated types:**
- `total_with_bonus` → `decimal` (strict - COALESCE removes nullability)
- `total_with_nullable_bonus` → `decimal option` (nullable - arithmetic with nullable column)
- `avg_when_bonus` → `decimal option` (nullable - CASE without ELSE, plus AVG)

## Window Functions and Nullability

In practice, window aggregates follow the same "aggregate vs input" intuition, but you must also consider that an **outer query can filter away all rows** (so a call site might see "no output rows").

**Rule of thumb:**
- `SUM`, `AVG`, `MAX`, `MIN` → often nullable
- `COUNT` → strict (exception)
