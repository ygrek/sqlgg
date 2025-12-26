---
sidebar_position: 3
title: CTE (WITH ...)
description: Common Table Expressions support in sqlgg
---

# CTE (WITH ...): Common Table Expressions

**sqlgg** fully supports CTEs (`WITH ...`) and correctly infers parameter and result types through CTE layers.

## Basic CTE

This example demonstrates a single CTE used as a named, reusable subquery, then joined against a base table and aggregated.

### Schema

```sql
CREATE TABLE users (
  id BIGINT PRIMARY KEY,
  email TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL
);

CREATE TABLE orders (
  id BIGINT PRIMARY KEY,
  user_id BIGINT NOT NULL,
  total_amount DECIMAL(18,2) NOT NULL,
  status VARCHAR(255) NOT NULL,
  created_at TIMESTAMP NOT NULL
);
```

### Query

```sql
-- @active_users_last_30_days
WITH recent_orders AS (
  SELECT
    o.id,
    o.user_id,
    o.total_amount,
    o.created_at
  FROM orders o
  WHERE
    o.status = 'paid'
    AND o.created_at >= CURRENT_DATE - INTERVAL 30 DAY
)
SELECT
  u.id AS user_id,
  u.email,
  COUNT(ro.id) AS orders_count,
  SUM(ro.total_amount) AS total_spent
FROM users u
JOIN recent_orders ro
  ON ro.user_id = u.id
GROUP BY
  u.id,
  u.email;
```

## sqlgg Expressions Inside CTEs

All sqlgg query expression features work **inside** CTEs:

- **`@param`** — regular parameters
- **`@choice`** — conditional branches
- **`IN @list`** — list parameters
- **`(a, b, ...) IN @tuple_list`** — tuple-list parameters
- **`{expr}?`** — optional query parts

```sql
-- @filtered_orders
WITH recent_orders AS (
  SELECT
    o.id,
    o.user_id,
    o.total_amount,
    o.created_at,
    (o.total_amount * @fx_rate) AS total_amount_converted,
    CASE
      WHEN o.total_amount >= @big_order_threshold THEN 'big'
      ELSE 'normal'
    END AS order_size
  FROM orders o
  WHERE
    @choice {
      Paid { o.status = 'paid' } |
      Status { o.status = @status } |
      Any { 1 = 1 }
    }
    AND o.created_at >= CURRENT_DATE - INTERVAL 30 DAY
    AND o.user_id IN @user_ids
    AND (o.user_id, o.status) IN @user_status_pairs
    AND { o.total_amount >= @min_total_amount }?
    AND { o.created_at >= @created_since }?
)
SELECT
  u.id AS user_id,
  u.email,
  COUNT(ro.id) AS orders_count,
  SUM(ro.total_amount) AS total_spent
FROM users u
JOIN recent_orders ro
  ON ro.user_id = u.id
GROUP BY
  u.id,
  u.email;
```

## Multiple CTEs

You can define several CTEs separated by commas:

```sql
-- @users_with_stats
WITH 
  active_users AS (
    SELECT id, email FROM users WHERE created_at >= CURRENT_DATE - INTERVAL 90 DAY
  ),
  user_orders AS (
    SELECT user_id, COUNT(*) AS order_count, SUM(total_amount) AS total_spent
    FROM orders
    WHERE status = 'paid'
    GROUP BY user_id
  )
SELECT
  au.id,
  au.email,
  COALESCE(uo.order_count, 0) AS order_count,
  COALESCE(uo.total_spent, 0) AS total_spent
FROM active_users au
LEFT JOIN user_orders uo ON uo.user_id = au.id;
```

## Recursive CTE

**sqlgg** supports recursive CTEs (`WITH RECURSIVE`). This example generates a calendar (one row per day), expands events across date ranges, and aggregates daily event titles.

### Schema

```sql
CREATE TABLE events (
  id BIGINT PRIMARY KEY,
  title TEXT NOT NULL,
  starts_at DATE NOT NULL,
  ends_at DATE NOT NULL
);
```

### Query

```sql
-- @calendar_with_events
WITH RECURSIVE calendar AS (
  SELECT DATE('2026-01-01') AS day_
  UNION ALL
  SELECT day_ + INTERVAL 1 DAY
  FROM calendar
  WHERE day_ < DATE('2026-12-31')
),
events_expanded AS (
  SELECT
    c.day_,
    e.id AS event_id,
    e.title
  FROM calendar c
  LEFT JOIN events e
    ON c.day_ BETWEEN e.starts_at AND e.ends_at
)
SELECT
  day_,
  JSON_ARRAYAGG(
    CASE WHEN title IS NOT NULL THEN title END
    ORDER BY title
  ) AS events
FROM events_expanded
GROUP BY day_
ORDER BY day_;
```

:::note
We use `LEFT JOIN` in the intermediate CTE to keep days without events. Titles are aggregated using `JSON_ARRAYAGG(... ORDER BY ...)`.
:::

## Reusable Queries

sqlgg allows you to define queries once and reuse them across multiple statements using the `&name` syntax.

### Defining a Reusable Query

Add `| include: reuse` to the query annotation:

```sql
-- @base_query | include: reuse
SELECT 
    p.id,
    p.name,
    p.price
FROM products p
WHERE p.price > @min_price;
```

The `include: reuse` flag tells sqlgg that this query can be referenced by other queries.

### Using a Reusable Query

Reference the query with `&name` in a CTE:

```sql
-- @extended_query
WITH base AS &base_query
SELECT 
    base.*,
    c.name AS category_name
FROM base
JOIN categories c ON base.category_id = c.id
WHERE base.price < @max_price;
```

The `&base_query` syntax inlines the reusable query as a CTE. Parameters from the referenced query (`@min_price`) are combined with local parameters (`@max_price`).

### `include: reuse_and_execute`

By default, `include: reuse` means the query is **only** available for reuse — it won't generate a standalone function. Use `include: reuse_and_execute` if you want both:

```sql
-- @products_over_100 | include: reuse_and_execute
WITH inner_cte AS (
    SELECT id, name, category_id 
    FROM products 
    WHERE price > 100
)
SELECT inner_cte.*, c.name AS category_name
FROM inner_cte
JOIN categories c ON inner_cte.category_id = c.id;

-- @reuse_it
WITH data AS &products_over_100
SELECT * FROM data WHERE category_name LIKE @pattern;
```

- `products_over_100` — generates a standalone function **and** can be reused
- `reuse_it` — uses `products_over_100` as a CTE and adds filtering

### Complex Example

```sql
-- @abcd | include: reuse
SELECT 
    1 as y, 
    4 + @five as y1
FROM ( 
    SELECT 1 AS id, 'l' AS creator_name
    UNION ALL
    SELECT 2, 'k'
) AS x 
WHERE @param { None { TRUE } | Some { FALSE } };

-- @test2 
WITH x AS &abcd
SELECT 1 + @test - @test2 + @test5 + x.y1 as y2
FROM x;
```

Here `@abcd` uses a `@choice` expression. When reused in `@test2`, the choice parameter (`@param`) is included along with the new parameters (`@test`, `@test2`, `@test5`).

### Benefits

- **DRY**: Define complex queries once, reuse everywhere
- **Type safety**: Parameters are properly typed across all usages
- **Composability**: Build complex queries from simpler building blocks

