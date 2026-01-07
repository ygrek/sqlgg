---
sidebar_position: 0
title: Dialects
description: SQL dialect support and feature validation
---

# SQL Dialects

sqlgg supports multiple SQL dialects and validates that your queries use only features available in the selected dialect.

## Supported Dialects

| Dialect | Flag | Parameter Style |
|---------|------|-----------------|
| MySQL | `-dialect mysql` | `?` (unnamed) |
| PostgreSQL | `-dialect postgresql` | `$1, $2, ...` |
| SQLite | `-dialect sqlite` | `?` (unnamed) |
| TiDB | `-dialect tidb` | `?` (unnamed) |

## Usage

```bash
sqlgg -dialect mysql queries.sql
sqlgg -dialect postgresql queries.sql
sqlgg -dialect sqlite queries.sql
```

## How It Works

sqlgg analyzes SQL syntax and detects dialect-specific features. When you specify a dialect with `-dialect`, sqlgg validates that your queries only use features supported by that dialect.

For example:
- **COLLATE** syntax differs between MySQL and SQLite
- **WHERE alias** referencing (using SELECT aliases in WHERE) is allowed in some dialects but not others
- **CREATE TABLE AS SELECT** has different support levels
- **JOIN ON subquery** syntax varies

If a feature isn't supported by the selected dialect, sqlgg reports an error with the location and which dialects do support it.

## Disabling Checks

To skip specific feature checks:

```bash
# Skip single feature check
sqlgg -dialect sqlite -no-check=collation queries.sql

# Skip multiple feature checks
sqlgg -dialect sqlite -no-check=collation,join_on_subquery queries.sql

# Skip all dialect checks
sqlgg -dialect sqlite -no-check=all queries.sql
```

Available features to skip:
- `collation` — COLLATE syntax differences
- `join_on_subquery` — subqueries in JOIN ON clause
- `create_table_as_select` — CREATE TABLE AS SELECT syntax

## Default Behavior

Without `-dialect`, sqlgg accepts all SQL syntax without dialect-specific validation. This is useful when you want maximum flexibility or are targeting multiple databases.

