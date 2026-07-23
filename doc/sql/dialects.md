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

Name the feature to skip its check, or several separated by commas:

```bash
sqlgg -dialect sqlite -no-check=collation queries.sql
sqlgg -dialect sqlite -no-check=collation,join_on_subquery queries.sql
```

`-no-check=all` is a shorthand for the syntax-level checks. The [`-no-check` option](../cli.md#all-options) spells out which ones those are and lists every feature name sqlgg knows.

## Default Behavior

Without `-dialect`, sqlgg accepts all SQL syntax without dialect-specific validation. This is useful when you want maximum flexibility or are targeting multiple databases.

