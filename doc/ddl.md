---
sidebar_position: 1
title: DDL - Data Definition Language
description: Schema definition and type inference in sqlgg
---

# DDL - Data Definition Language

## Overview

DDL (Data Definition Language) statements in sqlgg serve a dual purpose:

1. **Schema Definition**: The DDL statements define the database schema that sqlgg uses for **type inference**. When you write DML/DQL queries later, sqlgg analyzes these table definitions to automatically determine parameter types and result column types.

2. **Migration Strategy**: It's recommended (though not required) to structure your DDL migrations so that:
   - Base table creation uses `CREATE TABLE IF NOT EXISTS` 
   - Tables are created in dependency order
   - Additional up/down migrations can be added manually while maintaining type safety through the established schema

This approach ensures that your database schema and generated code remain synchronized and type-safe throughout the development lifecycle.

## Basic CREATE TABLE Syntax

```sql
-- @create_table_name
CREATE TABLE table_name (
    column1 TYPE [constraints],
    column2 TYPE [constraints],
    ...
    [table_constraints]
);
```

:::warning Important
sqlgg processes DDL statements sequentially. Tables must be declared in dependency order - referenced tables (like `users`) must appear before tables that reference them (like `posts` with foreign keys to `users`):

1. Independent tables first (users, categories)
2. Dependent tables second (posts, comments)
3. Junction tables last (post_tags, user_roles)
:::

This applies whether you use separate files:
```bash
sqlgg -gen none ddl.sql -gen cxx dml.sql
```

Or combine everything in a single file:
```bash
sqlgg single.sql
```

### Function Naming

You can explicitly name the generated function using the `-- @create_table_name` comment annotation.

Alternatively, you can omit the annotation and sqlgg will auto-generate a function name based on the SQL statement.

## Data Types and Constraints

The data types and constraints you specify in DDL statements are used by sqlgg for **type inference** in your DML/DQL queries. Column types and constraints directly determine parameter and return types in generated functions.

### Nullability

sqlgg automatically determines nullability based on column constraints (nullability inference for joins and subqueries is covered in DML/DQL sections):

```sql
-- @create_users_with_nullability
CREATE TABLE users (
    id INTEGER PRIMARY KEY,          -- Strict (NOT NULL due to PRIMARY KEY)
    name TEXT NOT NULL,              -- Strict (explicit NOT NULL)
    email TEXT,                      -- Nullable (no constraint specified)
    phone TEXT NULL                  -- Nullable (explicit NULL allowed)
);
```

sqlgg infers nullability from `PRIMARY KEY` and `NOT NULL` (strict) vs `NULL` or no constraint (nullable). Other constraints (UNIQUE, CHECK, FOREIGN KEY, DEFAULT, AUTOINCREMENT) are parsed but don't affect generated types.

## ALTER TABLE

Queries written after an `ALTER TABLE` see the new types:

```sql
ALTER TABLE users ADD COLUMN age INT NOT NULL;
ALTER TABLE users MODIFY COLUMN name VARCHAR(500) NOT NULL;
ALTER TABLE users RENAME COLUMN name TO full_name;
```

Dialect-specific forms such as PostgreSQL `ALTER COLUMN ... TYPE` or TiDB `TTL` are checked against [`-dialect`](./dialects.md).

## CREATE TYPE (PostgreSQL)

User-defined enum types are supported with `-dialect postgresql` and can be used as column types:

```sql
CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');

CREATE TABLE person (
    id INT PRIMARY KEY,
    current_mood mood NOT NULL
);

DROP TYPE mood;
```

Enum types get the same treatment as inline `ENUM(...)` columns. See [Literals](./literals.md) for enum literal validation and the OCaml mapping to polymorphic variants.

## Metadata

Metadata (`-- [sqlgg] key=value`) can be attached to columns and propagates through DDL/DML/DQL. See [Metadata](./metadata.md) for details.

## Keeping the schema in sync

The same DDL files can be run against the database and fed to sqlgg, so there is one schema instead of two. Write them with `CREATE TABLE IF NOT EXISTS` and running them again is harmless:

```sql
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE
);
```

sqlgg can also generate migrations by diffing schemas, see [Migrations](../migrations.md).

