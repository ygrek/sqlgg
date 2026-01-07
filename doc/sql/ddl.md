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

## Metadata

Metadata (`-- [sqlgg] key=value`) can be attached to columns and propagates through DDL/DML/DQL. See [Metadata](./metadata.md) for details.

## Migration Strategy Recommendations

For maintaining type safety across database schema evolution:

### Base Schema Creation
It's recommended to use `CREATE TABLE IF NOT EXISTS` for your base tables to avoid duplication errors and maintain a single source of truth. This prevents maintaining separate schemas for sqlgg and your actual database, and eliminates errors from forgetting to add tables in different places:

```sql
-- @create_users
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE
);

-- @create_posts  
CREATE TABLE IF NOT EXISTS posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    content TEXT,
    author_id INTEGER NOT NULL,
    FOREIGN KEY (author_id) REFERENCES users(id)
);
```

### Schema Evolution
Additional migrations can be added manually while maintaining type safety through the established schema. The DDL serves as the "source of truth" for type inference, ensuring your generated code stays synchronized with database changes.

