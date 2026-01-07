---
sidebar_position: 6
title: Literals
description: SQL literal type-checking and validation in sqlgg
---

# Literals

This page describes how sqlgg type-checks and validates SQL literals.

## Basic literals

- **Number (integer)**: `123`, `0`, `42`
  - Parsed as an integer literal.
- **Number (float)**: `1.5`, `0.01`, `10e-3`
  - Parsed as a floating literal.
- **Boolean**: `TRUE`, `FALSE`
- **String**: `'text'`
  - (In the parser this is the `TEXT` token — an SQL string literal.)
- **NULL**: `NULL`

## "Strict" literals (context-driven validation)

### Enum / Union (constructor validation)

In sqlgg, an enum/union type comes from:
- DDL `ENUM('a','b','c')`, or
- inferred unions of string literals in expressions.

An **enum literal is a string literal** used where an enum/union is expected. In this case sqlgg verifies that the value belongs to the set of allowed constructors.

Example (DDL):

```sql
CREATE TABLE t(
  status ENUM('pending','sent') NOT NULL
);

-- ✅ ok
SELECT * FROM t WHERE status = 'pending';

-- ❌ type error (literal is not a valid enum constructor)
SELECT * FROM t WHERE status = 'oops';
```

### JSON (JSON validity check)

When the context expects type `JSON` (e.g. a `JSON` column or a JSON-function argument), a **string literal can be treated as JSON**, and sqlgg validates that the string is valid JSON.

Examples:

```sql
-- ✅ valid JSON (object)
SELECT CAST('{"a": 1, "b": true}' AS JSON);

-- ❌ invalid JSON (sqlgg validation error): JSON syntax is broken (missing closing '}')
SELECT CAST('{"a": 1' AS JSON);
```

See [JSON in OCaml](../ocaml/json.md) for `Yojson.Safe.t` usage.

### Json_path (path validity check)

`Json_path` is a special type for JSON path strings used by JSON functions/operators.
It is also a **string literal**, but additionally validated as a valid path.

Example:

```sql
-- ✅ valid path
SELECT JSON_EXTRACT(CAST('{"a": 1}' AS JSON), '$.a');

-- ❌ invalid path (sqlgg validation error)
SELECT JSON_EXTRACT(CAST('{"a": 1}' AS JSON), '$..[');
```

### One_or_all (only "one" or "all")

`One_or_all` is a string literal with a strict allowed set: **only** `"one"` or `"all"` (case-insensitive).
It is used, for example, in functions like `JSON_CONTAINS_PATH(...)`.

Example:

```sql
-- ✅ ok
SELECT JSON_CONTAINS_PATH(CAST('{"a": 1}' AS JSON), 'one', '$.a');

-- ❌ error: must be one/all
SELECT JSON_CONTAINS_PATH(CAST('{"a": 1}' AS JSON), 'any', '$.a');
```

### Decimal (fits-in-DECIMAL(p,s) validation)

The `DECIMAL(p,s)` type is defined via DDL/CAST, and numeric literals (integer/float) are validated **by context**: if `DECIMAL(p,s)` is expected, sqlgg checks that the value fits the allowed range for that precision/scale.

Example:

```sql
-- ✅ ok
SELECT CAST(12.34 AS DECIMAL(5,2));

-- ❌ error: value does not fit into DECIMAL(5,2)
SELECT CAST(123456.78 AS DECIMAL(5,2));
```

