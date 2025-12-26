---
sidebar_position: 3
title: Parameters
description: Parameter syntax and type inference in sqlgg
---

# Parameters

## `@param` Syntax

sqlgg uses `@param` as the universal parameter syntax, translated to the appropriate format for your target database.

```sql
SELECT * FROM users WHERE id = @user_id AND status = @status;
```

## Dialect Output

| Dialect | Input | Output |
|---------|-------|--------|
| mysql/sqlite | `@param` | `?` |
| postgresql | `@param` | `$1, $2, ...` |
| oracle | `@param` | `:param` |
| named | `@param` | `@param` |

## Configuration

```bash
sqlgg -dialect postgresql queries.sql
sqlgg -params named queries.sql
```

## Type Inference

Parameters are typed based on usage context:

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER,
    salary REAL
);

-- @find_user
SELECT * FROM users WHERE id = @user_id;
-- @user_id: Int (from id column)

-- @find_by_age
SELECT * FROM users WHERE age > @min_age;
-- @min_age: Int (from age column)

-- @find_by_salary
SELECT * FROM users WHERE salary > @min_salary + 1000;
-- @min_salary: Float (from salary column in arithmetic)
```

### In UPDATE

```sql
-- @update_user
UPDATE users 
SET age = age + @increment,
    name = CONCAT(@prefix, name),
    salary = @new_salary
WHERE id = @user_id;
```

- `@increment`: Int (arithmetic with age)
- `@prefix`: Text (CONCAT with name)
- `@new_salary`: Float (assignment to salary)
- `@user_id`: Int (comparison with id)

### In INSERT

```sql
-- @insert_user
INSERT INTO users (name, email, age) VALUES (@name, @email, @age);
```

Types inferred from target columns.

```sql
-- @insert_users_list
INSERT INTO users (name, email, age) VALUES @values;
```

`@values`: `(string * string option * int option) list`

## Explicit Type Annotations

Override inference with `::` syntax:

```sql
@param :: TYPE [NULL]

-- Examples
@x :: TEXT NULL      -- nullable string
@y :: INT            -- strict int
@z :: FLOAT NULL     -- nullable float
```

Useful when:
- Need nullable parameters
- Want different precision
- Context is ambiguous

```sql
-- @search
SELECT * FROM users 
WHERE age > @min_age :: INT
  AND name LIKE @pattern :: TEXT NULL
  AND salary < @max_salary :: FLOAT;
```

Types are validated against context:

```sql
-- ✓ Valid
@param :: INT > 5
@param :: TEXT LIKE '%x%'

-- ✗ Invalid
@param :: TEXT - 2        -- TEXT can't do arithmetic
@param :: INT LIKE '%x%'  -- INT can't use LIKE
```

