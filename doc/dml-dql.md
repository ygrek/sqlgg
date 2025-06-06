# DML/DQL - Data Manipulation & Query Language

[← Back to Main](./index.md) | [← Previous: DDL](./ddl.md)

---

## Table of Contents

- [SELECT Queries](#select-queries)
- [Query Expression Kinds](#query-expression-kinds)
  - [Basic Parameters (`@param`)](#basic-parameters)
  - [Choice Expressions (`@choice`)](#choice-expressions) 
  - [IN/NOT IN Parameters](#innot-in-parameters)
  - [IN Tuple List Parameters](#in-tuple-list-parameters)
  - [Option Actions (`{expr}?`)](#option-actions)
- [Nullability Inference](#nullability-inference)
  - [Basic Nullability Rules](#basic-nullability-rules)
  - [Aggregate Functions and Nullability](#aggregate-functions-and-nullability)
  - [JOIN Nullability](#join-nullability)
  - [Subquery Nullability](#subquery-nullability)
  - [CASE Expression Nullability](#case-expression-nullability)
  - [Window Functions and Nullability](#window-functions-and-nullability)
  - [Advanced Nullability Cases](#advanced-nullability-cases)

---

## Overview

DML (Data Manipulation Language) and DQL (Data Query Language) statements in sqlgg generate typed functions for database operations. sqlgg analyzes your queries against the schema defined in DDL statements to automatically infer:

1. **Parameter types** - Based on usage context (WHERE, SELECT, INSERT, UPDATE, JOIN, etc.)
2. **Return types** - Based on SELECT column lists and their nullability
3. **Function signatures** - Complete function types for type-safe database access

**Note:** Examples show OCaml function signatures, though sqlgg can generate code for other target languages.

## Basic Query Syntax

All DML/DQL statements can be annotated with function names:

```sql
-- @function_name
SELECT/INSERT/UPDATE/DELETE statement
```

Function names are auto-generated if no annotation is provided.

## SELECT Queries

SELECT statements generate functions that return query results with proper typing based on your schema.

### Basic SELECT

```sql
-- DDL Schema
CREATE TABLE users (
    id INTEGER PRIMARY KEY,      -- int, strict
    name TEXT NOT NULL,          -- string, strict
    email TEXT,                  -- string, nullable
    age INTEGER                  -- int, nullable
);

-- @get_all_users
SELECT name, email, age FROM users;
```

**Inferred schema:**
 - `name: Text (strict, NotNull)`
 - `email: Text (nullable)`
 - `age: Int (nullable)`

Here, for now, an example of which types will be nested is not given on purpose because it depends on the chosen language and also the implementation of traits, more on that in another section

## UPDATE Queries

UPDATE statements generate functions that modify table data with type-safe parameter inference.

```sql
-- @update_user_email
UPDATE users SET email = @new_email WHERE id = @user_id;
```

Parameter types are automatically inferred as described in [Basic Parameters](#basic-parameters) below.

## INSERT Queries

INSERT statements generate functions for adding new data with proper type checking.

```sql
-- @create_user
INSERT INTO users (name, email, age) VALUES (@name, @email, @age);
```

Parameter types are automatically inferred as described in [Basic Parameters](#basic-parameters) below.

## Query Expression Kinds

sqlgg supports various expression types in SQL queries, each with specific type inference rules.

### Basic Parameters

#### Parameter Syntax Overview

sqlgg uses `@param` as the universal parameter syntax in SQL queries, which gets translated to the appropriate format based on your target database dialect.

#### Universal @param Syntax

In sqlgg queries, always use `@param` regardless of target database:

```sql
-- Universal syntax - works everywhere
SELECT @param + 1;
SELECT * FROM users WHERE @param = name;
SELECT * FROM users WHERE id = @user_id AND status = @status;
```

#### Dialect-Specific Output

sqlgg translates `@param` to the appropriate format based on dialect settings:

| Dialect          | sqlgg Input | Generated Output             |
| ---------------- | ----------- | ---------------------------- |
| **mysql/sqlite** | `@param`    | `?` (unnamed)                |
| **postgresql**   | `@param`    | `$1, $2, $3...` (positional) |
| **oracle**       | `@param`    | `:param` (named)             |
| **named mode**   | `@param`    | `@param` (named)             |

#### Configuration

Control parameter format via command line:

```bash
# Auto-detect from dialect (default)
sqlgg -dialect postgresql queries.sql

# Override parameter format explicitly  
sqlgg -params named queries.sql
sqlgg -params unnamed queries.sql
sqlgg -params oracle queries.sql
sqlgg -params postgresql queries.sql
```

The parameter substitution happens during code generation - your SQL source always uses the consistent `@param` syntax.

#### Parameter Type Inference

sqlgg automatically infers parameter types based on the context where they're used:

**WHERE Clause Type Inference:**

```sql
-- DDL Schema
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER,
    salary REAL
);

-- @find_user_by_id
SELECT * FROM users WHERE id = @user_id;
-- @user_id inferred as: int (matches id column type)

-- @find_users_by_age_range  
SELECT * FROM users WHERE age > @min_age;
-- @min_age inferred as: int (matches age column type)

-- @complex_condition
SELECT * FROM users WHERE salary > @min_salary + 1000;
-- @min_salary inferred as: float (matches salary column type in arithmetic)
```

**Inferred parameter types (sqlgg internal):**
- `@user_id`: `Int` (matches id column type)
- `@min_age`: `Int` (matches age column type)
- `@min_salary`: `Float` (matches salary column type in arithmetic)

*Note: At the moment we still use internal types of sqlgg, that is, which are equal to types in sql, because the generated types will depend on the chosen implementation (or custom implementation) of [traits](href), which will be discussed in the next part of the documentation.*

But for better understanding let's assume that we are using the built-in implementation for [mariadb](href), with the ocaml language, then as a result the function will expect the following parameters:

 - `user_id: int`
 - `min_age: int64`
 - `min_salary: float`

**UPDATE SET Clause Type Inference:**

```sql
-- @update_user_data
UPDATE users 
    SET age = age + @age_increment,
        name = CONCAT(@name_prefix, name),
        salary = @new_salary
    WHERE id = @user_id;
```

**Inferred parameter types:**
- `@age_increment`: `Int` (matches age column in arithmetic)
- `@name_prefix`: `Text` (matches name column in CONCAT)  
- `@new_salary`: `Float` (matches salary column assignment)
- `@user_id`: `Int` (matches id column in WHERE condition)

**INSERT VALUES Clause Type Inference:**

```sql
-- @insert_multiple_users  
INSERT INTO users (name, email, age, salary) 
VALUES (@name1, @email1, @age1, @salary1),
       (@name2, @email2, @age2, @salary2);
```

```sql
-- @insert_users_list
INSERT INTO users (name, email, age, salary) VALUES @values;
```

- Parameter `@values` is inferred as `(string * string option * int option * float option) list`

#### Explicit Type Annotations

You can explicitly specify parameter types using the `::` syntax:

```sql
-- Explicit type specification
@param :: TYPE [NULL]

-- Examples:
@x :: TEXT NULL      -- nullable string parameter
@y :: INT            -- strict int parameter (NOT NULL by default)
@z :: FLOAT NULL     -- nullable float parameter
```

**Explicit typing is useful when:**
- You need nullable parameters: `@param :: TEXT NULL`
- You want different precision: `@param :: FLOAT NULL > 2` (float instead of int)
- Type inference context is ambiguous

```sql
-- @search_with_explicit_types
SELECT * FROM users 
WHERE age > @min_age :: INT
  AND name LIKE @pattern :: TEXT NULL
  AND salary < @max_salary :: FLOAT;
```

**Type validation:**
```sql
-- ✓ Valid: types match context
@param :: INT > 5         -- INT works with arithmetic comparison
@param :: TEXT LIKE '%x%' -- TEXT works with LIKE operator

-- ✗ Invalid: type mismatch  
@param :: TEXT - 2        -- TEXT cannot be used with arithmetic (-) operator
@param :: INT LIKE '%x%'  -- INT cannot be used with LIKE operator
```

The explicitly specified types are validated against the query context and override automatic inference.

### Choice Expressions

Choice expressions provide pattern matching-like conditional logic in SQL queries using the `@choice` syntax.

```bnf
choice_expr := @param '{' choice ('|' choice)* '}'
choice := IDENT ('{' expr '}')?
```

#### Basic Choice Syntax

```sql
@choice { A { 1 } | B { @b + 2 } | E { @x - 2 } }
```

This can be read as pattern matching:
```ocaml
| A -> 1
| B b -> b + 2  
| E x -> x - 2
```

Where:
- `A`, `B`, `E` are **constructors** (choice variants)
- `@b`, `@x` are **scoped parameters** available only within their respective branches
- All branches must return the **same type** (like pattern matching)

#### Generated Types

For the choice expression above, sqlgg generates a polymorphic variant type:*

```ocaml
[`A | `B of int | `E of int]
```
*\*OCaml example - types vary by target language and traits implementation*

The parameter types are inferred from the scoped parameters within each branch:
- `A` constructor takes no parameters → `A`
- `B` constructor uses `@b` parameter → `B of int` (assuming `@b` is inferred as int)
- `E` constructor uses `@x` parameter → `E of int` (assuming `@x` is inferred as int)

#### Choice in WHERE Clauses

```sql
-- DDL Schema
CREATE TABLE products (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    price REAL,
    category TEXT
);

-- @search_products
SELECT * FROM products 
WHERE price > @choice { 
    Cheap { 10.0 } | 
    Expensive { @min_price } |
    Free { 0.0 }
};
```

Taking OCaml as an example, the generated function signature would be:

```ocaml
search_products: [> `RO ] T.connection ->
                 choice: [< `Cheap | `Expensive of T.Types.Float.t | `Free ] ->
                 (id:T.num -> name:T.text -> price:T.Types.Float.t option -> category:T.text option -> 'c) ->
                 'c list IO.future
```

Where:
- `[> `RO ] T.connection` - database connection type ([covered in traits](./traits.md))
- `choice: [< `Cheap | `Expensive of T.Types.Float.t | `Free ]` - the choice parameter with our polymorphic variants
- `T.Types.Float.t` - typed wrapper for SQL REAL type (why not plain `float` will be explained in [traits](./traits.md))
- `id:T.num -> name:T.text -> ...` - column accessor functions (column handling [covered in traits](./traits.md))
- `IO.future` - async result wrapper ([covered in traits](./traits.md))

**Usage examples:**
```ocaml
(* Search for cheap products *)
search_products connection `Cheap row_handler

(* Search for expensive products with minimum price *)
search_products connection (`Expensive 100.0) row_handler

(* Search for free products *)
search_products connection `Free row_handler
```

#### Choice Expressions with Arbitrary SQL Expressions

Choice expressions can contain any valid SQL expression, and constructors may have no parameters at all:

```sql
-- DDL Schema
CREATE TABLE test_table (
    id INTEGER PRIMARY KEY,
    col_a INTEGER,
    col_b TEXT,
    col_c TIMESTAMP
);

-- @filter_by_criteria
SELECT * FROM test_table 
WHERE @choice {
    A { col_a = @param_a } |
    B { col_b LIKE @param_b } |
    C { col_c BETWEEN @start_date AND @end_date } |
    D { 1 = 1 }
};
```

**Generated polymorphic variant:***
```ocaml
[`A of int | `B of string | `C of (string * string) | `D]
```
*\*OCaml example - types vary by target language and traits implementation*

**Parameter type inference:**
- `A` constructor uses `@param_a` → `A of int` (compared with col_a)
- `B` constructor uses `@param_b` → `B of string` (used with LIKE on col_b)
- `C` constructor uses two parameters → `C of (string * string)` (tuple for BETWEEN)
- `D` constructor has no parameters → `D` (constant expression)

#### Choice in Non-WHERE Contexts

Choice expressions work in any SQL context, including ORDER BY clauses:

```sql
-- @get_sorted_data
SELECT * FROM test_table 
ORDER BY
  @choice {
    A { col_a } |
    B { col_b } |
    C { col_c }
  } DESC,
  id ASC;
```

**Generated polymorphic variant:***
```ocaml
[`A | `B | `C]
```
*\*OCaml example - types vary by target language and traits implementation*

All constructors have no parameters since they reference column expressions directly.

#### Complex Expressions in Choice Branches

Choice branches can contain complex SQL expressions with multiple operations:

```sql
-- @complex_choice_example
SELECT * FROM test_table 
WHERE @choice {
    A { col_a + @offset > 100 } |
    B { UPPER(col_b) = UPPER(@search_term) } |
    C { col_c IS NOT NULL }
};
```

**Generated polymorphic variant:***
```ocaml
[`A of int | `B of string | `C]
```
*\*OCaml example - types vary by target language and traits implementation*

- `A` branch: arithmetic expression with parameter
- `B` branch: function calls with parameter
- `C` branch: NULL check with no parameters

### IN/NOT IN Parameters

sqlgg supports dynamic IN and NOT IN clauses using the `@param` syntax for list parameters.

#### Basic IN Parameter Syntax

```sql
-- expr (IN | NOT IN) @param
column_name IN @param_list
column_name NOT IN @param_list
```

#### IN Parameter Examples

```sql
-- DDL Schema
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    department TEXT,
    age INTEGER
);

-- @find_users_by_ids
SELECT * FROM users WHERE id IN @user_ids;
```

Parameter `@user_ids` is inferred as `int list` (list of integers matching id column type).

#### NOT IN Example

```sql
-- @find_users_excluding_departments
SELECT * FROM users WHERE department NOT IN @excluded_departments;
```

Parameter `@excluded_departments` is inferred as `string list` (list of strings matching department column type).

#### Complex IN Expressions

```sql
-- @find_users_with_conditions
SELECT * FROM users 
WHERE age IN @allowed_ages 
  AND department NOT IN @blocked_departments;
```

Parameters:
- `@allowed_ages`: `int list`
- `@blocked_departments`: `string list`

The IN/NOT IN parameters are type-inferred based on the column type they're compared against, always resulting in list types in the generated functions.

### IN Tuple List Parameters

sqlgg supports IN clauses with tuple expressions for multi-column comparisons.

#### Tuple IN Syntax

```sql
(expr1, expr2, ..., exprN) IN @param
(expr1, expr2, ..., exprN) NOT IN @param
```

The parameter type is inferred from the left-side expression types.

#### Tuple IN Examples

```sql
-- DDL Schema
CREATE TABLE user_roles (
    user_id INTEGER,
    role_id INTEGER,
    department TEXT,
    is_active INTEGER,
    PRIMARY KEY (user_id, role_id)
);

-- @find_user_roles_by_pairs
SELECT * FROM user_roles 
WHERE (user_id, role_id) IN @user_role_pairs;
```

Parameter `@user_role_pairs` is inferred as `(int * int) list` based on `user_id` and `role_id` column types.

#### Complex Tuple IN Example

```sql
-- @find_active_roles_by_criteria
SELECT * FROM user_roles 
WHERE (user_id, department, is_active) IN @criteria;
```

Parameter `@criteria` is inferred as `(int * string * int) list`.

#### Mixed Expressions in Tuples

```sql
-- @find_roles_with_calculated_values
SELECT * FROM user_roles 
WHERE (user_id + 1, role_id * 2) IN @calculated_pairs;
```

The tuple parameter type is inferred from the result types of the expressions on the left side, not just direct column references.

### Option Actions

Option Actions provide conditional SQL clauses using the `{expr}?` syntax for optional query parts.

#### Optional WHERE Clauses

```sql
-- Optional WHERE condition
SELECT * FROM test19
LEFT JOIN test20 ON test20.c = @test20a
WHERE { c = @choice2 }?
GROUP BY b;
```

The `{c = @choice2}?` creates an optional WHERE clause:
- If parameter is provided → WHERE clause is included
- If parameter is `None/null` → WHERE clause becomes `WHERE TRUE` (effectively skipped)

#### Optional SET Clauses with DEFAULT

```sql
-- DDL with DEFAULT constraint
CREATE TABLE tbl (
    id INTEGER PRIMARY KEY,
    field_name TEXT DEFAULT 'default_value'
);

-- Optional SET with DEFAULT fallback  
INSERT INTO tbl
SET field_name = { CONCAT(@param, 'string') }??;
```

The `{CONCAT(@param, 'string')}??` creates optional SET behavior:
- If `@param` is provided → uses the CONCAT expression
- If `@param` is `None/null` → uses DEFAULT value (must exist in DDL)

#### Language-Specific Implementation

The parameter handling depends on target language:

**OCaml example:**
```ocaml
(* @param becomes option type *)
insert_with_optional : string option -> unit

(* Usage *)
insert_with_optional (Some "value")  (* Uses CONCAT *)
insert_with_optional None            (* Uses DEFAULT *)
```

Option Actions require parameters within the `{...}?` block and rely on DEFAULT constraints for fallback behavior in SET clauses.

## Nullability Inference

Beyond column constraints from DDL, sqlgg performs **advanced nullability inference** for complex expressions involving multiple values.

### Basic Nullability Rules

When combining multiple values in expressions, sqlgg follows these nullability rules:

**Priority order (most restrictive wins):**
1. **Nullable** - If any operand can be NULL, result is nullable
2. **Strict** - If all operands are NOT NULL, result is strict  
3. **Depends** - For ambiguous cases (fallback to Strict)

#### Basic Examples

```sql
-- DDL Schema
CREATE TABLE mixed_nulls (
    id INTEGER PRIMARY KEY,      -- Strict
    name TEXT NOT NULL,          -- Strict  
    email TEXT,                  -- Nullable
    phone TEXT                   -- Nullable
);

-- Expression nullability inference
SELECT 
    name,                        -- Strict (column is NOT NULL)
    email,                       -- Nullable (column allows NULL)
    CONCAT(name, email)          -- Nullable (email operand is nullable)
FROM mixed_nulls;
```

**Generated return type:**
```ocaml
(string * string option * string option)
```

#### Arithmetic with Mixed Nullability
```sql
SELECT 
    id + 1,                      -- Strict (id is strict, constant is strict)
    age + @param,                -- Depends on @param nullability
    COALESCE(email, 'default')   -- Strict (COALESCE removes nullability)
FROM mixed_nulls;
```

### Aggregate Functions and Nullability

Aggregate functions in sqlgg have special nullability rules that consider whether the query can return "no rows".

#### The Core Rule

**The key rule**: Aggregation nullability depends on whether the query can return "no rows":

- **Simple aggregation (no GROUP BY)**: Results become nullable because the entire table might be empty
- **GROUP BY aggregation**: Strict types stay strict because each returned group is guaranteed to exist, but nullable types remain nullable
- **Window functions with OVER clause**: Results become nullable because outer query filters could eliminate all rows

#### Simple Aggregation Examples

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

Result is `decimal option` because SUM can return NULL if the table is empty.

```sql
-- @get_statistics
SELECT SUM(amount), AVG(amount), MAX(amount), MIN(amount), COUNT(*) FROM sales;
```

**Generated types:**
- `SUM(amount)` → `decimal option` (nullable if table empty)
- `AVG(amount)` → `float option` (nullable if table empty)  
- `MAX(amount)` → `decimal option` (nullable if table empty)
- `MIN(amount)` → `decimal option` (nullable if table empty)
- `COUNT(*)` → `int` (strict - COUNT never returns NULL)

#### GROUP BY Aggregation Examples

```sql
-- Case 2: GROUP BY aggregation - each group is guaranteed to exist
-- @get_sales_by_region
SELECT region, SUM(amount), COUNT(*) 
FROM sales 
GROUP BY region;
```

**Generated types:**
- `region` → `string` (strict - it's the grouping column, guaranteed to exist **if region is NOT NULL**)
- `SUM(amount)` → `decimal` (strict - each group is guaranteed to have rows)  
- `COUNT(*)` → `int` (strict - COUNT never returns NULL)

**Important**: The above assumes `region` is NOT NULL. If `region` were nullable, the types would be different.

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

#### COUNT is Special

`COUNT` has unique behavior across all contexts:

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
- Works the same in simple aggregation, GROUP BY, and window functions

#### Function-Specific Aggregation Rules

**In simple aggregation (no GROUP BY):**
- `SUM`, `AVG`, `MAX`, `MIN` → nullable (might return NULL if table empty)
- `COUNT` → strict (always returns 0 or positive integer)

**In GROUP BY aggregation:**
- `SUM`, `AVG`, `MAX`, `MIN` of strict columns → strict (each group exists)
- `SUM`, `AVG`, `MAX`, `MIN` of nullable columns → nullable (inherit input nullability)
- `COUNT` → always strict
- **Grouping columns**: strict if NOT NULL, nullable if nullable (SQL NULL grouping behavior)

#### Detailed GROUP BY Examples

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

#### Multiple GROUP BY Columns

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

#### GROUP BY with NULL Values Behavior

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
- One row with NULL value (representing all rows where optional_region IS NULL)
- No row with NULL if no NULL values exist in the data

#### Mixed Aggregation Example

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

**In window functions:**
- `SUM`, `AVG`, `MAX`, `MIN` → nullable (outer query might eliminate all rows)
- `COUNT` → strict (exception to the rule)

### JOIN Nullability

JOINs in sqlgg automatically affect column nullability based on JOIN type, regardless of original column constraints.

#### INNER JOIN

```sql
-- DDL Schema
CREATE TABLE users (
    id INTEGER PRIMARY KEY,      -- Strict
    name TEXT NOT NULL,          -- Strict
    email TEXT                   -- Nullable
);

CREATE TABLE profiles (
    user_id INTEGER NOT NULL,    -- Strict  
    bio TEXT NOT NULL,           -- Strict
    avatar_url TEXT              -- Nullable
);

-- @get_user_profiles  
SELECT u.name, u.email, p.bio, p.avatar_url
FROM users u
INNER JOIN profiles p ON u.id = p.user_id;
```

INNER JOIN preserves original nullability - all columns keep their DDL-defined strictness/nullability.

**Generated types:**
- `u.name` → `string` (strict)
- `u.email` → `string option` (nullable)  
- `p.bio` → `string` (strict)
- `p.avatar_url` → `string option` (nullable)

#### LEFT JOIN

```sql
-- @get_users_with_optional_profiles
SELECT u.name, u.email, p.bio, p.avatar_url  
FROM users u
LEFT JOIN profiles p ON u.id = p.user_id;
```

LEFT JOIN makes **right-side columns nullable**:

**Generated types:**
- `u.name` → `string` (strict, from left side)
- `u.email` → `string option` (nullable, from left side)  
- `p.bio` → `string option` (was strict, now nullable due to LEFT JOIN)
- `p.avatar_url` → `string option` (was already nullable)

#### RIGHT JOIN

```sql
-- @get_profiles_with_optional_users
SELECT u.name, u.email, p.bio, p.avatar_url
FROM users u  
RIGHT JOIN profiles p ON u.id = p.user_id;
```

RIGHT JOIN makes **left-side columns nullable**:

**Generated types:**
- `u.name` → `string option` (was strict, now nullable due to RIGHT JOIN)
- `u.email` → `string option` (was already nullable)
- `p.bio` → `string` (strict, from right side)
- `p.avatar_url` → `string option` (nullable, from right side)

#### FULL OUTER JOIN

```sql
-- @get_all_users_and_profiles
SELECT u.name, u.email, p.bio, p.avatar_url
FROM users u
FULL OUTER JOIN profiles p ON u.id = p.user_id;
```

FULL OUTER JOIN makes **both sides nullable** - all columns become `option` types regardless of original constraints.

**Generated types:**
- `u.name` → `string option`
- `u.email` → `string option`
- `p.bio` → `string option`
- `p.avatar_url` → `string option`

#### NATURAL JOIN and USING

```sql
-- NATURAL JOIN preserves nullability based on join type
SELECT * FROM users u NATURAL JOIN profiles p;

-- USING clause works the same as explicit ON conditions
SELECT * FROM users u JOIN profiles p USING (user_id);
```

NATURAL JOIN and USING clauses follow the same nullability rules as their corresponding JOIN types (INNER, LEFT, RIGHT, FULL).

### Subquery Nullability

Subqueries have complex nullability rules that depend on their context and whether they can return "no rows".

#### Basic Subquery Rules

**The key principle**: Subqueries can return NULL if:
1. The subquery returns no rows
2. The subquery returns NULL values
3. The subquery has a HAVING clause (which can eliminate all groups)

#### Scalar Subqueries

```sql
-- DDL Schema
CREATE TABLE orders (
    id INTEGER PRIMARY KEY,
    user_id INTEGER NOT NULL,
    amount DECIMAL NOT NULL,
    status TEXT NOT NULL
);

-- @get_user_with_total
SELECT u.name, 
       (SELECT SUM(amount) FROM orders o WHERE o.user_id = u.id) as total_spent
FROM users u;
```

**Generated types:**
- `u.name` → `string` (strict)
- `total_spent` → `decimal option` (nullable - subquery might return no rows)

#### Subqueries with HAVING

```sql
-- @get_user_with_average_high_orders
SELECT u.name,
       (SELECT AVG(amount) 
        FROM orders o 
        WHERE o.user_id = u.id 
        HAVING COUNT(*) > 5) as avg_amount
FROM users u;
```

**Generated types:**
- `avg_amount` → `float option` (nullable - HAVING can eliminate all groups)

#### COUNT in Subqueries

```sql
-- COUNT is special - never returns NULL
-- @get_user_with_order_count
SELECT u.name,
       (SELECT COUNT(*) FROM orders o WHERE o.user_id = u.id) as order_count
FROM users u;
```

**Generated types:**
- `order_count` → `int` (strict - COUNT never returns NULL, even in subqueries)

#### Complex Nested Subqueries

```sql
-- @get_complex_calculation
SELECT u.name,
       (SELECT MAX(
           (SELECT AVG(amount) 
            FROM orders o2 
            WHERE o2.user_id = o1.user_id 
            GROUP BY o2.status)
        ) FROM orders o1 
          WHERE o1.user_id = u.id) as max_avg_by_status
FROM users u;
```

**Generated types:**
- `max_avg_by_status` → `float option` (nullable - complex nested aggregation can return NULL)

#### Subqueries with WHERE FALSE

```sql
-- @impossible_subquery
SELECT (SELECT amount FROM orders WHERE FALSE) as impossible;
```

**Generated types:**
- `impossible` → `decimal option` (nullable - subquery can never return rows)

#### EXISTS Subqueries

```sql
-- @users_with_orders
SELECT u.name
FROM users u
WHERE EXISTS (SELECT 1 FROM orders o WHERE o.user_id = u.id);
```

EXISTS subqueries don't affect the nullability of selected columns - they only affect row filtering.

[← Back to Main](./index.md) | [← Previous: DDL](./ddl.md) | <!--  [Next:  →](./.md) -->
