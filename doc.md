# 📘 Documentation `sqlgg`

## 🧭 Contents
- [📘 Documentation `sqlgg`](#-documentation-sqlgg)
  - [🧭 Contents](#-contents)
  - [🆕 Feature Review](#-feature-review)
    - [Support for DEFAULT Values](#support-for-default-values)
      - [🔹 Example](#-example)
    - [Support for Reusable Queries](#support-for-reusable-queries)
      - [🔹 Usage Modes](#-usage-modes)
      - [🔹 Example](#-example-1)
    - [Type-safe Enums](#type-safe-enums)
      - [🔹 String Literal Type Inference](#-string-literal-type-inference)
      - [🔹 Union Types](#-union-types)
      - [🔹 Type Checking Rules](#-type-checking-rules)
        - [Type Hierarchy](#type-hierarchy)
        - [Valid Operations](#valid-operations)
      - [⚙️ Optional Flag: `type-safe-enums`](#️-optional-flag-type-safe-enums)
        - [💡 OCaml Example](#-ocaml-example)
    - [VALUES as Table Source](#values-as-table-source)
      - [🔹 Supported Forms](#-supported-forms)
      - [🔹 Examples](#-examples)
    - [Conditional WHERE Clauses](#conditional-where-clauses)
      - [🔹 Example](#-example-2)
      - [🧠 Semantics](#-semantics)
  - [🛣️ Roadmap](#️-roadmap)


## 🆕 Feature Review

> Features are listed from latest to earliest, with detailed descriptions and examples.

### Support for DEFAULT Values

*Added: April 2025*

Use `{expr}??` syntax to conditionally include values or fall back to SQL DEFAULT.

#### 🔹 Example

```sql
INSERT INTO `tbl`
SET `field_name` = { CONCAT(@param, 'string') }??
```

- When `@param` is provided: The expression inside braces is used
- When `@param` is empty: SQL `DEFAULT` keyword is inserted

Multiple parameters are treated as tuples:

```sql
INSERT INTO `tbl`
SET `field_name` = { CONCAT(@param1, @param2) }??
```

⚠️ **Note**: Fields must have `DEFAULT` value defined in schema, otherwise SQL errors will occur.

⚠️ **Note#2**: Currently only supported in OCaml using the `option` type.


→ [PR #189](https://github.com/ygrek/sqlgg/pull/189)

---

### Support for Reusable Queries

*Added: April 2025*

Create reusable query fragments with Common Table Expressions (CTE) using `include: reuse` and `&name` syntax.

#### 🔹 Usage Modes

| Annotation                   | Behavior                                       |
| ---------------------------- | ---------------------------------------------- |
| `include: reuse`             | Query is reusable but no function is generated |
| `include: reuse-and-execute` | Query is both reusable and directly callable   |
| `include: executable`        | Normal behavior (default)                      |

#### 🔹 Example

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

In this example:
- `@abcd` defines a reusable query fragment
- `&abcd` embeds that query as a CTE named `x`
- Parameters are supported in reused queries
- Type inference works across both queries

⚠️ **Note**: Currently only available for OCaml.

→ [PR #174](https://github.com/ygrek/sqlgg/pull/174)

---

### Type-safe Enums

*Added: March 2025*

Provides type-safe handling of SQL ENUMs and string literal union types with inference and validation.

#### 🔹 String Literal Type Inference

```sql
SELECT @param { A { 'A' } | B { 'B' } | C { 'C' } }
```

This defines an enum-like union type: `'A' | 'B' | 'C'`

#### 🔹 Union Types

| Type           | Description                                     |
| -------------- | ----------------------------------------------- |
| 🔒 Closed union | Fixed set of literal values (like enums)        |
| 🔓 Open union   | Built dynamically from literals or other unions |

#### 🔹 Type Checking Rules

With schema:
```sql
CREATE TABLE tbl (
  status_enum ENUM('A', 'B', 'C') NOT NULL,
  status_text TEXT NOT NULL
);
```

##### Type Hierarchy

- ✅ Enum values can be used as text (`Enum <: Text`)
- ❌ Text values cannot be treated as enums

##### Valid Operations

```sql
-- ✅ Valid: assigning Enum to Text
INSERT INTO tbl (status_text)
VALUES (@param { A { 'A' } | B { 'B' } });

-- ✅ Valid: using enum in string operations
INSERT INTO tbl (status_text)
SELECT CONCAT(
  @param { A { 'A' } | B { 'B' } },
  '_suffix'
);

-- ✅ Valid: direct enum value
INSERT INTO tbl (status_enum) VALUES ('A');

-- ❌ Invalid: CONCAT() returns Text, not Enum
INSERT INTO tbl (status_enum)
SELECT CONCAT(@param { A { 'A' } | B { 'B' } }, '_suffix');
```

#### ⚙️ Optional Flag: `type-safe-enums`

When enabled:
- 🧪 Parameters for enum fields are type-checked
- 🧠 Corresponding enum types are inferred
- 🔁 For OCaml only, enums can map to polymorphic variants

##### 💡 OCaml Example

```sql
CREATE TABLE some_table (
  id INT PRIMARY KEY AUTO_INCREMENT,
  status ENUM('pending', 'sending', 'sent', 'cancelled') NOT NULL,
  status_b ENUM('a', 'b', 'c', 'd') NOT NULL
);

-- @name | include: executable
INSERT INTO some_table (
  status,
  status_b
) VALUES (
  'pending',
  @status_b
);
```

Generated OCaml code with `type-safe-enums` enabled:

```ocaml
module Enum_status_b = T.Make_enum(struct
  type t = [ `A | `B | `C | `D ]
  let inj = function
    | "a" -> `A | "b" -> `B | "c" -> `C | "d" -> `D
    | s -> failwith ("Invalid enum value: " ^ s)
  let proj = function
    | `A -> "a" | `B -> "b" | `C -> "c" | `D -> "d"
end)

let insert_some_table db ~status_b =
  let set_params stmt =
    let p = T.start_params stmt 1 in
    Enum_status_b.set_param p status_b;
    T.finish_params p
  in
  T.execute db "INSERT INTO some_table (status, status_b) VALUES ('pending', ?)" set_params
```

> 🔐 **All SQL string values are type-checked against the field's declared enum** in the database.
>
> 🧬 The `type-safe-enums` flag affects only OCaml code generation, where enum parameters can be mapped to polymorphic variants.

→ [PR #152](https://github.com/ygrek/sqlgg/pull/152)

---

### VALUES as Table Source

*Added: February 2025*

Support for using `VALUES` as a table source in JOINs and other contexts.

#### 🔹 Supported Forms

- Inline values: `VALUES ROW(1, 'foo'), ROW(2, 'bar')`
- Parameterized values: `VALUES @param`

#### 🔹 Examples

```sql
-- Using inline ROWs
SELECT p.id, x.a
FROM products p
JOIN ( VALUES ROW(1, 'foo'), ROW(2, 'bar') ) AS x (a, b)
ON p.name = x.b;

-- Using parameters
SELECT p.id, x.a
FROM products p
JOIN ( VALUES @param ) AS x (a, b)
ON p.name = x.b;
```

⚠️ **Note**: If `@param` is an empty list, a fallback query is generated: `SELECT %cols% WHERE FALSE`

📘 Reference: [MySQL 8.4 VALUES Syntax](https://dev.mysql.com/doc/refman/8.4/en/values.html)

→ [PR #148](https://github.com/ygrek/sqlgg/pull/148)

---

### Conditional WHERE Clauses

*Added: January 2025*

Use `{expr}?` syntax to conditionally include WHERE clauses based on optional parameters.

#### 🔹 Example

```sql
SELECT *
FROM test19
LEFT JOIN test20 ON test20.c = @test20a
WHERE { c = @choice2 }?
GROUP BY b;
```

If parameter `@choice2` is:
- Present/Non-empty → generates `WHERE c = ?`
- Empty/Missing → generates `WHERE TRUE`

> **Note**: The exact representation of "empty" vs "non-empty" depends on the target language. Currently only OCaml is supported, where this is implemented using the `option` type. No polymorphic variant is introduced to avoid unnecessary wrapping.

#### 🧠 Semantics

| Syntax     | When empty        | Suitable for    |
| ---------- | ----------------- | --------------- |
| `{expr}?`  | Inserts `TRUE`    | `WHERE` clauses |
| `{expr}??` | Inserts `DEFAULT` | `INSERT/UPDATE` |


→ [PR #142](https://github.com/ygrek/sqlgg/pull/142)

---

## 🛣️ Roadmap

*Coming soon*
