# 📘 Documentation `sqlgg`

## 🧭 Contents
- [📘 Documentation `sqlgg`](#-documentation-sqlgg)
  - [🧭 Contents](#-contents)
  - [🆕 Feature Review](#-feature-review)
    - [Column-level Customization for `SELECT` (currently) Queries](#column-level-customization-for-select-currently-queries)
      - [🔹 Supported Annotations](#-supported-annotations)
      - [🔹 Example](#-example)
      - [🔹 Generated OCaml (Excerpt)](#-generated-ocaml-excerpt)
      - [🧠 Semantics](#-semantics)
      - [🔹 Module Requirements](#-module-requirements)
      - [🔹 OCaml Implementation Example](#-ocaml-implementation-example)
    - [Support for DEFAULT Values](#support-for-default-values)
      - [🔹 Example](#-example-1)
    - [Support for Reusable Queries](#support-for-reusable-queries)
      - [🔹 Usage Modes](#-usage-modes)
      - [🔹 Example](#-example-2)
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
      - [🔹 Example](#-example-3)
      - [🧠 Semantics](#-semantics-1)
  - [🛣️ Roadmap](#️-roadmap)


## 🆕 Feature Review

> Features are listed from latest to earliest, with detailed descriptions and examples.

### Column-level Customization for `SELECT` (currently) Queries

*Added: May 2025*

Provides custom mapping for column values in `SELECT` queries using simple `-- [sqlgg]` annotations in table definitions.

#### 🔹 Supported Annotations

| Annotation                 | Description                                                 |
| -------------------------- | ----------------------------------------------------------- |
| `module=Module_name`       | *(Optional)* Specifies a module to wrap column value access |
| `get_column=function_name` | *(Optional)* Specifies a custom function within the module  |

Both annotations are optional:
- With no annotations: Default trait functions are used directly (`T.get_column_Int`, etc.)
- With only `module`: Uses `Module_name.get_column` or `Module_name.get_column_nullable`
- With both: Uses the custom function specified (`Module_name.function_name`)

#### 🔹 Example

```sql
CREATE TABLE table_37 (
  -- [sqlgg] module=HelloWorld
  col_1 INT PRIMARY KEY,

  -- [sqlgg] module=Abcdefg
  -- [sqlgg] get_column=test
  col_2 INT NOT NULL
);

CREATE TABLE table_38 (
  -- [sqlgg] module=FooBar
  col_3 INT PRIMARY KEY,
  col_4 TEXT NOT NULL
);
```

And query

```sql
-- @deeply_nested_example
SELECT 
  (
    SELECT MAX(x.col_val)
    FROM (
      SELECT col_1 as col_val,
           1 + 3 as aaaaa,
           1 + col_1 as bbb
      FROM table_37 
      WHERE col_1 > (
        SELECT MIN(col_1) 
        FROM table_37
      )
    ) as x
  ) as deeply_nested_query,
  col_2,
  col_3,
  col_4
FROM table_37
LEFT JOIN table_38
ON table_37.col_1 = table_38.col_3;
```

#### 🔹 Generated OCaml (Excerpt)

```
callback
  ~deeply_nested_query:(HelloWorld.get_column_nullable (T.get_column_int64_nullable stmt 0))
  ~col_2:(Abcdefg.test (T.get_column_int64 stmt 1))
  ~col_3:(FooBar.get_column_nullable (T.get_column_int64_nullable stmt 2))
  ~col_4:(T.get_column_Text_nullable stmt 3)
```

- `col_1` uses `HelloWorld.get_column_nullable` to wrap the standard getter
- `col_2` uses `Abcdefg.test` as specified by the custom function name
- `col_3` uses `FooBar.get_column_nullable` with the default function name
- `col_4` uses the default `T.get_column_Text_nullable` (no annotation)

#### 🧠 Semantics

| Case                     | Supported | Notes                                                      |
| ------------------------ | --------- | ---------------------------------------------------------- |
| `SELECT` column results  | ✅         | Custom column accessors are generated based on annotations |
| `@param` in `WHERE` etc. | ❌         | Parameters currently use default `T.set_param_*` behavior  |
| `INSERT` / `UPDATE`      | ❌         | Not yet supported — future extension planned               |

Currently, customization applies only to `SELECT` results. Parameter handling (`@param`) and other DML operations(`INSERT`, `UPDATE`) still rely on default trait functions defined in the `module Sqlgg (T : Sqlgg_traits.M)` implementation.

#### 🔹 Module Requirements

To use this feature, you must ensure that the specified module implements:

1. For standard cases (when only `module=ModuleName` is specified):
   - `ModuleName.get_column` - For non-nullable values
   - `ModuleName.get_column_nullable` - For nullable values

2. For custom function names (when both `module=ModuleName` and `get_column=custom_name` are specified):
   - `ModuleName.custom_name` - For non-nullable values
   - `ModuleName.custom_name_nullable` - For nullable values

The function signatures must match the corresponding SQL type. For example, for an INT column:

```ocaml
(* Standard function names *)
val get_column : int64 -> 'b
val get_column_nullable : int64 option -> 'b

(* Custom function names *)
val custom_name : int64 -> 'b
val custom_name_nullable : int64 option -> 'b
```

Where `'b` is your desired domain-specific output type.

#### 🔹 OCaml Implementation Example

```ocaml
module HelloWorld = struct
  (* For non-nullable INT values *)
  let get_column (x : int64) : int = 
    Printf.printf "Processing value: %Ld\n" x;
    Int64.to_int x

  (* For nullable INT values *)
  let get_column_nullable (x : int64 option) : int option = 
    match x with
    | None -> 
        Printf.printf "Received NULL value\n";
        None
    | Some value -> 
        Printf.printf "Processing value: %Ld\n" value;
        Some (Int64.to_int value)
end

module Abcdefg = struct
  (* Custom function name used in annotation for INT column *)
  let test (x : int64) : string =
    Printf.printf "Custom test function: %Ld\n" x;
    Int64.to_string x
end
```

<details>
<summary><strong>Default trait function signatures (click to expand)</strong></summary>

```ocaml
(* Core getter functions in trait module *)
val get_column_bool : row -> int -> bool
val get_column_bool_nullable : row -> int -> bool option

val get_column_int64 : row -> int -> int64
val get_column_int64_nullable : row -> int -> int64 option

val get_column_float : row -> int -> float
val get_column_float_nullable : row -> int -> float option

val get_column_decimal : row -> int -> float
val get_column_decimal_nullable : row -> int -> float option

val get_column_datetime : row -> int -> string
val get_column_datetime_nullable : row -> int -> string option

(* These functions are mapped to SQL types in the trait *)
val get_column_Bool : row -> int -> Bool.t
val get_column_Int : row -> int -> Int.t
val get_column_Text : row -> int -> Text.t
val get_column_Any : row -> int -> Any.t
val get_column_Float : row -> int -> Float.t
val get_column_Decimal : row -> int -> Decimal.t
val get_column_Datetime : row -> int -> Datetime.t

val get_column_Bool_nullable : row -> int -> Bool.t option
val get_column_Int_nullable : row -> int -> Int.t option
val get_column_Text_nullable : row -> int -> Text.t option
val get_column_Any_nullable : row -> int -> Any.t option
val get_column_Float_nullable : row -> int -> Float.t option
val get_column_Decimal_nullable : row -> int -> Decimal.t option
val get_column_Datetime_nullable : row -> int -> Datetime.t option
```

</details>

→ [PR #192](https://github.com/ygrek/sqlgg/pull/192)

---

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
