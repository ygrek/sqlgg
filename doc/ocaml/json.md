---
sidebar_position: 3
title: JSON
description: Working with JSON in OCaml using Yojson
---

# JSON in OCaml

SQL `JSON` type maps to a polymorphic variant type compatible with `Yojson.Safe.t`. The default implementation can be customized via [traits](./traits.md).

## Basic Usage

```sql
CREATE TABLE game_data (
  id INT AUTO_INCREMENT PRIMARY KEY,
  data JSON NOT NULL
);

-- @get_all
SELECT * FROM game_data;
```

```ocaml
let* rows = Db.List.get_all db (fun ~id ~data -> (id, data)) in
List.iter (fun (id, data) ->
  prerr_endline @@ Printf.sprintf "ID: %Ld, Data: %s" 
    id 
    (Yojson.Safe.to_string (data :> Yojson.Safe.t))
) rows
```

### Why `:>` coercion? {#why-coercion}

sqlgg defines its own `json` type:

```ocaml
type json = [
  | `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Assoc of (string * json) list
  | `List of json list
  | `Int of int
  | `Intlit of string
]
```

This is a subset of `Yojson.Safe.t` (which also has `` `Tuple `` and `` `Variant ``). Use `:>` to coerce when calling Yojson functions:

```ocaml
Yojson.Safe.to_string (data :> Yojson.Safe.t)
```

For the reverse direction (e.g. when using `Yojson` to build JSON), use `Sqlgg_trait_types.convert_json`:

```ocaml
let to_yojson t =
  `Assoc [
    "name", `String t.name;
    "count", `Int t.count;
  ]

let set_param x =
  let x = Sqlgg_trait_types.convert_json (to_yojson x) in
  (x :> Sqlgg_trait_types.json)
```

`convert_json` recursively converts `Yojson.Safe.t` to sqlgg's `json` type. It handles `` `Tuple `` by converting to `` `List ``, but fails on `` `Variant ``.

## Constructing JSON Parameters {#constructing-json-parameters}

Let's look at [JSON_SEARCH](https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions/json_search):

```
JSON_SEARCH(json_doc, return_arg, search_str[, escape_char[, path] ...])
```

sqlgg infers this vararg signature. Example:

```sql
-- @search_json
SELECT JSON_SEARCH(@json, @one, 'def');
```

```ocaml
let result = Db.search_json db
  ~json:(`Assoc [
    "users", `List [
      `Assoc [
        "id", `Int 1;
        "settings", `Assoc [
          "themes", `List [`String "dark"]
        ]
      ]
    ]
  ])
  ~one:`One
```

Here:
- `~json` — the `json_doc` parameter, type `Sqlgg_trait_types.json`. Build JSON directly with polymorphic variant constructors (`` `Assoc ``, `` `List ``, `` `String ``, `` `Int ``, etc.)
- `~one` — the `return_arg` parameter, type `` [ `One | `All ] ``. This is a special [strict literal](../sql/literals.md#one_or_all-only-one-or-all) that only accepts `'one'` or `'all'` in SQL

## JSON Functions

```sql
-- @build_array
SELECT JSON_ARRAY(1, 'abc', NULL, TRUE, CURTIME());

-- @build_object  
SELECT JSON_OBJECT('id', 87, 'name', 'carrot');

-- @extract_value
SELECT JSON_EXTRACT('[10, 20, [30, 40]]', '$[1]');
```

```ocaml
let arr = Db.build_array db in
prerr_endline @@ Yojson.Safe.to_string (arr :> Yojson.Safe.t)
(* [1, "abc", null, true, "12:34:56"] *)

let obj = Db.build_object db in
prerr_endline @@ Yojson.Safe.to_string (obj :> Yojson.Safe.t)
(* {"id": 87, "name": "carrot"} *)

(* JSON_EXTRACT can return null *)
match Db.extract_value db with
| Some json -> prerr_endline @@ Yojson.Safe.to_string (json :> Yojson.Safe.t)
| None -> prerr_endline "Path not found"
```

## JSON Path Parameters

JSON paths use `Sqlgg_json_path`:

```sql
-- @search_path
SELECT JSON_CONTAINS(@j, @j2, @path);
```

```ocaml
open Sqlgg_json_path.Ast.Syntax

let catalog = `Assoc [
  "products", `List [
    `Assoc [
      "id", `String "LAPTOP-001";
      "specs", `Assoc [
        "features", `List [
          `String "backlit_keyboard";
          `String "fingerprint_reader"
        ]
      ]
    ]
  ]
] in

(* $.products.specs.features *)
let path = root / ~."products" / ~."specs" / ~."features" in

let result = Db.search_path db ~j:catalog ~j2:(`String "fingerprint_reader") ~path in
prerr_endline @@ Bool.to_string @@ Option.default false result

(* $.products[0].specs.features *)
let path_with_idx = root / ~."products" / idx 0 / ~."specs" / ~."features"
```

### Path Syntax

| Syntax | Meaning |
|--------|---------|
| `root` | `$` — root element |
| `~."key"` | `.key` — object member |
| `idx n` | `[n]` — array index |
| `a / b` | Concatenate path segments |

## Custom Module Wrapper

Use `[sqlgg] module=` to wrap JSON with custom types. See [metadata](../sql/metadata.md#modulename) for details.

## SQL NULL vs JSON null

```ocaml
(* SQL NULL - column has no value *)
let row_with_sql_null : json option = None

(* JSON null - column contains the JSON literal null *)
let row_with_json_null : json option = Some `Null

(* Non-null JSON object *)
let row_with_data : json option = Some (`Assoc ["key", `String "value"])
```

See [json_null_kind metadata](../sql/metadata.md#json_null_kindtruefalseauto) to control this behavior.
