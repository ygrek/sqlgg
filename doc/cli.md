---
sidebar_position: 2
title: CLI Reference
description: Complete sqlgg command-line reference
---

import CliOptions from './_cli-options.md';

# CLI Reference

```
sqlgg <options> <file.sql> [<file2.sql> ...]
```

Files are read as one stream: DDL builds up the schema, queries become generated functions. Each option applies to the files after it, which is how the schema goes in without generating code:

```bash
sqlgg -gen none ddl.sql -gen caml dml.sql
```

## All Options

<CliOptions />

## Configuration file

`-config <file>` reads options from a JSON file so build rules do not have to repeat them:

```json title="sqlgg.json"
{
  "dialect": "mysql",
  "header": "static",
  "gen": "caml_io",
  "name": "Make",
  "params": "unnamed",
  "schema": ["schema/*.sql"],
  "inputs": ["queries.sql"]
}
```

```bash
sqlgg -config sqlgg.json > queries.ml
```

This is equivalent to:

```bash
sqlgg -dialect mysql -static-header -gen caml_io -name Make -params unnamed \
  -open schema/tables.sql queries.sql > queries.ml
```

sqlgg accepts at most one `-config` flag (`-config file` or `-config=file`). It applies the file first and then the command-line options in order, so command-line options override the file regardless of where `-config` sits, and repeatable options such as `-target` append to the file's lists. Because sqlgg reads each SQL file with the options active when it is seen, leave `inputs` out of the file when passing SQL files or overriding `-dialect`, `-gen`, or `-category` on the command line:

```bash
sqlgg -config sqlgg.json -name Users users.sql > users.ml
sqlgg -config sqlgg.json -name Posts posts.sql > posts.ml
```

Relative paths resolve from the configuration file's directory, not the working directory. Every key is optional and falls back to its default. Unknown keys or invalid values exit with 2. Run `sqlgg -config-json-schema` to print the JSON Schema.

| Key | Option | Value | Default |
| --- | --- | --- | --- |
| `dialect` | `-dialect` | `"mysql"`, `"postgresql"`, `"sqlite"`, `"tidb"` | `"mysql"` |
| `no_check` | `-no-check` | list of dialect feature names | `[]` |
| `write_notnull_null` | `-allow-write-notnull-null` | `"allow"`, `"reject"` | `"reject"` |
| `header` | `-no-header`, `-no-header-timestamp`, `-static-header` | `"full"`, `"no_timestamp"`, `"static"`, `"none"` | `"full"` |
| `gen` | `-gen` | `"cxx"`, `"caml"`, `"caml_io"`, `"java"`, `"xml"`, `"json"`, `"csharp"`, `"sql"`, `"none"` | `"none"` |
| `name` | `-name` | string | `"sqlgg"` |
| `params` | `-params` | `"auto"`, `"none"`, `"named"`, `"unnamed"`, `"oracle"`, `"postgresql"` | `"auto"` |
| `category` | `-category` | list of `"ddl"`, `"dql"`, `"dml"`, `"dcl"`, `"tcl"`; `[]` is `none` | all five |
| `dynamic_select` | `-dynamic-select` | `"both"`, `"off"` | `"off"` |
| `schema` | `-open` | list of paths; the file name may be a glob such as `*.sql` (an unmatched pattern is an error) | `[]` |
| `inputs` | positional files | list of paths; `"-"` reads stdin | `[]` |
| `mode` | `-diff`, `-migrate` | `"generate"`, `["diff", {...}]`, or `["migrate", {...}]` | `"generate"` |
| `migrations` | [`sqlgg migrations generate`](#sqlgg-migrations) | object | none |

### Migration modes

`mode` defaults to `"generate"`. For `-diff` and `-migrate`, pass a two-element array `["diff", {...}]` or `["migrate", {...}]`. Each mode accepts only its own keys, and neither accepts top-level `schema` or `inputs`:

```json title="migration.json"
{
  "dialect": "mysql",
  "gen": "caml_io",
  "name": "Make",
  "mode": ["migrate", {
    "initial": ["initial.sql"],
    "migrations_file": "migrations.sql",
    "extends": "manual.sql",
    "target": ["schema.sql"]
  }]
}
```

| Key in `mode` | Mode | Option | Value | Default |
| --- | --- | --- | --- | --- |
| `base` | `diff` | `-base` | list of paths | `[]` |
| `initial` | `migrate` | `-initial` | list of paths | `[]` |
| `migrations_file` | `migrate` | `-migrations-file` | path | none |
| `extends` | `migrate` | `-extends` | path | none |
| `target` | both | `-target` | list of paths | `[]` |
| `max_migration_id_length` | both | `-max-migration-id-length` | integer | `191` |
| `new_tables` | both | `-ddl-as-migration` | `"schema"`, `"migrations"` | `"schema"` |
| `alter_algorithm` | both | `-alter-algorithm` | `"default"`, `"instant"`, `"inplace"`, `"copy"` | none |
| `alter_lock` | both | `-alter-lock` | `"default"`, `"none"`, `"shared"`, `"exclusive"` | none |

`-now`, `-no-check all`, `-category -<cat>`, and diagnostic flags (`-show-tables`, `-show-table`, `-debug`, `-version`, `-test`) have no JSON keys and stay on the command line.

### Sharing `sqlgg.json` with the language server

The language server reads `schema` and `dialect` from the nearest `sqlgg.json` in the SQL file's directory or an ancestor and ignores all other keys. One `sqlgg.json` can configure the editor, code generation, and `sqlgg migrations generate` together (except when using `diff` or `migrate` mode, which disallow `schema`).

## `-params`

Without `-params` the mode follows `-dialect`: MySQL, TiDB and SQLite use `unnamed` (`?`), PostgreSQL uses `postgresql` (`$1, $2, ...`). `oracle` produces `:param`, `named` keeps `@param` as is, `none` fails on queries with autogenerated parameters.

## `-gen json`

Prints one JSON document with what the built-in generators receive, for a generator written in another language: every query with its function name, result columns, parameters and statement kind; an SQL template that keeps parameters as `Bind` nodes and choices, `IN` lists and dynamic selects as nested nodes; the table catalog; the dialect and the `-params` mode. `-json-schema` prints its JSON Schema.

## `sqlgg ir`

```text
sqlgg ir [-schema <file>] [-plain-sql] [<file.sql> ...]
sqlgg ir -json-schema
```

Prints one JSON document with the parsed AST of every statement, for tooling that wants the SQL itself rather than generated bindings. Input comes from the files, or stdin when there are none. Unlike `-gen json` it does not stop at the first bad statement: a statement that fails to parse or type-check becomes an `Invalid` node with its diagnostics, the rest of the document is still produced, and the exit code is 1. Unreadable input or a schema that does not compile exits with 2.

With `-schema <file>` statements are also checked against that schema and carry the typed result; `-plain-sql` rejects sqlgg extensions; `-json-schema` prints the JSON Schema of the document. `ir` is a separate command with its own options, so the code generation options above do not apply to it.

## `sqlgg migrations`

```bash
sqlgg migrations generate --dialect mysql --lang ocaml \
  --schema schema.sql --initial initial.sql \
  --migrations migrations.sql --module Migrations \
  --output migrations.ml
```

`generate` updates the migration log and generates a module for the OCaml
migration runtime. See the [OCaml migration
workflow](./migrations.md#ocaml-migration-workflow) for the files, runtime and
deployment model.

```bash
sqlgg migrations generate --help
```

The command help is the source of truth for all options. Usage errors exit with
2.

`--config <file>` reads options from the `migrations` object of a
[configuration file](#configuration-file) and `--dialect` from its top-level
`dialect` (which has no default here). It ignores other top-level keys and
rejects unknown keys inside `migrations`. Command-line flags override the file,
and relative paths resolve from the configuration file's directory. `--now` has
no JSON key and stays on the command line.

| Key in `migrations` | Option | Value | Default |
| --- | --- | --- | --- |
| `lang` | `--lang` | `"ocaml"`, `"cxx"`, `"csharp"`, `"java"` (only `"ocaml"` has a runtime) | required |
| `schema` | `--schema` | path | required |
| `initial` | `--initial` | path | none |
| `baseline` | `--baseline` | migration id | none |
| `migrations` | `--migrations` | path | required |
| `manual` | `--manual` | path | none |
| `module` | `--module` | OCaml module name | required |
| `new_tables` | `--ddl-as-migration` | `"schema"`, `"migrations"` | `"schema"` |
| `output` | `--output` | path | stdout |

```json title="sqlgg.json"
{
  "dialect": "mysql",
  "migrations": {
    "lang": "ocaml",
    "schema": "schema.sql",
    "initial": "initial.sql",
    "migrations": "migrations.sql",
    "module": "Migrations",
    "output": "migrations.ml"
  }
}
```

```bash
sqlgg migrations generate --config sqlgg.json
```

## `-static-header`

Drops the version and timestamp from the header, so the generated file only changes when the SQL does. Use it with `dune` `promote` rules (see [Getting Started](./getting-started.md)).
