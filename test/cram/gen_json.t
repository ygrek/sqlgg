-gen json emits the statements a generator would receive, with their SQL
templates, and the table catalog. -open loads the catalog without emitting its
statements. Templates keep binds with their original spelling, so a generator
picks its own placeholder syntax:

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT, KEY by_name (name));
  > SQL
  $ cat > find.sql <<'SQL'
  > -- @find
  > SELECT id, name FROM users WHERE id = @id AND name IN @names;
  > SQL
  $ sqlgg -static-header -dialect postgresql -name db -open schema.sql -gen json find.sql | ydump
  {
    "sqlgg_version": null,
    "module_name": "db",
    "dialect": "PostgreSQL",
    "params": "PostgreSQL",
    "queries": [
      {
        "name": "find",
        "stmt": {
          "sql": "SELECT id, name FROM users WHERE id = @id AND name IN @names",
          "schema": [
            [
              "Attr",
              {
                "name": "id",
                "domain": { "t": "Int", "nullability": "Strict" },
                "extra": [ "PrimaryKey", "NotNull" ],
                "meta": {}
              }
            ],
            [
              "Attr",
              {
                "name": "name",
                "domain": { "t": "Text", "nullability": "Strict" },
                "extra": [],
                "meta": {}
              }
            ]
          ],
          "vars": [
            [
              "Single",
              {
                "id": { "value": "id", "pos": [ 38, 41 ] },
                "typ": { "t": "Int", "nullability": "Strict" }
              },
              {}
            ],
            [
              "ChoiceIn",
              {
                "param": { "value": "names", "pos": [ 46, 60 ] },
                "kind": "In",
                "vars": [
                  [
                    "SingleIn",
                    {
                      "id": { "value": "names", "pos": [ 54, 60 ] },
                      "typ": { "t": "Text", "nullability": "Strict" }
                    },
                    {}
                  ]
                ]
              }
            ]
          ],
          "kind": [ "Select", "Zero_one" ],
          "props": [ [ "File", "find.sql" ], [ "Name", "find" ] ]
        },
        "template": [
          [ "Text", "SELECT id, name FROM users WHERE id = " ],
          [
            "Bind",
            {
              "param": {
                "id": { "value": "id", "pos": [ 38, 41 ] },
                "typ": { "t": "Int", "nullability": "Strict" }
              },
              "original": "@id"
            }
          ],
          [ "Text", " AND " ],
          [
            "DynamicIn",
            { "value": "names", "pos": [ 46, 60 ] },
            "In",
            [
              [ "Text", "name IN " ],
              [
                "SubstIn",
                {
                  "id": { "value": "names", "pos": [ 54, 60 ] },
                  "typ": { "t": "Text", "nullability": "Strict" }
                },
                {}
              ]
            ]
          ]
        ]
      }
    ],
    "tables": [
      {
        "name": { "db": null, "tn": "users" },
        "columns": [
          {
            "attr": {
              "name": "id",
              "domain": { "t": "Int", "nullability": "Strict" },
              "extra": [ "PrimaryKey", "NotNull" ],
              "meta": {}
            },
            "source_kind": {
              "collated": [
                "Int",
                { "size": null, "sign": "Signed", "display_width": null }
              ],
              "collation": null
            },
            "default_sql": null
          },
          {
            "attr": {
              "name": "name",
              "domain": { "t": "Text", "nullability": "Nullable" },
              "extra": [],
              "meta": {}
            },
            "source_kind": {
              "collated": [ "Text", [ "PlainText", null ] ],
              "collation": null
            },
            "default_sql": null
          }
        ],
        "tbl_charset": null,
        "tbl_ttl": null,
        "tbl_indexes": {
          "by_name": { "kind": "Plain_idx", "cols": [ "name" ] }
        },
        "tbl_foreign_keys": []
      }
    ]
  }

A noparse statement is kept verbatim:

  $ cat > raw.sql <<'SQL'
  > -- [sqlgg] noparse
  > -- @raw
  > SELECT whatever(1);
  > SQL
  $ sqlgg -static-header -gen json raw.sql | ydump
  {
    "sqlgg_version": null,
    "module_name": "sqlgg",
    "dialect": "MySQL",
    "params": null,
    "queries": [
      {
        "name": "raw",
        "stmt": {
          "sql": "SELECT whatever(1)",
          "schema": [],
          "vars": [],
          "kind": "Other",
          "props": [ [ "File", "raw.sql" ], [ "Name", "raw" ], "Noparse" ]
        },
        "template": [ [ "Text", "SELECT whatever(1)" ] ]
      }
    ],
    "tables": []
  }

Only the statements a generator would get are emitted: reusable queries are
dropped, dynamic_select=both adds the static companion, -category filters:

  $ cat > queries.sql <<'SQL'
  > -- @active | include: reuse
  > SELECT id FROM users WHERE name IS NOT NULL;
  > -- [sqlgg] dynamic_select=both
  > -- @get_user
  > SELECT id, name FROM users WHERE id = @id;
  > -- @rename
  > UPDATE users SET name = @name WHERE id = @id;
  > SQL
  $ sqlgg -open schema.sql -gen json queries.sql | grep -o '{"name":"[^"]*","stmt"'
  {"name":"get_user","stmt"
  {"name":"get_user_static","stmt"
  {"name":"rename","stmt"
  $ sqlgg -open schema.sql -category dml -gen json queries.sql | grep -o '{"name":"[^"]*","stmt"'
  {"name":"rename","stmt"

The version is reported unless -static-header or -no-header drop it, as for
the other generators:

  $ sqlgg -open schema.sql -gen json find.sql | grep -c '"sqlgg_version":"'
  1
  $ sqlgg -no-header -open schema.sql -gen json find.sql | grep -o '"sqlgg_version":null'
  "sqlgg_version":null

-params overrides the placeholder mode detected from the dialect:

  $ sqlgg -dialect postgresql -params named -open schema.sql -gen json find.sql | grep -o '"params":"[^"]*"'
  "params":"Named"

-json-schema prints the JSON Schema of that output:

  $ sqlgg -json-schema | grep -F '"$schema"'
    "$schema": "https://json-schema.org/draft/2020-12/schema",

Migrations have no JSON form:

  $ sqlgg -gen json -diff -base schema.sql -target schema.sql
  Fatal error: exception Failure("migrations not supported for JSON")
  [2]
