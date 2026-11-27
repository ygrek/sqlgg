---
sidebar_position: 13
title: Relation Slots
description: Choose the table or reusable query of FROM / JOIN at runtime, type-safely
---

# Relation Slots

A relation slot leaves the relation of a `FROM` or `JOIN` open. sqlgg finds every
loaded table and [reusable query](./cte.md#reusable-queries) that fits the query and
generates one value per candidate. The caller picks one of these values, never a
table name string.

```sql
CREATE TABLE users (id INT NOT NULL, name TEXT);
CREATE TABLE drafts (id INT, name TEXT NOT NULL);
CREATE TABLE logs (msg TEXT NOT NULL);

-- @count_from
SELECT COUNT(1) AS n FROM @source AS source;
```

```ocaml
module Count_from : sig
  module Source : sig
    type t = private
      | Table_users : t
      | Table_drafts : t
      | Table_logs : t
    val table_users : t
    val table_drafts : t
    val table_logs : t
  end
  val select : [> `RO ] T.connection -> source:Source.t -> T.Types.Int.t
end

let n = Count_from.select db ~source:Count_from.Source.table_drafts
```

`Source.t` is private: it can be matched on but only built through the generated
values. Input that names a relation must be parsed by the application into its own
choice and mapped to one of them.

OCaml only, for now. Other generators reject a statement with a relation slot.

## Types that depend on the choice

When the type of a result column or of a parameter depends on the chosen relation,
it becomes an index of `Source.t`:

```sql
-- @select_id
SELECT source.id FROM @source AS source;
```

```ocaml
module Source : sig
  type _ t = private
    | Table_users : T.Types.Int.t t
    | Table_drafts : T.Types.Int.t option t
end
val select : [> `RO ] T.connection -> source:'a Source.t -> (id:'a -> unit) -> unit
```

`logs` has no `id` and is not offered. A parameter compared with such a column
gets the same index: `WHERE source.id = @id` makes `~id` of type `'a`.

## Reusable queries

A reusable query is a candidate too. Its parameters belong to its value, not to the
outer function:

```sql
-- @active_users | include: reuse
SELECT id, status FROM users WHERE status = @status;

-- @pick
SELECT s.id FROM @src AS s WHERE s.id > @after;
```

```ocaml
Pick.select db ~src:(Pick.Src.query_active_users ~status:"on") ~after:5L callback
```

The relation is inserted as a parenthesized subquery and its values are bound in
the order of the final SQL. PostgreSQL placeholders (`$1`, `$2`, ...) are numbered
after the relation is chosen.

Only reusable queries with plain named parameters of built-in types are offered;
those using `subst`, `dynamic_select`, choices, lists, enums or custom codecs are not.

## Several slots

Different names are chosen independently. When the query relates their columns,
the SQL kind of those columns is a shared index, so incompatible relations do not
type-check together:

```sql
-- @pair
SELECT COUNT(1) AS n FROM @left_src AS l JOIN @right_src AS r ON l.id = r.id;
```

```ocaml
val select : [> `RO ] T.connection -> left_src:'x Left_src.t -> right_src:'x Right_src.t -> T.Types.Int.t
(* Left_src.table_users : [ `Int ] Left_src.t, Right_src.table_tags : [ `Text ] Right_src.t *)
```

The same name used twice is a single choice (`FROM @src AS child JOIN @src AS parent`).
A slot may appear in a subquery, including a correlated one.

The check is conservative: two columns must have the same SQL kind, even where SQL
would convert between them, and an expression mixing columns of two slots
(`l.a + r.b`) is only accepted when both sides agree on its type.

## Restrictions

- an alias is required: `FROM @source AS source`;
- columns of a slot are referenced through its alias;
- `SELECT *` and `alias.*` over a slot, `NATURAL JOIN` and `JOIN ... USING` with a slot
  are rejected;
- relation slots are rejected in recursive CTEs, in statements other than `SELECT`,
  and together with `dynamic_select`;
- a slot name cannot also be used as a parameter or choice;
- inline SQL (`[%sql]`) rejects a statement with a relation slot;
- a reusable query that has a relation slot cannot be used as a CTE (`WITH x AS &query`);
- a type that depends on the relation chosen has to be a result column or a named
  parameter: `IN @list` over a column typed differently by two relations and
  unnamed `?` parameters in that position are rejected;
- a statement with several slots has to type-check before any relation is chosen;
  with one slot, constructs that need the column types (`(s.a, s.b) IN @pairs`) work;
- the chosen table is written into the SQL quoted and in lower case, the way sqlgg
  stores identifiers: on a case-sensitive database a table whose real name has
  upper-case letters is not found.
