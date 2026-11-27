# Typed Relation Slots Implementation Plan

> **Status:** executed. The approach changed in places while implementing; see
> "Implementation status" in `docs/superpowers/specs/2026-11-27-typed-relation-slots-design.md`.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add OCaml-only `FROM @source AS alias` relation slots whose generated abstract `Query.Source.t` values safely select among shape-compatible tables and reusable queries.

**Architecture:** Parse relation-position parameters as a distinct AST source, analyze them through statement-scoped symbolic column witnesses, and match each slot independently against an ordered relation registry. Generated OCaml uses a hidden per-slot GADT interpreted into a private descriptor, while a deferred SQL template renderer flattens selected relations before assigning placeholders and bind operations.

**Tech Stack:** OCaml, Dune, Menhir, OUnit, QCheck, Cram, `ppx_deriving`.

## Global Constraints

- Do not commit or push any implementation or plan changes.
- Preserve the user's existing uncommitted changes in `lib/sql.ml`, `lib/syntax.ml`, `src/test.ml`, and `test/cram/not_null_refine.t`.
- First release supports direct OCaml and OCaml IO generation only.
- Existing `&query` reusable CTE behavior must remain unchanged.
- A caller can only construct relation choices through generated builders; no raw table-name or SQL-string input.
- Repeated occurrences of one slot name share one source value and intersect their constraints.
- Candidate matching must not enumerate the Cartesian product of multiple slots.
- PostgreSQL placeholders are assigned only after all runtime relation templates are selected and flattened.
- New domain types are abstract or genuine records/variants; add no module, type, function, value, import, or export aliases.
- Keep one-use helpers local. Use existing PPX derivers for equality/show generation instead of handwritten boilerplate.
- Run the typed-design checklist over every task's diff before moving to the next task.
- Stop after each task for the user to inspect the uncommitted code.

---

## File Structure

### New library modules

- `lib/relation_slot.mli` / `lib/relation_slot.ml`
  - Owns statement-scoped slot identities, symbolic column witnesses, constraint unification, candidate matching, and immutable analyzed views consumed by generators.
- `lib/relation_candidate.mli` / `lib/relation_candidate.ml`
  - Owns checked physical-table and reusable-query candidates, deterministic naming, declaration order, and typed mismatch reasons.

### New OCaml runtime module

- `impl/ocaml/sqlgg_template.mli` / `impl/ocaml/sqlgg_template.ml`
  - Owns deferred SQL text/bind/splice templates and final placeholder rendering.
- `impl/ocaml/sqlgg_relation.mli` / `impl/ocaml/sqlgg_relation.ml`
  - Owns canonical abstract phantom types for built-in SQL kinds, nullability, and built-in representations.

### Existing files modified

- `lib/sql.ml` — relation-slot source constructor, occurrence var, and source-aware output evidence.
- `lib/sql_parser.mly` — contextual `PARAM` source grammar.
- `lib/dialect.ml` — exhaustive relation-slot traversal.
- `lib/shared_queries.ml` — ordered reusable-query entries with statement properties.
- `lib/compile.ml` — registration of complete reusable entries.
- `lib/syntax.ml` / `lib/syntax.mli` — symbolic analysis, rejection rules, candidate evaluation, analyzed slots in `Syntax.result`.
- `lsp/completion.ml`, `lsp/params.ml` — exhaustive handling and slot parameter presentation.
- `src/main.ml` — transport analyzed slots into `Gen.stmt`.
- `src/gen.ml` — deferred bind nodes and relation-splice IR.
- `src/gen_caml.ml` — sealed query modules, hidden GADTs, descriptors, and runtime templates.
- `src/gen_cxx.ml`, `src/gen_csharp.ml`, `src/gen_java.ml`, `src/gen_xml.ml` — explicit unsupported diagnostics.
- `src/test.ml` — parser, solver, and IR unit tests.
- `test/cram/test_relation_slots/` and `test/cram/relation_slots.t` — generated API, compile-pass/fail, nested query, reusable query, and placeholder tests.

---

### Task 1: Protect the baseline and parse relation slots

**Files:**
- Modify: `lib/sql.ml:563-575, 708-710`
- Modify: `lib/sql_parser.mly:304-339`
- Modify: `lib/dialect.ml:117-124, 257-299`
- Modify: `lsp/completion.ml:150-165`
- Modify: `src/test.ml:3063-3210`

**Interfaces:**
- Produces: ``Sql.source_kind`` gains `` `RelationSlot of Sql.param_id ``.
- Produces: parser accepts named `PARAM` only in `source1`; unnamed `?` remains invalid.
- Consumes: existing `PARAM` lexer token; no lexer change.

- [ ] **Step 1: Capture a fresh baseline**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
```

Expected: both commands exit `0`. If either fails, diagnose the pre-existing uncommitted changes before editing relation-slot code.

- [ ] **Step 2: Add parser-focused failing tests**

Add one local OUnit test group in `src/test.ml` that directly inspects `Parser.parse_stmt`:

```ocaml
let test_relation_slot_parser =
  let parses_from_slot =
    "FROM named parameter" >:: fun () ->
      match (Parser.parse_stmt "SELECT COUNT(1) FROM @source AS source").stmt with
      | Select {
          select_complete = {
            select = ({ from = Some (((`RelationSlot id, Some alias), [])); _ }, []);
            _;
          };
          _;
        } ->
        assert_equal (Some "source") id.value;
        assert_equal "source" alias.table_name.value.tn
      | stmt -> assert_failure (Sql.show_stmt stmt)
  in
  let scalar_stays_scalar =
    "scalar parameter" >:: fun () ->
      match (Parser.parse_stmt "SELECT @value").stmt with
      | Select _ -> ()
      | stmt -> assert_failure (Sql.show_stmt stmt)
  in
  [ parses_from_slot; scalar_stays_scalar ]
```

Register `"relation slot parser" >::: test_relation_slot_parser` in `run`.

- [ ] **Step 3: Run the parser test to verify red**

Run:

```bash
dune build
```

Expected: compile failure because `` `RelationSlot`` does not exist or Menhir rejects `PARAM` in `source1`.

- [ ] **Step 4: Add the AST and grammar constructor**

Extend `source_kind` without touching the user's `assignment_domain` change:

```ocaml
and source_kind =
  [ `Select of select_full
  | `Table of table_name
  | `Nested of nested
  | `ValueRows of row_values
  | `RelationSlot of param_id
  ]
```

Extend only `source1`:

```ocaml
source1:
       | table_name { `Table $1 }
       | id=PARAM { `RelationSlot id }
       | LPAREN s=select_stmt RPAREN { `Select s }
       | LPAREN s=table_list RPAREN { `Nested s }
       | LPAREN s=values_stmt RPAREN { `ValueRows s }
```

Add exhaustive no-feature traversal branches in `Dialect.analyze_source`,
`Dialect.get_join_source`, and LSP completion. Do not assign a SQL dialect
feature: this is a sqlgg compile-time construct.

- [ ] **Step 5: Add a temporary semantic rejection**

In `Syntax.eval_source`, add a branch that reports a positioned error:

```ocaml
| `RelationSlot id ->
  failed ~at:id.pos "relation slot analysis is not available yet"
```

This keeps the repository buildable while later tasks replace the branch with
symbolic analysis.

- [ ] **Step 6: Verify green**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
```

Expected: exit `0`; direct parser tests pass and ordinary scalar parameters remain unchanged.

---

### Task 2: Make candidate registries ordered and typed

**Files:**
- Modify: `lib/shared_queries.ml`
- Modify: `lib/compile.ml:19-31, 33-48`
- Modify: `lib/syntax.ml:1718-1724`
- Modify: `src/gen.ml:185-192`
- Create: `lib/relation_candidate.mli`
- Create: `lib/relation_candidate.ml`
- Modify: `src/test.ml`

**Interfaces:**
- Produces:

```ocaml
module Shared_queries : sig
  type entry = private {
    name : string;
    sql : string;
    select : Sql.select_full;
    props : Props.t list;
  }

  type t

  val add :
    name:string ->
    sql:string ->
    select:Sql.select_full ->
    props:Props.t list ->
    unit
  val get : string -> entry
  val entries : unit -> entry list
  val mem : string -> bool
  val snapshot : unit -> t
  val restore : t -> unit
end
```

- Produces:

```ocaml
type Relation_candidate.t

type Relation_candidate.origin =
  | Physical_table of Sql.table_name
  | Reusable_query of Shared_queries.entry
[@@deriving show]

type Relation_candidate.view = {
  source_name : string;
  builder_name : string;
  schema : Sql.schema;
  origin : origin;
}

val tables_in_declaration_order : unit -> t list
val reusable_in_declaration_order :
  analyze:(Shared_queries.entry -> Sql.schema) ->
  t list
val view : t -> view
```

- Consumes: `Tables.snapshot`, reversing its prepend order exactly once.

- [ ] **Step 1: Write ordered-registry tests**

Add tests that register `q1`, `q2`, create `t1`, `t2`, and assert candidate
order is `t1`, `t2`, `q1`, `q2`. Add a collision test where normalized
`table_public_users` names collide and assert a structured error.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
```

Expected: failure because `Shared_queries.entries` and `Relation_candidate` do not exist.

- [ ] **Step 3: Replace the reusable-query hash alias with a real registry**

Implement `Shared_queries.t` as a record containing a reverse declaration list
and lookup table. `snapshot` must copy both components; `restore` must restore
both. Keep `get` deterministic and update existing call sites to read
`entry.sql` and `entry.select`.

Register full properties from `Compile.statement`:

```ocaml
Shared_queries.add
  ~name:(Option.value ~default:"" (Props.name stmt.props))
  ~sql:stmt.text
  ~select
  ~props:stmt.props
```

- [ ] **Step 4: Implement `Relation_candidate`**

Use abstract `t` in the `.mli`. Validate builder names at construction:

```ocaml
let table_builder_name table =
  "table_" ^ normalize (Sql.show_table_name table)

let query_builder_name name =
  "query_" ^ normalize name
```

Do not use `Gen.Name.idents`, because it silently appends numeric suffixes.
Raise a typed `Name_collision` error containing both SQL names.

- [ ] **Step 5: Verify ordered snapshots and existing reusable CTEs**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
dune test test/cram/open_option.t
dune test test/cram/reusable_queries_postgresql_params.t
```

Expected: all exit `0`; legacy `&query` behavior and `-open` ordering remain unchanged.

---

### Task 3: Add a symbolic witness solver

**Files:**
- Create: `lib/relation_slot.mli`
- Create: `lib/relation_slot.ml`
- Modify: `src/test.ml`

**Interfaces:**
- Produces an abstract statement-scoped solver:

```ocaml
type Relation_slot.t
type Relation_slot.column
type Relation_slot.witness

type Relation_slot.escape =
  | Query_parameter of Sql.param_id
  | Result_column of int
  | Shared_between_slots
[@@deriving show, eq]

type Relation_slot.mismatch =
  | Missing_column of string
  | Type_conflict of { expected : Sql.Type.t; found : Sql.Type.t }
  | Nullability_conflict of {
      expected : Sql.Type.nullability;
      found : Sql.Type.nullability;
    }
  | Metadata_conflict of { column : string }
  | Duplicate_output of string
  | Candidate_not_closed
  | Candidate_has_substitution
  | Candidate_has_dynamic_shape
  | Candidate_has_relation_slot
[@@deriving show]

type Relation_slot.column_view = {
  name : string;
  witness : witness;
}

type Relation_slot.witness_view = {
  id : int;
  constraint_type : Sql.Type.t option;
  metadata : Sql.Meta.t option;
  escapes : escape list;
}

val create : unit -> t
val register_occurrence :
  t ->
  id:Sql.param_id ->
  alias:Sql.table_name ->
  unit
val column :
  t ->
  alias:Sql.table_name ->
  name:string ->
  column
val constrain : t -> column -> Sql.Type.t -> (unit, mismatch) result
val unify : t -> column -> column -> (unit, mismatch) result
val mark_escape : t -> column -> escape -> unit
val columns : t -> slot_name:string -> column_view list
val witnesses : t -> witness_view list
```

The implementation uses union-find internally. A witness stores an optional
concrete `Sql.Type.t`, metadata evidence, and escape marks. No boolean field
represents witness state.

- [ ] **Step 1: Write solver tests**

Add focused OUnit cases:

```ocaml
"concrete constraint propagates through equality"
"conflicting kinds are rejected"
"nullable and strict preserve required evidence"
"same slot column is interned"
"shared columns receive Shared_between_slots"
```

Use `Type.equal` and generated `show_mismatch` printers.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune build
```

Expected: failure because `Relation_slot` is missing.

- [ ] **Step 3: Implement union-find and typed states**

Represent roots with a variant:

```ocaml
type binding =
  | Unbound
  | Bound of Sql.Type.t
```

Merge bindings through `Sql.Type.common_type`; never keep an invalid pair and
validate it later. Merge metadata through exact external-domain metadata, not
through lossy strings.

- [ ] **Step 4: Add candidate matching**

Add:

```ocaml
type match_view = {
  candidate : Relation_candidate.t;
  witness_types : (int * Sql.Type.t * Sql.Meta.t) list;
}

val match_candidate :
  t ->
  slot_name:string ->
  Relation_candidate.t ->
  (match_view, mismatch) result
```

The matcher only checks one slot and one candidate. Shared witness identities
remain in `match_view`, so matching several slots never enumerates candidate
pairs.

- [ ] **Step 5: Verify solver tests**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
```

Expected: exit `0`.

---

### Task 4: Integrate symbolic columns into SQL analysis

**Files:**
- Modify: `lib/syntax.ml:20-150, 217-243, 654-752, 795-1124, 1275-1672, 1690-1743, 2180-2391`
- Modify: `lib/syntax.mli:25-35`
- Modify: `lib/sql.ml:637-653, 1156-1175`
- Modify: `src/test.ml`

**Interfaces:**
- Produces:

```ocaml
type Relation_slot.analyzed

type Syntax.result = {
  sql : string;
  schema : Sql.schema_column list;
  vars : Sql.var list;
  relation_slots : Relation_slot.analyzed list;
  kind : Stmt.kind;
  dialect_features : Dialect.dialect_support list;
  annotations : stmt_annotations;
}
```

- `res_expr` gains:

```ocaml
| ResRelationColumn of Relation_slot.column
```

- `Sql.var` gains one occurrence per relation-position token:

```ocaml
| RelationSlot of {
    id : param_id;
    alias : table_name;
  }
```

- [ ] **Step 1: Write failing semantic tests**

Add tests for:

```sql
SELECT COUNT(1) FROM @source AS source
SELECT source.id FROM @source AS source
SELECT COUNT(1) FROM @source AS source WHERE source.id = @id
SELECT l.id FROM @left_source AS l JOIN @right_source AS r ON l.id = r.id
SELECT u.id FROM users AS u WHERE EXISTS (
  SELECT 1 FROM @events AS e WHERE e.user_id = u.id
)
```

Add rejection tests for missing alias, scalar/slot name collision,
`source.*`, unqualified `*`, `NATURAL JOIN`, `JOIN ... USING`, and recursive
CTE placement.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
```

Expected: relation-slot statements fail at the temporary rejection.

- [ ] **Step 3: Add the statement-scoped solver to `env`**

Extend `env` with one `Relation_slot.t`. Create it once in `eval_parsed`; nested
queries receive the same solver so outer and inner constraints can unify.

In `resolve_source`, require an alias and register the occurrence. Build a
symbolic source whose columns are created lazily by qualified lookup rather
than pretending the source has one `Any` schema.

- [ ] **Step 4: Preserve slot identity in expression analysis**

When `resolve_column` sees a qualified slot alias, return a slot-backed source
attribute. `resolve_columns` produces `ResRelationColumn` instead of erasing it
to `ResValue`.

Extend `apply_expected_type`:

```ocaml
| ResRelationColumn column ->
  Relation_slot.constrain_exn env.relation_slots column expected;
  expr
```

Extend function signature inference so arguments sharing one `Type.Var`
unify their relation columns before concrete constraints are applied.

- [ ] **Step 5: Mark public escapes**

When extracting query parameters and output schema, mark relation witnesses
used by:

- scalar parameters unified with a slot column;
- output columns whose decoder depends on a slot;
- constraints shared by two distinct slot names.

Do not mark server-only constrained columns such as an integer column used
only inside `COUNT(column)`.

- [ ] **Step 6: Evaluate and match ordered candidates**

For each reusable entry, evaluate its `SELECT` in the current ordered schema
environment to obtain fixed schema and vars. Reject candidates containing
`subst`, `dynamic_select`, unresolved relation slots, duplicate required
outputs, or free outer references.

Call `Relation_slot.match_candidate` independently for each slot. If no
candidate matches, report required columns plus at most five declaration-order
near misses.

- [ ] **Step 7: Verify analysis and existing join behavior**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
dune test test/cram/not_null_refine.t
```

Expected: all exit `0`; the user's outer-join nullability tests remain green.

---

### Task 5: Add deferred SQL templates

**Files:**
- Create: `impl/ocaml/sqlgg_template.mli`
- Create: `impl/ocaml/sqlgg_template.ml`
- Modify: `src/gen.ml:73-269`
- Modify: `src/test.ml:3063-3114`
- Add: `test/cram/test_relation_slots/template_test.ml`
- Add: `test/cram/test_relation_slots/dune`

**Interfaces:**
- Produces:

```ocaml
type Sqlgg_template.placeholder_style =
  | Named
  | Oracle
  | Unnamed
  | PostgreSQL

type 'params t

type 'params rendered = private {
  sql : string;
  count : int;
  set : 'params -> unit;
}

val text : string -> 'params t
val bind :
  name:string option ->
  set:('params -> unit) ->
  'params t
val concat : 'params t list -> 'params t
val choose : 'params t -> 'params t
val render :
  placeholder_style ->
  'params t ->
  'params rendered
```

`choose` is a semantic constructor for a runtime-selected branch, not a
passthrough alias. Empty templates render no SQL and no binds.

- `Gen.sql` gains:

```ocaml
| Bind of Sql.Type.t Sql.param * Sql.Meta.t
| RelationSplice of Sql.param_id
```

- Produces `Gen.get_sql_template`, while existing `Gen.get_sql` remains
unchanged for statements without relation slots.

- [ ] **Step 1: Write runtime renderer tests**

Cover:

- named, Oracle, unnamed, and PostgreSQL placeholders;
- nested `concat`;
- a relation splice between two outer binds;
- repeated splice;
- bind setter order equals rendered placeholder order.

- [ ] **Step 2: Run runtime tests to verify red**

Run:

```bash
dune test test/cram/test_relation_slots
```

Expected: failure because `Sqlgg_template` does not exist.

- [ ] **Step 3: Implement the abstract template algebra**

Keep representation hidden in `.mli`. Internally represent states with
constructors for text, bind, concatenation, and selected splice. `render`
performs exactly one left-to-right traversal, numbering PostgreSQL binds from
`$1`.

- [ ] **Step 4: Refactor `Gen.substitute_vars` through one local traversal**

Keep current static rendering as one interpretation and add deferred
rendering as another. Do not duplicate choice, optional-clause, tuple-list, or
shared-query traversal.

`RelationSlot` consumes exactly the `@source` token range and leaves `AS alias`
in the surrounding static text.

- [ ] **Step 5: Verify existing generator output is unchanged**

Run:

```bash
dune build
./_build/default/src/cli.exe -test
dune test test/cram/test.t
```

Expected: exit `0`; non-slot golden output is byte-for-byte unchanged.

---

### Task 6: Generate sealed table-only source modules

**Files:**
- Create: `impl/ocaml/sqlgg_relation.mli`
- Create: `impl/ocaml/sqlgg_relation.ml`
- Modify: `src/main.ml:42-49`
- Modify: `src/gen.ml:11`
- Modify: `src/gen_caml.ml:317-375, 826-1040, 1144-1288`
- Add: `test/cram/relation_slots.t`
- Add: `test/cram/test_relation_slots/basic.sql`
- Add: `test/cram/test_relation_slots/basic.compare.ml`

**Interfaces:**
- `Gen.stmt` gains:

```ocaml
relation_slots : Relation_slot.analyzed list;
```

- `Sqlgg_relation.mli` exposes abstract phantom types:

```ocaml
type int_kind
type uint64_kind
type text_kind
type blob_kind
type float_kind
type bool_kind
type datetime_kind
type decimal_kind
type json_kind
type required
type nullable
type builtin_representation
```

- Generated table-only API:

```ocaml
module Count_from : sig
  module Source : sig
    type t
    val table_users : t
    val table_orders : t
  end

  val select :
    T.connection ->
    source:Source.t ->
    int64 T.io_future
end = struct
  ...
end
```

- [ ] **Step 1: Write the table-only golden test**

Use:

```sql
CREATE TABLE users (id INT NOT NULL);
CREATE TABLE orders (id INT NOT NULL);

-- @count_from
SELECT COUNT(1) FROM @source AS source;
```

Assert the generated module is sealed, exposes abstract `Source.t`, and
contains `table_users` and `table_orders`.

- [ ] **Step 2: Run the golden test to verify red**

Run:

```bash
dune test test/cram/relation_slots.t
```

Expected: generated OCaml lacks the sealed query module.

- [ ] **Step 3: Transport analyzed slots to the generator**

Add `relation_slots` in `Syntax.result`, `Main.stmt_of_syntax`, and `Gen.stmt`.
Keep all constructors exhaustive.

- [ ] **Step 4: Emit the sealed query module**

For slot statements only, emit:

```ocaml
module Query_name : sig
  module Slot_name : sig
    type t
    val table_candidate : t
  end
  val select : ...
end = struct
  module Slot_name = struct
    type t = Table_candidate
    ...
  end
  ...
end
```

The ordinary variant is hidden by the outer module signature. Generate no flat
function alias.

- [ ] **Step 5: Interpret table constructors into descriptors**

The private descriptor contains:

```ocaml
type descriptor = {
  relation : T.params Sqlgg_template.t;
}
```

Each physical-table builder maps to a dialect-quoted `Sqlgg_template.text`.
The outer query splices `descriptor.relation`, renders once, then executes.

- [ ] **Step 6: Verify direct and IO generation**

Run:

```bash
dune build
dune test test/cram/relation_slots.t
```

Expected: direct and IO generated fixtures compile and the SQL differs only by
the chosen table.

---

### Task 7: Generate dependent GADT evidence

**Files:**
- Modify: `lib/relation_slot.mli`
- Modify: `lib/relation_slot.ml`
- Modify: `src/gen_caml.ml`
- Add: `test/cram/test_relation_slots/dependent.sql`
- Add: `test/cram/test_relation_slots/dependent_compile.ml`
- Modify: `test/cram/relation_slots.t`

**Interfaces:**
- A witness that escapes contributes four GADT indices:
  - SQL kind;
  - nullability;
  - representation/codec identity;
  - decoded OCaml type.
- Query-local abstract phantom types are generated for enum and custom-codec
  identities not covered by `Sqlgg_relation`.

- Example generated API:

```ocaml
module Select_id : sig
  module Source : sig
    type (_, _, _, _) t

    val table_users :
      ( Sqlgg_relation.int_kind,
        Sqlgg_relation.required,
        Sqlgg_relation.builtin_representation,
        int64 )
      t

    val table_drafts :
      ( Sqlgg_relation.int_kind,
        Sqlgg_relation.nullable,
        Sqlgg_relation.builtin_representation,
        int64 option )
      t
  end

  val select :
    T.connection ->
    source:('kind, 'nullability, 'representation, 'id) Source.t ->
    (id:'id -> 'result) ->
    'result list T.io_future
end
```

- [ ] **Step 1: Write compile-pass and compile-fail fixtures**

Cover:

- `SELECT source.id`;
- `WHERE source.id = @id`;
- two slots joined by equal column types;
- incompatible `int`/`text` source pair rejected by OCaml;
- nullable and strict result types;
- same host type but different SQL kind not unified.

- [ ] **Step 2: Run fixtures to verify red**

Run:

```bash
dune test test/cram/relation_slots.t
```

Expected: missing parameterized `Source.t` or generated type mismatch.

- [ ] **Step 3: Emit hidden GADT constructors**

Use locally abstract types in the interpreter:

```ocaml
let describe :
  type kind nullability representation value.
  (kind, nullability, representation, value) Source.t ->
  (kind, nullability, representation, value) descriptor =
  function
  | Table_users -> ...
  | Table_drafts -> ...
```

Generate query-local nominal phantom types when two candidate domains must not
unify despite sharing one OCaml representation.

- [ ] **Step 4: Generate dependent readers and setters**

Private descriptors contain only operations actually needed by the query:

```ocaml
read_id : T.row -> int -> 'id * int;
set_id : T.params -> 'id -> unit;
```

Do not emit a reader for a source column used only in a server-side predicate,
or a setter when no outer parameter depends on it.

- [ ] **Step 5: Verify type-level behavior**

Run:

```bash
dune build
dune test test/cram/relation_slots.t
```

Expected: pass fixtures compile, fail fixtures produce the expected OCaml type
errors, and no runtime validation is generated.

---

### Task 8: Add reusable-query candidates and bind ownership

**Files:**
- Modify: `lib/syntax.ml`
- Modify: `lib/relation_candidate.ml`
- Modify: `src/gen_caml.ml`
- Add: `test/cram/test_relation_slots/reusable.sql`
- Add: `test/cram/test_relation_slots/reusable_runtime.ml`
- Modify: `test/cram/relation_slots.t`

**Interfaces:**
- Generated reusable builder:

```ocaml
val query_active_users :
  status:status ->
  (...) Source.t
```

- Builder captures candidate parameters in its hidden GADT constructor.
- Descriptor interpretation returns a nested deferred template; outer query
  receives no optional arguments for candidate-only parameters.

- [ ] **Step 1: Write reusable-source tests**

Cover a parameterless reusable query, a parameterized query, repeated use of
one source value, a choice/list parameter inside the reusable query, and one
candidate excluded for each eligibility reason.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune test test/cram/relation_slots.t
```

Expected: only physical-table builders exist.

- [ ] **Step 3: Generate reusable smart builders**

Capture source-owned arguments in hidden constructors. The public builder
performs real template construction and is not a function alias.

- [ ] **Step 4: Splice reusable templates and preserve bind order**

Test PostgreSQL:

```sql
SELECT @before
FROM @source AS source
WHERE source.id = @after;
```

When the selected reusable source contains one bind, rendered placeholders
must be `$1` for `before`, `$2` inside the source, and `$3` for `after`, in
actual final SQL order.

- [ ] **Step 5: Verify legacy reuse and new source reuse together**

Run:

```bash
dune build
dune test test/cram/relation_slots.t
dune test test/cram/reusable_queries_postgresql_params.t
dune test test/cram/open_option.t
```

Expected: all exit `0`.

---

### Task 9: Complete nested, repeated, multiple-slot, and dynamic-select composition

**Files:**
- Modify: `lib/syntax.ml`
- Modify: `src/gen_caml.ml`
- Modify: `impl/ocaml/ppx/ppx_sqlgg.ml` only if existing derived projections require a type annotation update
- Add: `test/cram/test_relation_slots/composition.sql`
- Add: `test/cram/test_relation_slots/composition_compile.ml`
- Modify: `test/cram/relation_slots.t`

**Interfaces:**
- Repeated slot names produce one source module and one function argument.
- Distinct slots produce distinct source modules with shared witness type
  variables where SQL constraints connect them.
- Dynamic-select column descriptors remain a separate function argument.

- [ ] **Step 1: Write composition tests**

Cover:

- same slot twice under two aliases;
- two independently selected slots;
- shared equality witness between slots;
- slot inside correlated `EXISTS`;
- slot inside a non-recursive CTE;
- relation slot plus `dynamic_select`;
- `[@@deriving sqlgg]` projection against the dynamic-select argument.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune test test/cram/relation_slots.t
```

Expected: one or more composition fixtures fail.

- [ ] **Step 3: Deduplicate occurrences by slot name**

Generate one `Source` module and one function parameter per unique slot name.
Keep occurrence-specific aliases and source positions only in the deferred
template IR.

- [ ] **Step 4: Share witness variables between slot modules**

When symbolic analysis unifies two slot columns, emit one outer type variable
across both source arguments. Do not generate pair constructors or inspect
runtime combinations.

- [ ] **Step 5: Compose with dynamic select**

Keep source descriptors and `Sqlgg_scope` column descriptors independent:

```ocaml
val select :
  T.connection ->
  source:(...) Source.t ->
  'projection Sqlgg_scope.t ->
  ...
```

Template bind traversal must interleave relation-source binds and selected
column binds in final SQL order.

- [ ] **Step 6: Verify all composition fixtures**

Run:

```bash
dune build
dune test test/cram/relation_slots.t
dune runtest test/ppx_expansion
```

Expected: all exit `0`.

---

### Task 10: Add diagnostics, unsupported generators, LSP, and property oracle

**Files:**
- Modify: `lib/relation_slot.ml`
- Modify: `lib/syntax.ml`
- Modify: `lsp/completion.ml`
- Modify: `lsp/params.ml`
- Modify: `src/gen.ml`
- Modify: `src/gen_cxx.ml`
- Modify: `src/gen_csharp.ml`
- Modify: `src/gen_java.ml`
- Modify: `src/gen_xml.ml`
- Modify: `test/property_based/test_property_based.ml`
- Modify: `test/cram/relation_slots.t`
- Modify: `docs/superpowers/specs/2026-11-27-typed-relation-slots-design.md` only where implementation evidence changed the design

**Interfaces:**
- Non-OCaml generators fail with:

```text
relation slot @source is supported only by the OCaml generators
```

- No-candidate diagnostics show slot requirements and at most five near misses
in declaration order.

- [ ] **Step 1: Write diagnostic and non-OCaml cram cases**

Assert exact messages for no alias, no candidates, missing column, type clash,
duplicate output, relation slot in recursive CTE, `source.*`, `NATURAL`,
`USING`, unresolved nested slot, `subst`, dynamic shape, and every non-OCaml
generator.

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
dune test test/cram/relation_slots.t
```

Expected: missing or unstable diagnostics.

- [ ] **Step 3: Centralize relation-slot diagnostics**

Render `Relation_slot.mismatch` exhaustively in one function. Keep original
SQL names and positions. Do not catch typed mismatches and replace them with a
generic string.

- [ ] **Step 4: Add explicit generator capability checks**

Reject before generator-specific parameter extraction. Do not let
`Gen.get_sql_string_only` emit a generic “dynamic choices unsupported”
message for relation slots.

- [ ] **Step 5: Add LSP behavior**

Show relation slots as source parameters, preserve alias completion for
candidate-intersection columns, and keep scalar parameter hover behavior
unchanged.

- [ ] **Step 6: Add the exhaustive property oracle**

Generate small schemas with up to four tables and two slots. Compare:

1. symbolic per-slot candidate sets and shared witnesses;
2. full physical substitution followed by normal `Syntax.eval_parsed`.

The property fails on any candidate-set or type-equality difference.

- [ ] **Step 7: Update design only from implementation evidence**

If an implemented invariant differs from the approved spec, edit the exact
spec section and explain the concrete compiler/runtime reason. Do not broaden
scope to future fragments.

- [ ] **Step 8: Run full verification**

Run:

```bash
dune build
dune runtest
dune test test/cram
./run_test
git diff --check
```

Expected:

- build and Dune tests exit `0`;
- all Cram tests exit `0`;
- `./run_test` reports no unintended XML golden changes;
- `git diff --check` reports no whitespace errors;
- all implementation remains uncommitted for user review.

