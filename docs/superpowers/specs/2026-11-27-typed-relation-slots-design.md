# Typed relation slots

## Status

Approved design for an OCaml-only first release.

## Implementation status

Implemented in the `feature/relation-slots` worktree, uncommitted for review.
Where the implementation departs from the sections below, this section wins.

### Changed while implementing

- **Source type is a private GADT, not an abstract type.** Callers can match on
  `Query.Source.t` but cannot construct it; only the generated builder values do.
  A fully abstract type would require an explicit signature for every generated
  function, which the generator does not produce. The descriptor of the chosen
  relation is a set of GADT functions emitted once per statement module inside
  `open struct ... end`, so it is still not exported.
- **No union-find witness solver.** Each slot is checked against each candidate by
  the existing type checker, with that slot bound to the candidate and the other
  slots open. Open columns record the types expected of them; that gives the
  cross-slot constraints. The work is linear in the number of candidates, no
  Cartesian product. Whether a result column or parameter depends on the choice
  is decided by comparing its type across these evaluations. The first evaluation,
  with every slot open, records the slot columns the statement uses; a candidate
  lacking one of them is rejected without evaluating the statement.
- **No mutable state.** The slots of a statement are found when it is parsed
  (`Parser.parse_result.relation_slots`, by the walk that collects dialect
  features). The slot an evaluation binds travels in its environment. The columns
  of open slots it uses, with the SQL kinds expected of them, come back in its
  annotations and are ordered by their position in the text, which also orders
  the coupling indices. Of an evaluation with a slot bound only the types and the
  cardinality are kept; the statement itself is evaluated once more for the first
  relation left.
- **What the open evaluation is used for.** An expectation it already records
  (`l.created > '2020-01-01'`) holds whatever is chosen, so it is subtracted from
  the expectations of every bound evaluation: only the rest couples two slots.
  When it fails, a statement with one slot is still analysed, each relation may
  make it type-check. A statement with several slots is rejected with the error
  of the open evaluation: a slot cannot be checked with the others open.
- **Typed places.** Every evaluation gives the same list of typed places in the
  same order: result columns, named parameters, then unnamed parameters and the
  values of `IN @list`. The last two cannot be indices of the slot type, so they
  are rejected when their type depends on the choice.
- **Rejections** are kept in declaration order of the relations, whichever step
  rejected them.
- **Indices are simpler than four witnesses.** A dependent column or parameter is
  indexed by its OCaml value type. A column constrained across two slots is
  indexed by its SQL kind as a polymorphic variant (`` [ `Int ] ``), shared by
  both slots; nullability is not part of it. Equal kinds are always compatible,
  so the check is sound and conservative (INT does not meet DECIMAL).
  `Sqlgg_relation` is therefore not needed.
- **Placeholders.** `Sqlgg_template` renders the final SQL at runtime. It numbers
  placeholders only for PostgreSQL. Other placeholder styles do not depend on
  position and stay static text.
- **IR.** A slot is the `` `Relation `` fragment of `Sql_template`; a static source
  of the AST is wrapped in `` `Relation ``, a slot is `` `RelationSlot ``, so the JSON
  of every source changed shape. `Sql_template.fill ~spell` takes a fragment from
  `spell`: for PostgreSQL with a slot the bind is left in place and numbered at
  runtime. `Query.t`, `Ir.Analyzed` and `Parser.parse_result` carry
  `relation_slots`, omitted from JSON when empty.
- **Relation names** are always quoted for the selected dialect, quotes inside
  them doubled, so a table named after a reserved word works.
- **Messages.** Structural misuse (missing alias, `*`, `NATURAL`/`USING`, recursive
  CTE, non-SELECT, unqualified column, name clash) is reported directly. Only a
  slot no relation fits lists per-candidate reasons, up to five.
- **LSP.** A slot alias completes the columns common to every accepted relation;
  their displayed types are those of the first one.

### Not implemented yet

- relation slots together with `dynamic_select` (rejected with a diagnostic);
  dynamic-select columns are module-level values with fixed types;
- reusable candidates with choices, lists, tuples, enums or custom codecs as their
  own parameters (not offered, with a reason);
- enum or custom-codec types for columns or parameters that depend on the choice
  (generation error);
- an expression combining columns of two slots (`l.a + r.b`) is accepted only when
  both slots agree on its type, which can reject valid combinations;
- `IN @list` and tuple lists whose value type depends on the choice (rejected with
  a diagnostic): the generator has no per-relation literal conversion for them;
- relation slots inside an inline-SQL PPX endpoint;
- relation slots of a reusable query used as a CTE (`WITH x AS &query`), rejected
  with a diagnostic: the slots of a statement are the ones in its own text;
- table names with upper-case letters on a case-sensitive database: sqlgg stores
  identifiers in lower case and the relation is emitted under that name.

### Verification

- `src/test.ml`: parser, candidate order and collisions, analysis shape, a slot
  alias hidden by a table alias of a subquery, columns used only in a subquery,
  a literal comparison not shared between slots, `IN @list` values, a second slot
  the open evaluation does not reach, rejections in declaration order, a slot in
  every place a subquery can be, two slots compared inside a tuple list, a slot
  of a reusable CTE.
- `test/cram/test_build_relation_slots/`: generated interfaces, execution against
  the mock driver, bind order, PostgreSQL numbering, compile failures for forged
  values and incompatible pairs, every diagnostic.
- `test/property_based`: an oracle substitutes every combination of tables into
  the SQL text on random schemas. It checks that accepted combinations compile with
  the predicted types (soundness, all queries) and that valid ones are accepted
  (completeness, the exact subset). Re-introducing either known bug makes it fail.
- `lsp/test/relation_slot.t`: diagnostics, hover, completion.

This feature is separate from the typed SQL fragment design in
`docs/plans/2026-11-11-typed-sql-fragments-design.md`. It adds a closed,
automatically discovered choice of relations in `FROM` and `JOIN`. Typed
fragments remain the open-world mechanism for passing an arbitrary compatible
checked fragment.

## Purpose

A query may leave a relation source open:

```sql
-- @count_from
SELECT COUNT(1)
FROM @source AS source;
```

sqlgg discovers every loaded table and reusable query that can occupy the
slot, then generates an abstract source type with one safe builder per
candidate:

```ocaml
module Count_from : sig
  module Source : sig
    type t

    val table_users : t
    val table_orders : t
    val query_active_users : status:status -> t
  end

  val select :
    T.connection ->
    source:Source.t ->
    int64 T.io_future
end
```

The caller chooses a generated value, never a table-name string. The selected
value determines checked SQL IR and type evidence, and it captures any
candidate-specific parameter values.

## Goals

- Infer a closed set of table and named-`SELECT` candidates for each relation
  slot.
- Keep the generated query signature compact through a named abstract
  `Name.t`.
- Preserve source-dependent parameter and result types with GADT indices.
- Support slots inside nested subqueries.
- Support several slots without enumerating their Cartesian product.
- Keep reusable-query parameters owned by the selected source value.
- Render only checked SQL IR; never splice caller-provided SQL text.
- Reuse sqlgg's existing SQL analysis, codecs, nullability, and bind ordering.

## Non-goals for the first release

- Non-OCaml generators.
- Dynamic DML targets.
- An open-world arbitrary-fragment argument; that remains the purpose of
  `@@fragment`.
- Optional relation slots.
- Correlated reusable queries or `LATERAL` fragment candidates.
- `NATURAL JOIN` or `JOIN ... USING` involving a relation slot.
- `SELECT source.*` or an unqualified `SELECT *` that includes a relation
  slot.
- Relation slots inside a recursive CTE.
- Runtime parsing of raw strings into source values.
- Automatic generation of an additional physical `.ml` file.

## Language surface

The existing `PARAM` token is interpreted by grammar position:

```sql
FROM @source AS source
JOIN @events AS events ON events.user_id = source.id
```

In an expression position, `@name` remains an ordinary scalar parameter. In a
`FROM` or `JOIN` relation position, it becomes a relation-slot AST node.

The four composition forms have distinct meanings:

- `FROM @source AS source` selects at runtime from an automatically discovered
  closed candidate set.
- `FROM @@source AS source` accepts an open-world checked runtime fragment as
  described by the typed-fragment design.
- `FROM %%query%% AS source` expands one named template at compile time.
- `WITH source AS &query` retains the existing reusable-CTE behavior.

An alias is mandatory for every relation-slot occurrence. The `AS` keyword may
remain optional when the dialect grammar already permits an alias without it.
References to columns supplied by a relation slot must be qualified with that
alias.

The slot name and scalar-parameter names share the generated function-argument
namespace. Using the same name for both roles in one statement is rejected.

## Repeated slot names

Repeated occurrences of the same slot name denote one source argument:

```sql
SELECT left_side.id
FROM @source AS left_side
JOIN @source AS right_side ON left_side.parent_id = right_side.id;
```

All occurrences contribute constraints to one candidate set. The selected
relation is rendered once per occurrence. Parameters captured by its builder
are also emitted once per occurrence in final SQL order.

Different slot names denote independently selectable sources:

```sql
FROM @left_source AS left_side
JOIN @right_source AS right_side ON left_side.id = right_side.id
```

Shared SQL constraints produce shared GADT indices in the generated function
signature. This lets OCaml reject incompatible source pairs without generating
one constructor for every pair.

## Candidate environment

Candidates are discovered from the ordered compile environment already used by
sqlgg:

1. Physical tables registered by DDL processed before the statement.
2. Named `SELECT` statements declared with `include: reuse` or
   `include: reuse_and_execute` before the statement.
3. Definitions loaded through `-open`, in their existing processing order.

A reusable-query candidate must:

- have a fixed output shape;
- contain no unresolved relation slot;
- contain no runtime `subst`;
- contain no shape-changing `dynamic_select`;
- be closed with respect to outer query aliases;
- expose unique, non-empty output names for every column required by the slot.

An unnamed or duplicate output column is allowed only when no slot constraint
needs to address it by name.

Choices, optional predicates, list parameters, and tuple-list parameters remain
valid inside a reusable candidate when they preserve one output shape.

## Candidate names

Generated builders distinguish physical and query relations:

```ocaml
Source.table_users
Source.table_public_users
Source.query_active_users ~status
```

Names use sqlgg's normal OCaml identifier normalization. Two candidates that
still normalize to the same builder name cause a generation error. The
generator does not silently add numeric suffixes.

## Symbolic relation analysis

The analyzer evaluates a statement once with a symbolic schema for each unique
slot name.

Each qualified reference to a slot column creates or refines a symbolic column.
Normal expression analysis contributes constraints for:

- SQL scalar kind;
- nullability;
- custom codec and host representation;
- equality or common-type relationships to other columns and parameters;
- whether the value escapes through the generated parameter API;
- whether the value escapes through the result row API.

Nested queries use the normal scope stack. A slot inside a nested query is
analyzed in that query's scope. The surrounding nested query may reference an
outer alias:

```sql
SELECT users.id
FROM users
WHERE EXISTS (
  SELECT 1
  FROM @events AS events
  WHERE events.user_id = users.id
);
```

The reusable query substituted for `events` remains closed. Correlation belongs
to the surrounding checked SQL, not to the candidate.

After collecting constraints, the analyzer matches each candidate relation
against each slot independently. It does not evaluate every Cartesian product
of candidates. Constraints shared between slots become shared generated type
indices.

A small exhaustive substitution implementation is retained in tests as an
oracle. On generated small schemas, it must produce the same candidate sets and
type relationships as symbolic analysis.

## Source-dependent public types

If a source choice cannot affect any generated parameter or result type, the
source module exposes a non-parameterized abstract type:

```ocaml
module Source : sig
  type t

  val table_users : t
  val table_orders : t
end
```

This is the case for:

```sql
SELECT COUNT(1)
FROM @source AS source;
```

If source evidence escapes into the public API, the source type receives only
the necessary indices. For example:

```sql
SELECT source.id
FROM @source AS source;
```

conceptually generates:

```ocaml
module Source : sig
  type (_, _, _) t

  val table_users :
    (Sqlgg_type.int, Sqlgg_fragment.required, int64) t

  val table_drafts :
    (Sqlgg_type.int, Sqlgg_fragment.nullable, int64 option) t

  val table_external :
    (Sqlgg_type.text, Sqlgg_fragment.required, string) t
end

val select :
  T.connection ->
  source:('sql_kind, 'nullability, 'id) Source.t ->
  (id:'id -> 'result) ->
  'result list T.io_future
```

The concrete generated API uses the project's canonical SQL-kind and
nullability witnesses. The example names are schematic until the shared typed
fragment runtime types are implemented.

Source-dependent query parameters use the same evidence:

```sql
SELECT COUNT(1)
FROM @source AS source
WHERE source.id = @id;
```

conceptually generates:

```ocaml
val select :
  T.connection ->
  source:('sql_kind, 'nullability, 'id) Source.t ->
  id:'id ->
  int64 T.io_future
```

SQL kind is not erased merely because two SQL kinds decode to the same OCaml
type. Nullability is not erased merely because an operation accepts both
nullable and required inputs. This preserves cross-slot and parameter
constraints that host representation alone cannot prove.

## Hidden GADT and public construction

Every generated query module is sealed with an explicit signature. Its source
modules remain fully visible to the query implementation but abstract to
callers. Callers see each indexed source type and its generated builder values,
but not its representation or interpreter.

Its implementation uses a closed GADT when constructors refine public
evidence:

```ocaml
type (_, _, _) t =
  | Table_users :
      (Sqlgg_type.int, Sqlgg_fragment.required, int64) t
  | Table_drafts :
      (Sqlgg_type.int, Sqlgg_fragment.nullable, int64 option) t
  | Table_external :
      (Sqlgg_type.text, Sqlgg_fragment.required, string) t
```

When no evidence escapes, an ordinary closed variant is sufficient behind the
abstract `type t`.

Reusable queries with parameters store those values in the hidden constructor.
Their public builder is a smart constructor:

```ocaml
val query_active_users :
  status:status ->
  (Sqlgg_type.int, Sqlgg_fragment.required, int64) t
```

The source type has no `of_string` or public arbitrary constructor. Applications
that receive an untrusted table selector must parse that input into their own
domain choice and map it explicitly to one of the generated builders.

## Private relation descriptor

A private interpreter pattern-matches on the source GADT with locally abstract
types and produces an indexed relation descriptor. The descriptor contains:

- compositional SQL template IR;
- captured bind operations;
- the checked relation schema required by the outer query;
- SQL-kind, nullability, codec, and OCaml representation witnesses;
- result-column decoder operations needed by the outer query.

The descriptor type and interpreter are implementation details. They are not
exported from the sealed source module.

This split keeps the finite candidate choice in a GADT while keeping rendering
and decoding data-driven. It follows the same indexed-descriptor pattern already
used by OCaml dynamic select in `Sqlgg_scope`.

## Generated module placement

A statement containing relation slots is emitted as a sealed named query module
in the same generated `.ml`:

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
  module Source = struct
    (* hidden representation and interpreter *)
  end

  let select db ~source =
    (* checked execution *)
end
```

For multiple unique slot names, the query module contains one source module per
slot, named by the normalized slot name.

`Direct`, `Single`, `Fold`, and `List` execution views share the same source
modules and source values. The representation is generated once, not copied
into each execution view.

No side-effecting extra-file generation is introduced. A future explicit
generator mode may emit shared registries if measured generated-code size
justifies it.

## Parameter ownership

Outer scalar parameters remain arguments of the generated query function.

Parameters belonging to a selected reusable query are arguments of that
candidate's source builder:

```ocaml
Count_from.select
  db
  ~source:(Count_from.Source.query_active_users ~status:`Active)
```

They do not become optional arguments on the outer function. This makes
invalid combinations unrepresentable: selecting a physical table cannot
accidentally receive parameters belonging only to a reusable query.

Named candidate parameters are unified inside that candidate according to
existing sqlgg rules. Reusing one source value in repeated slot occurrences
duplicates its SQL and bind operations but not its construction API.

## Rendering

The query IR gains a relation-splice node containing a closed source descriptor.
It never contains a caller-provided string.

Final rendering:

1. Interprets each source GADT into a descriptor.
2. Inserts table identifiers or parenthesized reusable-query IR at relation
   positions.
3. Applies the occurrence's SQL alias.
4. Flattens nested templates and source templates.
5. Traverses final SQL order to assign placeholders.
6. Emits bind operations in that same order.

This ordering is required for PostgreSQL numbered placeholders and for repeated
or nested relation slots.

## Unsupported constructs

The first release rejects:

- a relation slot without an alias;
- unqualified references intended to resolve to a relation slot;
- `source.*`;
- unqualified `*` when a relation slot contributes to it;
- `NATURAL JOIN` involving a slot;
- `JOIN ... USING` involving a slot;
- an optional relation slot;
- a slot in a recursive CTE;
- a reusable candidate containing an unresolved relation slot;
- a reusable candidate requiring correlation or `LATERAL`;
- a reusable candidate containing `subst` or shape-changing
  `dynamic_select`;
- a relation slot in a non-OCaml generation run.

`COUNT(*)` is not a projection wildcard and remains valid.

## Diagnostics

Diagnostics are attached to the relation-slot occurrence.

When no candidate matches, the message includes:

- the slot name;
- required column names;
- inferred SQL-kind and nullability constraints;
- up to five near candidates in declaration order;
- the first concrete mismatch for each shown candidate.

Examples of concrete mismatch reasons:

- missing column `user_id`;
- expected integer-compatible `id`, found text;
- required non-null `name`, found nullable;
- duplicate output name `id`;
- candidate contains runtime `subst`;
- candidate is not closed.

Ambiguous or unresolved public type evidence is diagnosed during sqlgg
analysis. It is not deferred to an opaque generated-OCaml compiler error.

Name normalization collisions identify both original SQL names and the
colliding generated builder name.

## Generator scope

The first release supports:

- direct OCaml generation;
- OCaml IO generation;
- composition with the existing `[@@deriving sqlgg]` projection PPX when the
  outer statement also uses `dynamic_select`.

The existing projection PPX does not parse SQL and therefore gains no separate
relation-slot syntax. It consumes the generated query module as it does today.
The source GADT index and the selected-column descriptor index remain separate
arguments of the generated `select`.

Anonymous inline-SQL PPX endpoints described by the typed-fragment design are
outside this release. An expression extension has no stable place to export
the named `Query.Source.t` module required by this design. Relation slots in a
future inline PPX therefore require a separately designed named structure-item
endpoint; they must not silently expose an existentially packed source type.

C++, C#, Java, XML, and SQL-output generation report a clear unsupported
relation-slot diagnostic. They do not silently render one candidate or accept a
raw string.

## Tests

### Parser and AST

- `@name` parses as a relation slot only in `FROM` and `JOIN`.
- Scalar `@name` behavior is unchanged.
- Missing aliases and scalar/source name collisions are rejected.
- Repeated names produce one slot identity.

### Candidate inference

- `COUNT(1)` admits every valid loaded table and closed reusable query.
- Required columns filter candidates by name.
- Expressions filter or index candidates by SQL kind and nullability.
- Custom codecs are preserved.
- Repeated slot occurrences intersect their requirements.
- Multiple slot names remain independent except for shared SQL constraints.
- Symbolic inference agrees with exhaustive substitution on generated small
  schemas.

### Generated type API

- Shape-independent queries expose `Source.t`.
- Source-dependent result columns expose linked GADT indices.
- Source-dependent scalar parameters expose linked GADT indices.
- Two slots with a shared type constraint accept compatible builder pairs.
- Incompatible pairs fail to compile.
- Hidden constructors cannot be forged outside the generated module.
- Reusable-query builders require exactly their own parameters.
- A relation-source GADT composes with a dynamic-select descriptor and the
  existing `[@@deriving sqlgg]` projection output.

### Rendering

- Physical table names are dialect-quoted.
- Reusable queries are parenthesized correctly.
- Aliases are applied per occurrence.
- Repeated source values duplicate SQL and binds in final order.
- Nested subqueries preserve placeholder order.
- PostgreSQL placeholders are numbered only after all splices are flattened.

### Rejections

- No candidates.
- `source.*`.
- `NATURAL JOIN` and `JOIN ... USING`.
- Recursive CTE placement.
- Correlated, substituted, dynamically shaped, or unresolved reusable
  candidates.
- Every non-OCaml generator.

## Compatibility

Existing SQL without a parameter token in relation position is unchanged.
Existing scalar parameters retain their syntax and behavior.

Existing `&query` reusable CTEs remain unchanged. The new candidate environment
may read the same reusable-query registry, but relation slots do not rewrite
legacy CTE syntax.

The planned `%%template%%` and `@@fragment` forms retain their separate
semantics. Relation-slot source descriptors may share the same internal
compositional IR and type-witness runtime, but no public form is defined as an
alias of another.

## Implementation boundaries

The feature is split into independently testable responsibilities:

- SQL parser: recognizes relation-position parameters.
- SQL AST: represents relation slots distinctly from scalar parameters and
  concrete sources.
- Syntax analysis: creates symbolic relation schemas and solves constraints.
- Candidate matcher: matches loaded relations against one solved slot.
- OCaml API generator: emits sealed source modules, GADTs, and smart builders.
- Runtime template IR: renders relation splices and ordered binds.
- Diagnostics: explains candidate exclusion and unsupported constructs.

No relation-slot component accepts a caller-provided raw relation string after
the parser boundary. Every later phase operates on a parsed slot identity,
checked candidate, symbolic type evidence, closed source value, or
compositional IR node.
