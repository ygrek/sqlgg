# Typed SQL Fragments

Status: approved for implementation planning
Date: 2026-11-11
Scope: `.sql` inputs and inline OCaml PPX
Baseline: upstream `master` at `7fb6186f`

## Summary

sqlgg will support three typed subquery composition forms:

```sql
%%inner%%  -- compile-time template
@@inner    -- required runtime fragment
?@@inner   -- optional runtime fragment
```

All three forms pass through the normal sqlgg lexer, parser, type checker, and
generator. They never splice an unchecked SQL string.

The existing reusable CTE syntax remains unchanged and separate:

```sql
WITH inner AS &reusable_query
```

`&reusable_query` continues to be valid only as the body of a CTE. It is not
generalized to other subquery positions.

## Goals

- Reuse a checked `SELECT` in another query without copying its SQL.
- Compose fragments defined in `.sql` files and inline OCaml PPX.
- Pass a fragment between OCaml modules as an ordinary typed value.
- Infer the minimum relation shape required by a dynamic fragment slot.
- Preserve parameter types, nullability, cardinality, and dialect.
- Number placeholders only after the final query has been assembled.
- Make absence of an optional fragment explicit and position-dependent.
- Establish an extensible foundation for typed expression and clause fragments.

## Non-goals

- A second handwritten query-builder DSL.
- Raw SQL interpolation or a `<<...>>` escape hatch.
- Generalizing `&name` beyond reusable CTEs.
- Compile-time lookup of an OCaml value from another compilation unit.
- Correlated dynamic fragments in the first release.
- Optional `FROM` or `JOIN` sources in the first release.
- Dynamic `SELECT *` over a runtime fragment in the first release.
- `dynamic_select` inside a typed template or fragment.
- Runtime `subst` inside a typed template or fragment.

## Language surface

### Static templates

`%%name%%` resolves a named query at compile time:

```sql
-- @active_user_ids | include: reuse
SELECT id
FROM users
WHERE status = @status;

-- @active_orders
SELECT id, total
FROM orders
WHERE user_id IN %%active_user_ids%%;
```

The referenced query is inserted as a parsed and typed template, not as text.
Its scalar parameters become scalar parameters of the outer query:

```ocaml
active_orders db ~status:`Active
```

The existing include modes retain their execution meaning:

- `include: reuse` makes the statement available as a template and emits a
  fragment builder, but does not emit a database-executing function.
- `include: reuse_and_execute` emits the template, fragment builder, and
  database-executing function.
- `include: execute` emits only the database-executing function.

Templates from another `.sql` file are made available through the existing
`-open` mechanism. Resolution follows the source order already used for schema
and reusable-query state.

A reusable statement that contains `subst` retains its existing `&name`
behavior but is not a typed template or fragment source. Using it through
`%%name%%` produces a diagnostic. This keeps legacy reusable CTEs intact
without admitting runtime strings into typed composition.

### Required runtime fragments

`@@name` declares a labelled fragment argument:

```sql
-- @orders_for
SELECT o.id, o.total
FROM orders AS o
WHERE o.user_id IN @@users;
```

The argument is a closed, checked fragment rather than a string:

```ocaml
orders_for db
  ~users:(Sqlgg_fragments.active_user_ids ~status:`Active)
```

The fragment receives its own scalar parameters before it is passed to the
outer query. Those parameters do not leak into the outer function signature.

### Optional runtime fragments

`?@@name` declares a labelled `fragment option` argument:

```sql
SELECT id, total
FROM orders
WHERE user_id IN ?@@users;
```

`Some fragment` inserts the fragment. `None` applies the fallback defined for
the containing AST construct. It never inserts an empty string.

### Inline PPX

An explicit binding extension defines both a same-file template and a
first-class fragment builder:

```ocaml
let%sql.fragment active_user_ids = {|
  SELECT id
  FROM users
  WHERE status = @status
|}

let active_orders =
  [%sql {|
    SELECT id, total
    FROM orders
    WHERE user_id IN %%active_user_ids%%
  |}]
```

The template name is visible from its declaration to the end of the current
compilation unit. Declaration before use is required.

An anonymous fragment builder is also available:

```ocaml
let active_user_ids =
  [%sql.Fragment {|
    SELECT id
    FROM users
    WHERE status = @status
  |}]
```

`[%sql.Fragment]` does not register a compile-time template.

### Composition between OCaml modules

Compile-time template lookup does not cross `.ml` boundaries. Cross-module
composition uses a first-class fragment:

```ocaml
(* filters.ml *)
let%sql.fragment active_user_ids = {|
  SELECT id
  FROM users
  WHERE status = @status
|}

(* reports.ml *)
let active_orders =
  [%sql {|
    SELECT id, total
    FROM orders
    WHERE user_id IN @@users
  |}]

let run db =
  active_orders db
    ~users:(Filters.active_user_ids ~status:`Active)
```

The PPX does not inspect `filters.ml`, read implementation artifacts, or share
a process-global template registry. The OCaml type in `Filters.cmi` is the
cross-file contract.

## Supported subquery positions

The first release supports static and required dynamic fragments in:

```sql
FROM @@source AS source
JOIN @@source AS source ON ...
WHERE value IN @@values
WHERE (left, right) IN @@pairs
WHERE EXISTS @@rows
SELECT (@@value)
```

`%%name%%` is accepted in the corresponding positions.

Position-specific requirements are checked:

- `FROM` and `JOIN` require a relation-producing `SELECT`.
- scalar subqueries require exactly one column and cardinality no wider than
  zero-or-one;
- scalar `IN` requires exactly one compatible output column;
- tuple `IN` requires the same arity and compatible types;
- `EXISTS` accepts any output shape and cardinality.

An alias is mandatory for a dynamic `FROM` or `JOIN` source in the first
release. This keeps column ownership unambiguous while relation constraints are
inferred.

A fragment used as a runtime relation must expose unique, non-empty output
names. In the first release, each name must already be a valid OCaml method
label. Arbitrary quoted names and two SQL names that normalize to the same
OCaml name are rejected instead of being lossy-mangled.

`SELECT *` from a dynamic source is rejected. The outer query cannot determine
its result schema without knowing the concrete runtime fragment. Explicit
columns remain fully typed.

## Optional semantics

Optional behavior belongs to the containing AST node:

| Form | `None` result |
|---|---|
| `value IN ?@@rows` | the complete `IN` predicate becomes `TRUE` |
| `value NOT IN ?@@rows` | the complete `NOT IN` predicate becomes `TRUE` |
| `EXISTS ?@@rows` | the complete `EXISTS` predicate becomes `TRUE` |
| `NOT EXISTS ?@@rows` | the complete `NOT EXISTS` predicate becomes `TRUE` |
| `(?@@value)` | typed `NULL`; the scalar result becomes nullable |

An enclosing expression still applies normally. The fallback is attached only
to the immediate construct listed above.

An optional scalar is accepted only when surrounding SQL fixes one concrete SQL
type during analysis. The renderer emits the dialect-specific typed null for
that type. A standalone unconstrained `SELECT (?@@value)` is rejected because
`None` carries no fragment from which to recover a SQL type witness.

The following forms are rejected in the first release:

```sql
FROM ?@@source
JOIN ?@@source AS source ON ...
```

Removing a source changes the available column scope and can change the result
schema. Optional joins can be added later only with an analysis that proves no
reference escapes the removable branch.

## Static and dynamic parameter ownership

Static expansion lifts parameters:

```sql
SELECT *
FROM orders
WHERE user_id IN %%active_user_ids%%
  AND created_at > @after
```

produces a function with both `~status` and `~after`.

If static templates contribute parameters with the same name and compatible
types, they refer to one labelled argument, matching current reusable-query
behavior. A type or nullability conflict is a compile-time error.

Every unnamed parameter occurrence remains distinct. Repeating a template
therefore repeats its unnamed parameters in final SQL order. Named parameters
are unified only by equal name, SQL type, nullability, and codec.

Dynamic expansion preserves ownership:

```ocaml
orders_after db
  ~users:(active_user_ids ~status:`Active)
  ~after
```

The fragment is closed over `status`; the outer query owns only `users` and
`after`.

## Type contract

The public fragment representation is abstract. Conceptually it carries six
independent proofs:

```ocaml
('dialect, 'types, 'scope, 'columns, 'shape, 'cardinality) Sqlgg_fragment.t
```

- `dialect` prevents composition across incompatible SQL dialects;
- `types` is the driver type family used by `Sqlgg_request`;
- `scope` is `Sqlgg_fragment.closed` in the first release;
- `columns` is a structural row of named runtime column witnesses;
- `shape` is the positional output sequence used by scalar and tuple slots;
- `cardinality` records many, zero-or-one, or exactly-one.

The constructor of `Sqlgg_fragment.t` is not public. A caller can obtain a
fragment only from generated sqlgg code.

Each structural-row method returns an abstract typed column witness:

```ocaml
< id :
    ( 'types,
      Sqlgg_type.int,
      Sqlgg_fragment.required,
      int64 )
    Sqlgg_fragment.column
; name :
    ( 'types,
      Sqlgg_type.text,
      Sqlgg_fragment.nullable,
      string option )
    Sqlgg_fragment.column
; .. >
```

A column witness is indexed separately by driver type family, SQL scalar kind,
nullability, and decoded OCaml value type. It carries the output name,
parameter/result representation, and decoder. It is data used by the final
request interpreter, not only a phantom marker. Consequently, SQL `TEXT` and
SQL `ANY` cannot compose merely because both happen to decode to `string`.

`shape` is a nested abstract cons/end structure containing the same column
witness indices in SQL output order. Named `columns` provide width-subtyping
for `FROM` and `JOIN`; positional `shape` proves the arity and ordered element
types required by scalar and tuple `IN`.

Dynamic source analysis infers the smallest structural row required by the
outer query. If it reads `source.id` and `source.name`, the generated argument
accepts a fragment whose columns object has at least those methods. A fragment
with additional methods is accepted through object-row width subtyping.
Missing columns and incompatible scalar types or nullability fail at the OCaml
call site.

This runtime dictionary also supports a selected column whose type is not
otherwise fixed by the outer SQL. The outer result remains polymorphic in that
column's OCaml value type and obtains the concrete decoder from the supplied
column witness. Operations that constrain the column, such as comparison with
a statically known table column, unify that value type normally.

Cardinality uses a covariant closed polymorphic-variant row directly in the
fragment type. A relation slot accepts exactly-one, zero-or-one, or many; a
scalar slot accepts only exactly-one or zero-or-one. A many-row fragment
therefore cannot unify with a scalar slot.

The initial `closed` scope proof deliberately rejects correlated fragments.
Future support can introduce a scope containing the required outer columns
without changing the other fragment dimensions.

Fragment binds and columns use the driver-neutral scalar and representation
descriptors already used by `Sqlgg_request`. Custom codecs are captured as
typed conversion functions around those descriptors. No operation from a
concrete `Sqlgg_traits.M` runs while a fragment is built. The final executable
request interprets the complete descriptor sequence with one traits module, so
fragments of one dialect cannot carry mutually incompatible driver operations.
At that endpoint, the fragment's `types` index must unify with the concrete
`Sqlgg_request.Make(T).types`; a driver-neutral `Request` keeps the index
polymorphic until it is interpreted.

A representative public signature is:

```ocaml
val active_user_ids :
  status:status ->
  ( Sqlgg_dialect.postgresql,
    'types,
    Sqlgg_fragment.closed,
    < id :
        ( 'types,
          Sqlgg_type.int,
          Sqlgg_fragment.required,
          int64 )
        Sqlgg_fragment.column >,
    ( ( 'types,
        Sqlgg_type.int,
        Sqlgg_fragment.required,
        int64 )
      Sqlgg_fragment.column,
      Sqlgg_fragment.shape_end )
    Sqlgg_fragment.shape_item,
    [ `Many ] )
  Sqlgg_fragment.t
```

An outer query that projects unconstrained columns can retain their value types
in its own signature:

```ocaml
val report :
  T.connection ->
  source:
    ( Sqlgg_dialect.postgresql,
      'types,
      Sqlgg_fragment.closed,
      < id :
          ( 'types,
            'id_sql_kind,
            'id_nullability,
            'id )
          Sqlgg_fragment.column
      ; name :
          ( 'types,
            'name_sql_kind,
            'name_nullability,
            'name )
          Sqlgg_fragment.column
      ; .. >,
      'shape,
      [< `Exactly_one | `Zero_or_one | `Many ] )
    Sqlgg_fragment.t ->
  ('id * 'name) list T.io_future
```

Supplying `source` fixes `'id` and `'name` and supplies their runtime decoders.
The open object row admits additional fragment columns without weakening the
required `id` and `name` contract.

An outer scalar slot uses a bounded cardinality row rather than a second
fragment type. Compile-pass and compile-fail tests cover every permitted and
rejected cardinality direction.

## Typed states and transitions

The design distinguishes these states:

1. Raw SQL source.
2. Parsed statement containing explicit template and fragment references.
3. Resolved statement with static templates expanded.
4. Analyzed query with inferred dynamic-fragment contracts.
5. Fragment builder awaiting its own scalar or fragment arguments.
6. Closed fragment containing no unresolved arguments.
7. Executable request containing no unresolved arguments.
8. Rendered SQL and ordered bind operations.

Only the following transitions are allowed:

```text
raw source
  -> parsed statement
  -> resolved statement
  -> analyzed query
  -> generated builder
  -> closed fragment or executable request
  -> rendered SQL and binds
```

A runtime fragment slot accepts only state 6. Raw strings, partially applied
builders, executable DML requests, and fragments from another dialect cannot
enter that slot.

## Front-end representation

The SQL AST represents each case directly:

```ocaml
type subquery =
  | Inline of select_full
  | Static_template of Template_name.t
  | Dynamic_fragment of Fragment_name.t
```

Template and fragment names are parsed once into abstract name types. Invalid
names produce structured parse errors at the source boundary.

Optionality is represented only by constructors of the containing expression:
optional `IN`, optional `EXISTS`, and optional scalar. There is no general
optional-subquery constructor, so the parser cannot construct an optional
fragment in a position that has no defined fallback.

The lexer recognizes longer forms first:

1. `?@@identifier`
2. `@@identifier`
3. `%%identifier%%`
4. existing parameter and operator tokens

## Template environments

`.sql` processing uses the existing ordered source environment extended with
typed templates.

Inline PPX uses a compilation-unit-local environment carried by the structure
traversal:

- it starts empty for every `.ml`;
- `let%sql.fragment name` analyzes the fragment and extends the environment;
- later `%%name%%` references resolve from that environment;
- leaving the compilation unit discards the environment.

No new top-level mutable registry is introduced. Two files may declare the
same template name without interacting.

`let%sql.fragment` is accepted only as a top-level structure item in the first
release. It is rejected in local lets, nested modules, and functor bodies.
This gives every declaration one unambiguous compilation-unit scope and avoids
shadowing rules that the initial API does not need.

Declaration-before-use applies to both PPX and ordered `.sql` sources. A
template is fully resolved before it enters the environment, so a recursive
template cycle is not representable.

## Compositional query IR

Generated code must not store final SQL for a composable query. The current
`Sql_template` algebra remains the base: text, binds, list and tuple
substitutions, choices, optional clauses, dynamic selects, and conditional
pieces retain their distinct nodes. Fragment composition extends that algebra
with:

```text
Required_splice
Optional_predicate_splice
Optional_scalar_splice
```

`Bind` stores the typed encoding operation and value. A splice stores a closed
fragment, never a string.

Final rendering performs these operations in order:

1. resolve optional nodes;
2. recursively flatten required fragments;
3. traverse the final sequence in SQL order;
4. assign placeholder numbers;
5. emit SQL text;
6. emit bind operations in the same order.

This ordering is required for PostgreSQL placeholders. A fragment inserted
between two outer binds receives the corresponding middle placeholder numbers.

Using one fragment value in two slots duplicates its SQL and bind operations.
Nested fragments work recursively. The IR remains driver-neutral until the
existing request/traits layer renders or executes it.

Existing choices, optional clauses, and list/tuple expansion remain legal
inside a fragment when every branch has one invariant output shape.
`dynamic_select` changes the output shape and is rejected in typed templates
and fragments in the first release. `subst` is not an IR node carrying a typed
value; it performs runtime string replacement, so typed composition rejects it
as well.

`[%sql]`, `[%sql.Request]`, and `[%sql.Fragment]` share this analysis and IR.
They differ only in the generated endpoint:

- `[%sql]` produces an executable function;
- `[%sql.Request]` produces a driver-neutral request after all arguments are
  supplied;
- `[%sql.Fragment]` produces a closed fragment after all arguments are
  supplied.

## Relationship to the existing reusable CTE

The two static mechanisms have intentionally different language boundaries:

```sql
WITH rows AS &query
```

uses the existing reusable-CTE grammar.

```sql
WHERE id IN %%query%%
```

uses the new general subquery-template grammar.

They may share internal resolution and analysis machinery, but neither syntax
is rewritten into the other in user-visible SQL. Existing `&name` behavior,
diagnostics, and generated APIs remain compatible.

## Compatibility with `subst`

Existing runtime substitution remains valid inside quoted identifiers and
string-like SQL text:

```sql
-- [sqlgg] subst=tablename
SELECT * FROM `%%tablename%%`;
```

The lexer sees the quoted region as one identifier token, so
`%%tablename%%` is not a template token.

An unquoted standalone form is always a template:

```sql
WHERE id IN %%active_user_ids%%
```

If the same name is also listed in `subst`, sqlgg reports that standalone
`%%name%%` is a template and points the user to quoting for runtime
substitution. Name-based precedence is not used.

This rule avoids context-dependent token meaning and preserves existing valid
quoted-substitution use.

## Diagnostics

Errors preserve both use-site and definition-site provenance when available:

```text
sqlgg: template %%active_user_ids%% cannot be used in tuple IN
  used at reports.ml:18:27
  defined at reports.ml:3:1
  expected 2 columns, but the template produces 1
```

Dedicated diagnostics cover:

- unknown template;
- declaration after use in PPX;
- duplicate template declaration in one scope;
- fragment reference in an unsupported grammar position;
- optional fragment without a defined fallback;
- dynamic source without an alias;
- dynamic `SELECT *`;
- non-`SELECT` fragment definition;
- correlated fragment in the closed-fragment release;
- column arity, scalar type, nullability, or cardinality mismatch;
- dialect mismatch;
- `dynamic_select` exposed as a typed template or fragment;
- a statement with `subst` exposed as a typed template or fragment;
- standalone `%%name%%` confused with declared runtime substitution.

SQL diagnostics point into the string literal through the existing inline-SQL
location mapping. Type incompatibilities between modules are reported at the
OCaml fragment argument.

Using one slot name as both `@@name` and `?@@name`, or in positions requiring
incompatible row/cardinality contracts, is an analysis error. Required versus
optional is part of the slot contract rather than a boolean merged after
analysis.

## PPX configuration and interfaces

The design extends the inline SQL PPX present on the baseline master branch.
`let%sql.fragment`, `[%sql.Fragment]`, `[%sql]`, and `[%sql.Request]` use the
same `-schema`, `-dialect`, and `-params` configuration. Schema paths retain
their existing resolution relative to the preprocessed source file.

The direct executable endpoint receives its `Sqlgg_traits.M` module exactly as
existing `[%sql]` does. Fragment construction remains traits-independent; only
the final composed request is interpreted by that module.

Projects without an explicit `.mli` export an inferred fragment-builder type
through the generated `.cmi`. An explicit interface can write that inferred
type directly or use:

```ocaml
[%%sql.fragment.signature
  active_user_ids, {|
    SELECT id
    FROM users
    WHERE status = @status
  |}]
```

The signature extension runs the same analysis and emits one `val` item.
Normal `.ml`/`.mli` checking guarantees a type-compatible contract, not textual
or semantic identity of the duplicated SQL. Projects requiring one source of
truth should define the fragment in `.sql` and expose the generated module
instead of duplicating SQL in an interface.

## Generated APIs

For composable reusable `.sql` statements, OCaml generation places fragment
builders in a reserved canonical namespace:

```ocaml
module Sqlgg_fragments : sig
  val active_user_ids :
    status:status ->
    ( dialect,
      types,
      Sqlgg_fragment.closed,
      columns,
      shape,
      cardinality )
    Sqlgg_fragment.t
end
```

The concrete generated signature uses the inferred dialect, structural column
row, and cardinality directly rather than the schematic names above.

`include: reuse` emits this builder without an execution function.
`include: reuse_and_execute` emits both
`Sqlgg_fragments.active_user_ids` and the existing execution API. The reserved
module prevents collisions with existing flat statement functions. It contains
fragment-building logic and is not a module alias.

An outer query containing `@@users` gains a required labelled fragment
argument. An outer query containing `?@@users` gains a labelled fragment
`option` argument. Reusing one name in compatible positions produces one
argument. Incompatible requirements or mixed required/optional use of the same
name are an analysis error.

## Query-builder direction

The compositional IR is the query builder. It is intentionally not exposed as
constructors.

Future releases may add checked fragment categories such as:

- scalar expression;
- predicate;
- projection item;
- `ORDER BY` item;
- `GROUP BY` item;
- complete join branch.

Each category must have:

- its own AST constructor;
- a precise scope contract;
- a generated abstract runtime type;
- a normal sqlgg parse and analysis path;
- a position-specific composition rule.

No category accepts arbitrary SQL text. An escape hatch, if ever required,
must be designed as an explicitly unsafe API and is outside this proposal.

## Implementation sequence

### 1. Static subquery templates

- Add `%%name%%` lexer and parser support.
- Generalize typed template resolution to subquery positions without changing
  `&name`.
- Add `.sql` and same-file PPX environments.
- Add `let%sql.fragment` and `[%sql.Fragment]`.
- Verify parameter lifting and source provenance.

### 2. Required runtime fragments

- Add the abstract fragment runtime and compositional IR.
- Infer relation requirements for `@@name`.
- Support `IN`, tuple `IN`, `EXISTS`, and scalar subqueries.
- Support aliased `FROM` and `JOIN`.
- Render final placeholders and binds after composition.

### 3. Optional runtime fragments

- Add position-specific optional AST constructors.
- Implement predicate-to-`TRUE` and constrained scalar-to-typed-`NULL`
  fallbacks.
- Generate `fragment option` arguments.
- Reject optional source positions.

### 4. Generator coverage

- Integrate direct OCaml, IO, and `Request` generation.
- Emit fragment builders for `.sql` reusable statements.
- Either implement an equivalent typed abstraction for another target or
  produce a clear unsupported-generator diagnostic. No target silently falls
  back to textual interpolation.

### 5. Later extensions

- Correlated fragments with an explicit required-scope type.
- Optional joins with non-escaping alias proof.
- Additional typed fragment categories.

## Test strategy

### Lexer and parser

- Longest-token precedence for `?@@`, `@@`, and `%%...%%`.
- Quoted `%%subst%%` remains an identifier or literal.
- Every supported and rejected grammar position.
- Accurate spans for all three forms.

### Static analysis

- Same-file and `-open` template lookup.
- Declaration-before-use in PPX.
- Duplicate and unknown templates.
- Scalar and tuple arity.
- Driver type family, SQL kind, nullability, positional shape, cardinality, and
  dialect compatibility.
- Dynamic-source alias requirements.
- Rejection of correlation and dynamic `SELECT *`.
- Rejection of shape-changing `dynamic_select`.
- Unique, OCaml-safe output names for runtime relation fragments.
- Required/optional reuse of one slot name is rejected.

### PPX

- `let%sql.fragment` expansion.
- Anonymous `[%sql.Fragment]`.
- Same-file `%%template%%`.
- Cross-file `@@fragment` through `.cmi` types.
- Explicit `.mli` fragment signature.
- No template-state leakage between compilation units.
- Source locations inside quoted SQL.

### Rendering

- Nested and repeated fragments.
- Parameters before, inside, and after a fragment.
- Named, unnamed, Oracle, and PostgreSQL placeholders.
- Optional `Some` and `None` for every supported context.
- Bind count and bind order match rendered placeholders.
- Empty-list behavior inside nested fragments.

### Compatibility

- Existing reusable CTE tests remain unchanged.
- Existing quoted `subst` tests remain unchanged.
- Typed template/fragment definitions containing `subst` are rejected.
- Typed template/fragment definitions containing `dynamic_select` are
  rejected.
- Existing inline `[%sql]` and `[%sql.Request]` APIs remain unchanged when no
  fragment syntax is present.
- Unsupported generators fail explicitly.

## Acceptance criteria

The first release is complete when:

- the three new forms work in `.sql` and inline PPX;
- `&name` has not gained any new grammar position;
- static parameters lift into the outer API;
- dynamic parameters remain owned by their fragment;
- a fragment can cross an OCaml module boundary without PPX global state;
- incompatible rows, cardinality, scope, and dialect cannot compose;
- optional absence is represented by explicit AST semantics;
- final SQL and bind order are correct for all placeholder modes;
- no public API can construct a fragment from a raw string.
