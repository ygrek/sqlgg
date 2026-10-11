# sqlgg type system: one core, many dialects

Design note for the typing core in `lib/sql.ml` / `lib/syntax.ml`. The goal is a single
type language, a single signature form and a single inference procedure shared by every
dialect (MySQL, PostgreSQL, SQLite, TiDB, ClickHouse). A dialect contributes *data*
(which type constructors and functions exist, how literals are typed, which implicit
conversions are allowed, what nullability a column has by default) and never a code
path of its own inside the inference.

## Sources

- **TRAF** — Ye, Toro, Gutierrez, Oliveira, Tanter. *Elucidating Type Conversions in SQL
  Engines*, ESOP 2025; journal version *A Formal Framework for Typing and Cast Semantics
  in SQL Engines*, TOPLAS 2026 (adds NULL, EXISTS, IN, Oracle). A typing semantics for a
  relational core where everything engine-specific is isolated in abstract operators:
  `ty` (type of a literal), `resolve` (overload resolution with implicit casts),
  `biconv` (common type for set operations), `cast` (explicit cast feasibility).
  Engines are instantiations of these operators; the rest of the system is shared.
- **NRCλ** — Cooper, *The Script-Writer's Dream* (DBPL 2009, PhD); Ricciotti, Cheney,
  *Strongly Normalizing Higher-Order Relational Queries* (LMCS 2021). Nested relational
  calculus with non-recursive higher-order functions: lambdas inside queries are typed
  as plain simply-typed terms over collection types.
- **Nullable signatures** — Guagliardo, Libkin, *On the Codd semantics of SQL nulls*
  (Information Systems 2019). `n-sig(E)` is the set of nullable attributes of an
  expression, propagated structurally through the algebra, with
  `n-sig(σθ(E)) = n-sig(E) − c-sig(θ)`: a selection whose condition can only be true on
  non-null values removes those attributes from the nullable set. This is the formal
  counterpart of `lib/narrowing.ml`.
- **Substrait function extensions** (industrial, not a paper) — signature language with
  correlated type variables `any1..any9`, parametric types `list<T>`, `map<K,V>`,
  `func<(T1,..)->R>` and an explicit per-signature nullability policy
  `MIRROR | DECLARED_OUTPUT | DISCRETE`. Presto and ZetaSQL resolve higher-order
  functions the same way: bind type variables from the non-lambda arguments first, then
  type the lambda bodies.

## Type language

```
τ ::= α                                 type variable (only inside signatures)
    | C⟨τ₁, …, τₙ⟩                       constructor application

C ∈ { Int, UInt64, Float, Decimal(p,s), Text, Blob, Bool, Datetime, Json,
      Enum{…}, StringLiteral s, FloatingLiteral f, Json_path, One_or_all, Any,   -- arity 0
      Array, Map, Tuple, Fun }                                                   -- arity > 0
```

Nullability is a separate axis carried by every node: `Strict | Nullable | Depends`.
`Nullable(T)` in ClickHouse, `T NULL` in MySQL and a column from the outer side of a
`LEFT JOIN` all produce the same node; no dialect has its own nullability encoding.

Physical details that do not affect typing (`VARCHAR(n)`, `UNSIGNED` display width,
`LowCardinality`, `FixedString(n)`, `DateTime64(3,'UTC')`) stay in `Source_type` and are
erased when a column enters the inference, as today.

In OCaml this is two mutually recursive types: the ground `Type.t` (no variables,
consumed by generators) and the template `Type.tyvar` (variables allowed, used only in
signatures). Both carry the same constructors; a template is matched against ground
types, never the other way round.

## Signatures

One form for every function of every dialect:

```
σ ::= ∀ ᾱ. (τ₁, …, τₙ) → ρ        with nullability policy  Mirror | Declared | Discrete
```

- `Mirror` — result nullable iff some argument is nullable (today's `Depends` + `undepend`).
- `Declared` — result nullability is whatever ρ says (today's explicit `nullable Json`).
- `Discrete` — result strict iff some argument is strict (`COALESCE`, `IFNULL`).

Variadic functions are `(τ_fixed…, τ_rep…*) → ρ`. Aggregates are ordinary signatures
plus the grouping flag. Higher-order functions are ordinary signatures using `Fun`:

```
arrayMap    : ∀ α β. (Fun⟨α; β⟩, Array⟨α⟩)        → Array⟨β⟩       Mirror
arrayFilter : ∀ α.   (Fun⟨α; Bool⟩, Array⟨α⟩)     → Array⟨α⟩       Mirror
arrayReduce : ∀ α β. (StringLiteral, Array⟨α⟩)   → β              Declared (resolved by name)
```

Aggregate combinators (`-If`, `-Array`, `-OrNull`, `-State`, `-Merge`) are functions
from signature to signature and are derived at lookup time, not enumerated.

The five current forms (`F | Multi | Agg | Ret | sponge` plus the operator-specific
constructors) collapse into this one form; the operator constructors
(`Comparison`, `Logical`, `Membership`, …) remain as *names* so generators and
narrowing can still recognise them, but their typing goes through the same matcher.

## Inference

Bottom-up over the expression tree, as today. At a function application:

1. type every argument that is not a lambda;
2. match each template τᵢ against the ground argument type, binding variables; binding
   an already-bound variable goes through `resolve_d` (below) rather than equality, which
   is how `max(1, 2.5)` picks `Float`;
3. type lambda arguments with their parameters bound to the (now known) `Fun` domain;
4. instantiate ρ and apply the nullability policy;
5. push instantiated argument types back into parameters (`assign_params`).

Query parameters `@p` start as fresh ground `Any` and are refined by the same matcher;
the end-of-statement `unify_params` stays.

Nullability of query results follows `n-sig`: columns from base tables take the
dialect's default (`default_nullability_d`), joins widen, `WHERE`/`HAVING` narrow
through `narrowing.ml`, aggregates and scalar subqueries follow their signature plus
the "may produce no row" rule already implemented in `consider_agg_nullability`.

## Executable laws

`test/property_based/test_property_based.ml` checks the algebra independently of any
dialect:

- `common_type` is commutative and idempotent;
- it is associative on engine types whenever both parenthesizations are defined;
- `subtype` and `supertype` are defined together;
- `Array` and `Map` lift the relation structurally, while `Tuple` and `Fun` reject
  unequal arities;
- template substitution binds variables below constructors and rejects constructor
  mismatches with a typed error;
- outer nullability is joined independently from constructor arguments.

`StringLiteral` and `FloatingLiteral` are deliberately excluded from associativity.
They are evidence about a literal, not engine types: two equal floating literals retain
their evidence, while two different ones widen to `Float`. `ty_d` must erase that
evidence before laws of the engine type lattice are required.

## Dialect operators (TRAF)

| operator | meaning | today |
|---|---|---|
| `ty_d(v)` | type of a literal | scattered in `syntax.ml`, parser |
| `resolve_d(τ₁, τ₂)` | best common type under implicit casts | `Type.order_kind` / `common_type` |
| `biconv_d(τ₁, τ₂)` | common type for `UNION` / `CASE` / `IN` | `Type.common_supertype` |
| `default_nullability_d` | column nullability when the DDL says nothing | hard-coded `Nullable` in `make_attribute` |

Everything in `Dialect.Semantic` becomes a value of one of these; booleans there are
replaced by the operator results. The structural-vs-nominal enum question (MySQL vs
PostgreSQL, TODO in `sql.ml`) is a difference in `resolve_d`.

Syntax-level features (`PREWHERE`, `FINAL`, `ON CONFLICT`, `STRAIGHT_JOIN`, TTL, …) stay
in the `Dialect.feature` matrix; they are not part of typing.

## Migration order

Each step keeps `dune runtest` and `./run_test` green.

1. Recursive `Type.t` / `Type.tyvar` with `Array`, `Map`, `Tuple`, `Fun`; structural
   matcher in `Type.Template`; generators reject the new constructors in column/parameter
   position with a clear error until codecs exist. Lambda in the AST and parser.
2. ClickHouse as a dialect instantiation: type names and `Nullable`/`LowCardinality`
   in a dialect-scoped lexer table, strict-by-default columns, `{name:Type}` placeholders,
   array functions as signatures over the new constructors.
3. Signature unification: replace the ad-hoc `func` constructors by the single form with
   explicit nullability policy; combinators as signature transformers.
4. Lift `order_kind` / `common_type` into `resolve_d` / `biconv_d` parameterised by
   `Dialect.t`; move `Dialect.Semantic` flags into operator values.
5. Codecs for `Array`/`Map`/`Tuple` in `Sqlgg_traits` and `gen_caml`.
