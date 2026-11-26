type location =
  | Span of Sql.Pos.t
  | Properties
  | Statement
[@@deriving json, jsonschema]

type diagnostic = {
  message : string;
  location : location;
}
[@@deriving json, jsonschema]

val diagnostic_of_exn : exn -> diagnostic

val dialect_diagnostics :
  dialect:Dialect.t -> Dialect.dialect_support list -> diagnostic list

(** The configuration schema statements are parsed under, and whether the dialect
    features they use must be supported by its dialect. *)
type schema_dialect =
  | Checked of Dialect.configuration
  | Unchecked of Dialect.configuration

val context_of_schema_sql :
  ?context:Compile.state ->
  dialect:schema_dialect ->
  string -> (Compile.state, Statements.t * diagnostic * diagnostic list) result

val with_context : ?context:Compile.state -> (unit -> 'a) -> 'a

val parse :
  ?allow_extensions:bool ->
  semantics:Dialect.Semantic.t ->
  Statements.t ->
  (Parser.parse_result, diagnostic * diagnostic list) result
val analyze :
  ?allow_extensions:bool ->
  semantics:Dialect.Semantic.t ->
  context:Compile.state ->
  Statements.t ->
  ((Parser.parse_result * Syntax.result), diagnostic * diagnostic list) result
