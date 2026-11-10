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

val context_of_schema_sql :
  ?context:Compile.state ->
  ?dialect:Dialect.t ->
  string -> (Compile.state, Statements.t * diagnostic * diagnostic list) result

val with_context : ?context:Compile.state -> (unit -> 'a) -> 'a

val parse :
  ?allow_extensions:bool ->
  Statements.t ->
  (Parser.parse_result, diagnostic * diagnostic list) result
val analyze :
  ?allow_extensions:bool ->
  context:Compile.state ->
  Statements.t ->
  ((Parser.parse_result * Syntax.result), diagnostic * diagnostic list) result
