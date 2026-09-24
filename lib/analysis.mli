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

val context_of_schema_sql :
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
