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

(** features unsupported, or of unknown support, in the selected dialect *)
val dialect_diagnostics :
  dialect:Dialect.t -> Dialect.dialect_support list -> diagnostic list

(** replay the DDL of [sql] on top of [context] (empty schema by default);
    when [dialect] is supplied, reject unsupported features *)
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
