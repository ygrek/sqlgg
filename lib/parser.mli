type parse_result = private {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}
[@@deriving json, jsonschema]

val parse_stmt : string -> parse_result
