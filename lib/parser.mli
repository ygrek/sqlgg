type parse_result = private {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}

val parse_stmt : string -> parse_result
