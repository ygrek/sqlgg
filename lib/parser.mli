type parse_result = private {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
  semantics : Dialect.Semantic.t;  (** the dialect the statement was parsed under *)
}

val parse_stmt : Dialect.Semantic.t -> string -> parse_result
