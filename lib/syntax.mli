
val debug : bool ref

val parse : string -> string * Sql.Schema.t * Sql.var list * Stmt.kind * Dialect.dialect_support list

val eval_select: Sql.select_full -> Sql.Schema.t * Sql.vars * Stmt.kind
