
val debug : bool ref

val parse : string -> string * Sql.Schema.t * Sql.var list * Stmt.kind
