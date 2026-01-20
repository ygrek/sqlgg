open Sql

module Config: sig
  val debug : bool ref
  val allow_write_notnull_null : bool ref
  val dynamic_select : bool ref
end

val parse : string -> string * Sql.schema_column list * var list * Stmt.kind * Dialect.dialect_support list
val eval_select: select_full -> Sql.schema_column list * var list * Stmt.kind
