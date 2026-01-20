open Sql

module Config: sig
  val debug : bool ref
  val allow_write_notnull_null : bool ref
  val dynamic_select : bool ref
end

type 'a schema_column =
  | Attr of 'a
  | Dynamic of param_id * (param_id * 'a) list
  [@@deriving show]

val parse : string -> string * attr schema_column list * var list * Stmt.kind * Dialect.dialect_support list
val eval_select: select_full -> attr schema_column list * var list * Stmt.kind
