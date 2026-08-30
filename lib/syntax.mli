open Sql

module Config: sig
  val debug : bool ref
  val allow_write_notnull_null : bool ref
  val dynamic_select : bool ref
end

type select_scope = {
  pos : pos;
  src_tbls : table list;
  cte_tables : table list;
  table_aliases : table_alias list;
}

type stmt_annotations = {
  src_tbls : table list;
  cte_defs : (table located * pos list) list;
  table_aliases : table_alias list;
  table_defs : (table_name located * string located list) list;
  expr_types : Type.t located list;
  select_scopes : select_scope list;
}

type result = {
  sql : string;
  schema : schema_column list;
  vars : var list;
  kind : Stmt.kind;
  dialect_features : Dialect.dialect_support list;
  annotations : stmt_annotations;
}

val scope_of : ?cte:cte -> nested option -> stmt_annotations

val parse : string -> result
val eval_parsed : string -> Parser.parse_result -> result
