open Sqlgg

type error = private { pos : Sql.Pos.t; msg : string }

type checked = private {
  kind : Stmt.kind;
  schema : Sql.schema;
  params : Params.node list;
  dialect_errors : error list;
  new_table : Symbol.t option;
}

type analysis

type outcome = private
  | Verbatim
  | Rejected of error
  | Checked of checked

type stmt = private {
  pos : Sql.Pos.t;
  name : string option;
  props_errors : error list;
  tokens : Recover_parser.lexeme list Lazy.t;
  analysis : analysis;
  outcome : outcome;
}

val errors : stmt -> error list
val params : stmt -> Params.node list
val statement_scope : stmt -> Symbol.t list
val sources : stmt -> int -> Symbol.t list
val exprs : stmt -> (Sql.Type.t * Sql.Pos.t) list
val result_aliases : stmt -> (Sql.attr * Sql.Pos.t) list
val select_scope_opt : stmt -> int -> Symbol.t list option
val scope : stmt -> int -> Symbol.t list

type checked_statement = private {
  block : Statements.t;
  stmt : stmt;
}

type t = private {
  path : string;
  text : string;
  statements : checked_statement array;
  index : Symbol.t Symbol.Index.t;
  snapshot : Compile.state;
}

val find_statement : t -> int -> checked_statement option
val find_reusable_opt : t -> string -> (stmt * Symbol.loc) option

module Cache : sig
  type t
  val create : unit -> t
  val forget : t -> string -> unit
end

val analyze : ?cache:Cache.t -> path:string -> string -> t
val recheck : t -> Statements.t -> stmt
