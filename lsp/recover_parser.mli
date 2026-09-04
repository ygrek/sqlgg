open Sqlgg

type lexeme = { token : Sql_tokens.token; pos : Sql.pos }

val ident_name : Sql_tokens.token -> string option
val qualifier_before : lexeme list -> string option

type stop =
  | Complete of Sql.stmt
  | Fail
  | Pause of Sql.stmt Sql_parser_incremental.MenhirInterpreter.env * lexeme

type trace = {
  seen : lexeme list;
  recovery : bool;
  tables : string list;
  sources : Sql.source list;
}

type run = { trace : trace; stop : stop }

val run : string -> int -> run
val tokens : string -> lexeme list
val accepts : run -> Sql_tokens.token -> bool

type role =
  | Table_name
  | Column_name
  | Qualifier
  | Function_name

type slot =
  | Parameter of int
  | Column_of of string
  | Name of role list

val slot : ?next:Sql_tokens.token -> run -> slot
