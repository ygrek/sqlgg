type t

val create : text:string -> Document.t -> int -> t
val hover : t -> (string * Pos.t) option
val definition : t -> Symbol.loc list

type token = { pos : Pos.t; typ : Params.token_type }

val semantic_tokens : lines:Sqlgg.Line_index.t -> Document.t -> token list
