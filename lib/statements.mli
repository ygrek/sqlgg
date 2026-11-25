type metadata = (string * string) list

type src = { file : string option; line : int }

type t = {
  text : string;
  props : Props.t list;
  pos : Sql.Pos.t;
  src : src;
  metadata : (int * metadata) list;
  comments : Sql.Pos.t list;
  errors : (Sql.Pos.t * string) list;
}

type lexeme = [
  | `Text
  | `Literal
  | `Open_literal
  | `Blank
  | `Semicolon
  | `Comment
  | `Line_directive of int * string
  | `Props of (string * string) list
  | `Bad_props
]

val lexemes : string -> (Sql.Pos.t * lexeme) Seq.t
val split : string -> t list
val strip_comments : t -> t
val unstripped_offset : t -> int -> int
val pair_downs : t list -> (t * t option) list
