open Sqlgg

type loc = private {
  file : string;
  pos : Pos.t;
}

type kind =
  | Table
  | Cte
  | Local

type column = private {
  attr : Sql.attr;
  loc : loc option;
}

type t = private {
  name : string;
  kind : kind;
  loc : loc option;
  columns : column list;
}

val loc : file:string -> Pos.t -> loc
val column : ?loc:loc -> Sql.attr -> column
val make : name:string -> kind:kind -> ?loc:loc -> column list -> t
val rename : string -> t -> t

val columns : t -> Sql.schema
val find_column : t -> string -> column option
val find : t list -> string -> t option
val unique : t list -> t list
