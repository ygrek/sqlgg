type t = Sqlgg.Sql.pos

val contains : t -> int -> bool
val covers : t -> int -> bool
val is_empty : t -> bool
val shift : int -> t -> t
val valid_offset : string -> int -> int
val find_innermost : int -> ('a * t) Seq.t -> ('a * t) option
val find_innermost_covering : int -> ('a * t) Seq.t -> ('a * t) option
