type t

val make : ?encoding:[ `UTF16 | `UTF8 ] -> string -> t
val of_file : ?encoding:[ `UTF16 | `UTF8 ] -> string -> t
val line : t -> int -> int
val position : t -> int -> int * int
val offset : t -> line:int -> character:int -> int
