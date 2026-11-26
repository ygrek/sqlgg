type t

type error = [
  | `Invalid of string
  | `Too_long of int
]

val max_length : int

val parse : string -> (t, error) result
val parse_generated : id:string -> t
val make : ordinal:int64 -> name:string option -> (t, error) result
val to_string : t -> string
val ordinal : t -> int64
val name : t -> string option
val compare_position : t -> t -> int
val next_position : t -> (int64, [ `Position_overflow ]) result
val error_to_string : error -> string
