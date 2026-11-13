type t

type error = [
  | `Invalid of string
  | `Too_long of int
]

val max_length : int

val parse : string -> (t, error) result
val to_string : t -> string
val compare : t -> t -> int
val error_to_string : error -> string
