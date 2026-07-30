module Name : sig
  type t
  type word
  type action

  val words : string -> word list
  val action : word list -> word list -> action
  val make : word list -> action list -> t
  val render : int option -> t -> string
  val fit : int option -> string -> string
end

type t

val make : ?name:string -> int -> t
val name : t -> string option
val parse : string -> t option
val to_string : t -> string
val compare : t -> t -> int
val next_ord : t -> int
val naming : max_length:int option -> int -> int option
