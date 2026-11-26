(** ClickHouse functions that are typed differently from, or unknown to, the standard catalog. *)

type error =
  | Unknown_name  (** not a ClickHouse-specific function; the standard catalog applies *)
  | Invalid_arity

val names : string list

(** [resolve name argument_count] takes a lowercased function name. *)
val resolve : string -> int -> (Sql.Source_type.t Sql.func, error) result
