(** Relations that may occupy a relation slot: loaded tables and reusable SELECTs *)

type origin =
  | Table of Sql.table
  | Query of Shared_queries.entry
[@@deriving json, jsonschema]

(** [builder] is the OCaml value name, [constructor] the matching variant name *)
type t = private {
  origin : origin;
  builder : string;
  constructor : string;
}
[@@deriving json, jsonschema]

type error = Name_collision of { builder : string; first : string; second : string }

val show_error : error -> string

(** source name used in diagnostics *)
val source_name : t -> string

(** loaded tables in declaration order followed by reusable SELECTs in declaration order *)
val all : unit -> (t list, error) result
