type t =
  | Applying
  | Applied
  | Reverting
  | Apply_failed
  | Revert_failed

type row = private {
  migration_id : Migration_id.t;
  batch : int;
  state : t;
}

type error = [
  | `Invalid_id of Migration_id.error
  | `Invalid_batch of string
  | `Invalid_state of string
]

val parse_row :
  id:string ->
  batch:string ->
  state:string ->
  (row, error) result

val to_string : t -> string

val error_to_string : error -> string
