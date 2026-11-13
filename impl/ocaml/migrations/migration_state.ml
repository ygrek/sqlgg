type t =
  | Applying [@as "applying"]
  | Applied [@as "applied"]
  | Reverting [@as "reverting"]
  | Apply_failed [@as "apply_failed"]
  | Revert_failed [@as "revert_failed"]
[@@deriving to_string, of_string]

type row = {
  migration_id : Migration_id.t;
  batch : int;
  state : t;
}

type error = [
  | `Invalid_id of Migration_id.error
  | `Invalid_batch of string
  | `Invalid_state of string
]

let parse_row ~id ~batch ~state =
  match Migration_id.parse id with
  | Error error -> Error (`Invalid_id error)
  | Ok migration_id ->
      (match int_of_string_opt batch with
       | Some batch when batch > 0 ->
           (match of_string state with
            | Some state -> Ok { migration_id; batch; state }
            | None -> Error (`Invalid_state state))
       | Some _ | None -> Error (`Invalid_batch batch))

let error_to_string = function
  | `Invalid_id error -> Migration_id.error_to_string error
  | `Invalid_batch raw ->
      "invalid migration batch: " ^ raw
  | `Invalid_state raw ->
      "invalid migration state: " ^ raw
