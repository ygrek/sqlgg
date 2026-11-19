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
  | `Invalid_batch of int64
  | `Invalid_state of string
]

let parse_row ~id:raw_id ~batch:raw_batch ~state:raw_state =
  let ( let* ) result continue =
    match result with
    | Ok value -> continue value
    | Error error -> Error error
  in
  let* migration_id =
    Result.map_error
      (fun error -> `Invalid_id error)
      (Migration_id.parse raw_id)
  in
  let* batch =
    match Int64.unsigned_to_int raw_batch with
    | Some batch when batch > 0 -> Ok batch
    | Some _ | None -> Error (`Invalid_batch raw_batch)
  in
  let* state =
    match of_string raw_state with
    | Some state -> Ok state
    | None -> Error (`Invalid_state raw_state)
  in
  Ok { migration_id; batch; state }

let error_to_string = function
  | `Invalid_id error -> Migration_id.error_to_string error
  | `Invalid_batch batch ->
      "invalid migration batch: " ^ Int64.to_string batch
  | `Invalid_state raw ->
      "invalid migration state: " ^ raw
