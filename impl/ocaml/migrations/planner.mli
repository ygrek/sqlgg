

type 'run migration

type 'run step = private {
  migration_id : Migration_id.t;
  batch : int;
  run : 'run;
}

type error = [
  | `Duplicate_id of Migration_id.t
  | `Out_of_order of Migration_id.t * Migration_id.t
  | `Unknown_stored_id of Migration_id.t
  | `Missing_stored_id of Migration_id.t
  | `Non_final_state of Migration_id.t * Migration_state.t
  | `Non_monotonic_batch of int * int
  | `Missing_batch of int
  | `Reappeared_batch of int
  | `Irreversible of Migration_id.t
  | `Batch_overflow
]

val reversible :
  Migration_id.t ->
  apply:'run ->
  revert:'run ->
  'run migration

val irreversible :
  Migration_id.t ->
  apply:'run ->
  'run migration

val pending :
  'run migration list ->
  Migration_state.row list ->
  (Migration_id.t list, error) result

val migrate :
  'run migration list ->
  Migration_state.row list ->
  ('run step list, error) result

val rollback :
  'run migration list ->
  Migration_state.row list ->
  ('run step list, error) result

val error_to_string : error -> string
