val find_user :
  id:int64 -> (int64 * string option, Sqlgg_request.zero_or_one) Sqlgg_request.t

val rename_user :
  name:string option -> id:int64 -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_wide :
  c1:int64 -> c2:int64 -> c3:int64 -> c4:int64 -> c5:int64 -> c6:int64 -> c7:int64 -> c8:int64 -> c9:int64 -> c10:int64 -> c11:int64 -> c12:int64 ->
  (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val find_wide :
  c1:int64 -> (int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64, Sqlgg_request.zero_or_one) Sqlgg_request.t

val post_state :
  id:int64 -> ([ `Draft | `Published ], Sqlgg_request.zero_or_one) Sqlgg_request.t

val publish :
  state:[ `Draft | `Published ] ->
  id:int64 ->
  (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val users_by_ids :
  ids:int64 list -> (int64 * string option, Sqlgg_request.many) Sqlgg_request.t

val insert_users :
  rows:(int64 * string option * string) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val find_by :
  filter:[< `By_id of int64 | `By_name of string ] -> (int64, Sqlgg_request.many) Sqlgg_request.t

val find_after :
  min:int64 -> name_filter:[< `None | `Some of string ] -> (int64, Sqlgg_request.many) Sqlgg_request.t
