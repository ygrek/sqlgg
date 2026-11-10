module Wrapped_id : sig
  type t = Id of int64
end

val find_user :
  id:'t Sqlgg_request.int_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val rename_user :
  name:'t Sqlgg_request.text_of option -> id:'t Sqlgg_request.int_of -> (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_wide :
  c1:'t Sqlgg_request.int_of -> c2:'t Sqlgg_request.int_of -> c3:'t Sqlgg_request.int_of -> c4:'t Sqlgg_request.int_of ->
  c5:'t Sqlgg_request.int_of -> c6:'t Sqlgg_request.int_of -> c7:'t Sqlgg_request.int_of -> c8:'t Sqlgg_request.int_of ->
  c9:'t Sqlgg_request.int_of -> c10:'t Sqlgg_request.int_of -> c11:'t Sqlgg_request.int_of -> c12:'t Sqlgg_request.int_of ->
  (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t

val find_wide :
  c1:'t Sqlgg_request.int_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of
   * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of
   * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of * 't Sqlgg_request.int_of,
   't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val post_state :
  id:'t Sqlgg_request.int_of -> ([ `Draft | `Published ], 't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val publish :
  state:[ `Draft | `Published ] ->
  id:'t Sqlgg_request.int_of ->
  (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t

val users_by_ids :
  ids:'t Sqlgg_request.int_of list ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option, 't, Sqlgg_request.many) Sqlgg_request.t

val insert_users :
  rows:('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option * 't Sqlgg_request.text_of) list ->
  (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t

val find_by :
  filter:[< `By_id of 't Sqlgg_request.int_of | `By_name of 't Sqlgg_request.text_of ] ->
  ('t Sqlgg_request.int_of, 't, Sqlgg_request.many) Sqlgg_request.t

val find_after :
  min:'t Sqlgg_request.int_of -> name_filter:[< `None | `Some of 't Sqlgg_request.text_of ] ->
  ('t Sqlgg_request.int_of, 't, Sqlgg_request.many) Sqlgg_request.t

val users_cte :
  id:'t Sqlgg_request.int_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val users_in_sub :
  id:'t Sqlgg_request.int_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.text_of option, 't, Sqlgg_request.many) Sqlgg_request.t

val untyped :
  cond:'t Sqlgg_request.any_of ->
  ('t Sqlgg_request.int_of * 't Sqlgg_request.any_of option, 't, Sqlgg_request.many) Sqlgg_request.t

val wrapped_user :
  id:Wrapped_id.t ->
  (Wrapped_id.t * Wrapped_id.t option, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t

val wrapped_by_ids :
  ids:Wrapped_id.t list ->
  (Wrapped_id.t, 't, Sqlgg_request.many) Sqlgg_request.t

val insert_wrapped :
  rows:(Wrapped_id.t * Wrapped_id.t option) list ->
  (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t
