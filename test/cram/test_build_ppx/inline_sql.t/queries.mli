type user = {
  id : int64;
  name : string option;
}
[@@deriving sqlgg]

module Make (T : Sqlgg_traits.M_io) : sig
  val find_user :
    [> `RO ] T.connection ->
    id:T.Types.Int.t ->
    (T.Types.Int.t * T.Types.Text.t option) option T.IO.future

  val add_user :
    [> `WR ] T.connection ->
    id:T.Types.Int.t ->
    name:T.Types.Text.t option ->
    email:T.Types.Text.t ->
    T.execute_response T.IO.future

  val add_users :
    [> `WR ] T.connection ->
    rows:(T.Types.Int.t * T.Types.Text.t option * T.Types.Text.t) list ->
    T.execute_response T.IO.future

  val rename_user :
    [> `WR ] T.connection ->
    name:T.Types.Text.t option ->
    nickname:T.Types.Text.t option ->
    id:T.Types.Int.t ->
    T.execute_response T.IO.future

  val titles_by :
    [> `RO ] T.connection ->
    author:T.Types.Int.t ->
    state:[ `Draft | `Published ] ->
    (title:T.Types.Text.t -> nickname:T.Types.Text.t option -> unit) ->
    unit T.IO.future

  val publish :
    [> `WR ] T.connection ->
    state:[ `Draft | `Published ] ->
    id:T.Types.Int.t ->
    T.execute_response T.IO.future
end
