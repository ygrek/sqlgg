module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  type who = { id : int64; name : string option } [@@deriving sqlgg]

  let _q1 db = Dscope_q1.(select db (who_of_cols cols) ~id:1L)
  let _q2 db =
    Dscope_q2.(select db (who_of_cols cols) ~min_stock:10L (fun _ -> ()))
end
