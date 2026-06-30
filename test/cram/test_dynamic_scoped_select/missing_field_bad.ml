module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  type wide = { id : int64; price : float option } [@@deriving sqlgg]

  let _bad db =
    Dscope_q2.(select db (wide_of_cols cols) ~min_stock:10L (fun _ -> ()))
end
