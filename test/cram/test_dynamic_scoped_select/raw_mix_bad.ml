module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let _bad db =
    Dscope_q2.select db Dscope_q1.Cols.name ~min_stock:10L (fun _ -> ())
end
