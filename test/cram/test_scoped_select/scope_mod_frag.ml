module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  type who = { id : Product_id.t; name : string option } [@@deriving sqlgg]

  let _q db =
    Wrapped_scope.(select db (who_of_cols cols)
      ~id:(Product_id.get_column 1L))
end
