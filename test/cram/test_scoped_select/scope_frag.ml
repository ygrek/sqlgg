module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql
  open Sqlgg_scope

  type who = { id : int64; name : string option }

  let who_fr cols =
    let+ id = cols#id and+ name = cols#name in { id; name }

  let _q1 db = Scope_q1.select db (who_fr Scope_q1.cols) ~id:1L
  let _q2 db =
    Scope_q2.select db (who_fr Scope_q2.cols) ~min_stock:10L (fun _ -> ())
end
