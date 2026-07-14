module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let _q1 db =
    Q1.(select db (let+ id = id and+ name = name in (id, name)) ~id:1L (fun _ -> ()))

  let _q2 db =
    Q2.(select db category ~id:1L (fun _ -> ()))
end
