module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let _mixed =
    Sqlgg_scope.apply
      (Sqlgg_scope.apply
         (Sqlgg_scope.pure (fun a b -> (a, b)))
         Q1.id)
      Q2.category
end
