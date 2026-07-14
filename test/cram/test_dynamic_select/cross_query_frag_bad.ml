module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)

  let _bad db = Sql.Q2.select db Sql.Q1.name ~id:1L (fun _ -> ())
end
