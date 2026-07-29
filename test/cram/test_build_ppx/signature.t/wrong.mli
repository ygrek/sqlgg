type t = { id : int64; n : int [@sqlgg.map Int64.to_int] } [@@deriving sqlgg]
