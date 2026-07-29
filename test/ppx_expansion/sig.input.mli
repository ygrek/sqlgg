type product = { id : int64; name : string option } [@@deriving sqlgg]

type renamed = { id : int64; productName : string option [@sqlgg.col "name"] }
[@@deriving sqlgg]

type with_default = { id : int64; hits : int [@sqlgg.default 0] }
[@@deriving sqlgg]

type deferred = { id : int64; reply_count : int [@sqlgg.by] } [@@deriving sqlgg]

type t = { id : int64 } [@@deriving sqlgg]

type converted = { id : int64; reply_count : int [@sqlgg.map: int64] }
[@@deriving sqlgg]

type trimmed = { id : int64; name : string [@sqlgg.set: string] }
[@@deriving sqlgg]

type map_without_a_type = { id : int64; n : int [@sqlgg.map Int64.to_int] }
[@@deriving sqlgg]

type nested = { id : int64; product : product [@sqlgg.nested] }
[@@deriving sqlgg]
