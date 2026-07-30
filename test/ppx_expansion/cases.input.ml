type product = { id : int64; name : string option } [@@deriving sqlgg]

type renamed = { id : int64; productName : string option [@sqlgg.col "name"] }
[@@deriving sqlgg]

type same_column_twice = { id : int64; also_id : int64 [@sqlgg.col "id"] }
[@@deriving sqlgg]

type converted = { id : int64; reply_count : int [@sqlgg.map Int64.to_int] }
[@@deriving sqlgg]

type deferred = { id : int64; reply_count : int [@sqlgg.by] } [@@deriving sqlgg]

type deferred_with_default = {
  id : int64;
  reply_count : int; [@sqlgg.by] [@sqlgg.map Int64.to_int]
}
[@@deriving sqlgg]

type with_default = { id : int64; hits : int [@sqlgg.default 0] }
[@@deriving sqlgg]

type trimmed = { id : int64; name : string [@sqlgg.set String.trim] }
[@@deriving sqlgg]

type nested = { id : int64; product : product [@sqlgg.nested] } [@@deriving sqlgg]

type nested_qualified = { id : int64; channel : Feed.channel [@sqlgg.nested] }
[@@deriving sqlgg]

type t = { id : int64; label : string [@sqlgg.col "name"] } [@@deriving sqlgg]

type variant = A | B [@@deriving sqlgg]

type 'a parameterised = { id : 'a } [@@deriving sqlgg]
