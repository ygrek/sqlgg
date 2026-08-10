type product = { id : int64; name : string option } [@@deriving sqlgg ~nullable_cols]

type renamed = { id : int64; productName : string option [@sqlgg.col "name"] }
[@@deriving sqlgg]

type with_default = { id : int64; hits : int [@sqlgg.default 0] }
[@@deriving sqlgg]

type deferred = { id : int64; reply_count : int [@sqlgg.by] } [@@deriving sqlgg]

type t = { id : int64 } [@@deriving sqlgg]

type converted = { id : int64; reply_count : int [@sqlgg.map: int64] }
[@@deriving sqlgg]

type trimmed = { id : int64; name : string }
[@@deriving sqlgg]

type map_without_a_type = { id : int64; n : int [@sqlgg.map Int64.to_int] }
[@@deriving sqlgg]

type nested = { id : int64; product : product [@sqlgg.nested] }
[@@deriving sqlgg]

type optional_relation = { id : int64; product : product option [@sqlgg.nested] }
[@@deriving sqlgg]

type all_option = { note : string option } [@@deriving sqlgg]

type with_nested = { a : int64; product : product [@sqlgg.nested] }
[@@deriving sqlgg ~nullable_cols]

type nested_in_relation = {
  id : int64;
  inner : with_nested option; [@sqlgg.nested]
}
[@@deriving sqlgg]

type relation_and_conversions = {
  id : int64;
  product : product option; [@sqlgg.nested]
  reply_count : int; [@sqlgg.by]
}
[@@deriving sqlgg]
