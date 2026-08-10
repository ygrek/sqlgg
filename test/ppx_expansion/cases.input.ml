type product = { id : int64; name : string option } [@@deriving sqlgg ~nullable_cols]

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

type trimmed = { id : int64; name : string }
[@@deriving sqlgg]

type nested = { id : int64; product : product [@sqlgg.nested] } [@@deriving sqlgg]

type nested_qualified = { id : int64; channel : Feed.channel [@sqlgg.nested] }
[@@deriving sqlgg]

type optional_relation = { id : int64; product : product option [@sqlgg.nested] }
[@@deriving sqlgg ~nullable_cols]

type relation_qualified = {
  id : int64;
  channel : Feed.channel option; [@sqlgg.nested]
}
[@@deriving sqlgg]

type relation_and_conversions = {
  id : int64;
  product : product option; [@sqlgg.nested]
  reply_count : int; [@sqlgg.map Int64.to_int]
  hits : int; [@sqlgg.default 0]
}
[@@deriving sqlgg]

type all_option = { note : string option; tag : string option } [@@deriving sqlgg]

type opaque_conversions = {
  id : int64;
  note : string option; [@sqlgg.map Fun.id]
  tag : int option; [@sqlgg.by]
}
[@@deriving sqlgg]

type t = { id : int64; label : string [@sqlgg.col "name"] } [@@deriving sqlgg]

type variant = A | B [@@deriving sqlgg]

type 'a parameterised = { id : 'a } [@@deriving sqlgg]

type default_none_relation = {
  id : int64;
  product : product option; [@sqlgg.nested default_none]
}
[@@deriving sqlgg]

type with_nested = { a : int64; product : product [@sqlgg.nested] }
[@@deriving sqlgg ~nullable_cols]

type nested_in_relation = {
  id : int64;
  inner : with_nested option; [@sqlgg.nested]
}
[@@deriving sqlgg]

type chains_to_optional = { id : int64; rel : optional_relation [@sqlgg.nested] }
[@@deriving sqlgg]
