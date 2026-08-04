type product = { id : int64 } [@@deriving sqlgg]

type map_and_default = {
  stock : int; [@sqlgg.map Int64.to_int] [@sqlgg.default 0]
}
[@@deriving sqlgg]

type default_on_option = { name : string option [@sqlgg.default "unnamed"] }
[@@deriving sqlgg]

type nested_and_col = {
  product : product; [@sqlgg.nested] [@sqlgg.col "product_id"]
}
[@@deriving sqlgg]

type nested_not_a_record = { dimensions : int64 * int64 [@sqlgg.nested] }
[@@deriving sqlgg]

type nested_option_not_a_record = {
  dimensions : (int64 * int64) option; [@sqlgg.nested]
}
[@@deriving sqlgg]

type nested_option_and_col = {
  product : product option; [@sqlgg.nested] [@sqlgg.col "product_id"]
}
[@@deriving sqlgg]

type default_none_needs_option = {
  product : product; [@sqlgg.nested default_none]
}
[@@deriving sqlgg]

type map_without_a_function = {
  id : int64;
  n : int; [@sqlgg.map : int64 -> int]
}
[@@deriving sqlgg]
