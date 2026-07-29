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
