type cc = { cid : int64; cname : string } [@@deriving sqlgg ~nullable_cols]
type bb = { bid : int64; cc : cc option [@sqlgg.nested] }
[@@deriving sqlgg ~nullable_cols]
type aa = { aid : int64; bb : bb option [@sqlgg.nested] } [@@deriving sqlgg]

module Db = Joins.Sqlgg (Print_impl)

let _ = aa_of_cols Db.Left_then_inner.cols
