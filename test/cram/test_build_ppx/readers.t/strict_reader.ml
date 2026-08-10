type stock = { sid : int64; place : string } [@@deriving sqlgg]

module Db = Shop.Sqlgg (Print_impl)

let _ = stock_of_cols Db.Wide.cols
