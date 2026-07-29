type counted = { id : int64; n : int [@sqlgg.by] } [@@deriving sqlgg]

let no_conversion cols : (counted, _, _, _) Sqlgg_scope.col = counted_of_cols cols
