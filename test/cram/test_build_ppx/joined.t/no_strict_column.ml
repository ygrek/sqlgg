type note = { note : string option; tag : string option }
[@@deriving sqlgg ~nullable_cols]

type row = { id : int64; note : note option [@sqlgg.nested] }
[@@deriving sqlgg]
