type channel = { channel_id : int64; hits : int [@sqlgg.by] }
[@@deriving sqlgg ~nullable_cols]

type post = { id : int64; channel : channel option [@sqlgg.nested] } [@@deriving sqlgg]
