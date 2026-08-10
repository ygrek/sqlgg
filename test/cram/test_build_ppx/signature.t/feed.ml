type channel = { channel_id : int64; channel_name : string option }
[@@deriving sqlgg ~nullable_cols]
