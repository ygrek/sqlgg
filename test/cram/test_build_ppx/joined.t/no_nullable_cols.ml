type ch = { cid : int64; cname : string } [@@deriving sqlgg]

type top = { tid : int64; ch : ch option [@sqlgg.nested] } [@@deriving sqlgg]
