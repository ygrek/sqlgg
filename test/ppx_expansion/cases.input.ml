type who = { id : int64; name : string option } [@@deriving sqlgg]

type renamed = { id : int64; productName : string option [@sqlgg.col "name"] }
[@@deriving sqlgg]

type bad = A | B [@@deriving sqlgg]

type 'a poly = { id : 'a } [@@deriving sqlgg]
