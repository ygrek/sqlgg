type t = int64

let get_column (x : int64) : t = x
let get_column_nullable (x : int64 option) : t option = x
let set_param (x : t) : int64 = x
