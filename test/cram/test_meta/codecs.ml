module Of_int64 = struct
  type t = Id of int64
  let get_column x = Id x
  let set_param (Id x) = x
  let get_column_nullable x = Option.map get_column x
  let set_param_nullable x = Option.map set_param x
end

module Of_string = struct
  type t = S of string
  let get_column x = S x
  let set_param (S x) = x
  let get_column_nullable x = Option.map get_column x
  let set_param_nullable x = Option.map set_param x
end

module Of_float = struct
  type t = F of float
  let get_column x = F x
  let set_param (F x) = x
  let get_column_nullable x = Option.map get_column x
  let set_param_nullable x = Option.map set_param x
end

module Cid = Of_int64
module Company_id = Of_int64
module Course_id = Of_int64
module Db_int = Of_int64
module Left_id = Of_int64
module Right_id = Of_int64
module Order_id = Of_int64
module Owner_id = Of_int64
module Row_status = Of_string
module Slug = Of_string
module Status = Of_string
module Money = Of_float

module Order_status = struct
  type t = Of_db of string
  let of_db x = Of_db x
  let to_db (Of_db x) = x
end
