type row = {
  id : int64;
  name : string;
  count : string; [@sqlgg.map Int64.to_int]
  tail : int64;
}
[@@deriving sqlgg]
