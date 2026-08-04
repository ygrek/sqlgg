type product = {
  id : int64;
  name : string option; [@sqlgg.col "title"]
  hits : int64; [@sqlgg.default 0L]
  n : int; [@sqlgg.by]
}
[@@deriving sqlgg]

type counted = {
  cid : int64;
  reply_count : int; [@sqlgg.map Int64.to_int]
  label : string;
}
[@@deriving sqlgg]

type content = { text : string option [@sqlgg.col "body"] } [@@deriving sqlgg]

type post = {
  post_id : int64;
  content : content; [@sqlgg.nested]
  channel : Feed.channel; [@sqlgg.nested]
}
[@@deriving sqlgg]

type feed_item = {
  item_id : int64;
  channel : Feed.channel option; [@sqlgg.nested]
}
[@@deriving sqlgg]
