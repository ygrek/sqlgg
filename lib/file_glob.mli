type t

val create : unit -> t

val expand :
  t ->
  string ->
  (string list, [ `Malformed_pattern | `Unreadable_directory of string ]) result
