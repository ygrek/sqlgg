(**  *)

(** Sqlgg version *)
let version = Version.id

(** Debug level *)
let debug_level = ref 0

let set_debug_level n =
  debug_level := n;
  if n > 1 then Syntax.debug := true

let debug1 () = !debug_level > 0

let gen_header : [ `Full | `Without_timestamp ] option ref = ref (Some `Full)
