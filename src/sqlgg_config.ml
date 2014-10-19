(**  *)

(** Sqlgg version *)
let version = Version.id

(** Debug level *)
let debug_level = ref 0

let debug1 () = !debug_level > 0

let gen_header = ref true
