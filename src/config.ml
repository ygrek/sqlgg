(**  *)

(** Sqlgg version *)
let version = Git.revision

(** Debug level *)
let debug_level = ref 0

let debug1 () = !debug_level > 0

