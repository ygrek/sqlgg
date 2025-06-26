(**  *)
open Sqlgg

(** Sqlgg version *)
let version = Version.id

(** Debug level *)
let debug_level = ref 0

let set_debug_level n =
  debug_level := n;
  if n > 1 then Syntax.debug := true

let debug1 () = !debug_level > 0

let gen_header : [ `Full | `Without_timestamp | `Static ] option ref = ref (Some `Full)

let include_category : [ `All | `None | `Only of Stmt.category list | `Except of Stmt.category list ] ref = ref `All

let enum_as_poly_variant = ref false

let dialect = ref Dialect.MySQL
let set_dialect d = dialect := d

let no_check_features: Dialect.feature list ref = ref []

let set_no_check_features l = no_check_features := l
