
type mode = Normal | Ignore | Ident

type column_def_extra =
  | Default of Sql.expr * Sql.pos
  | Other_extra of Sql.Constraint.t

let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident
module Stmt_metadata = struct
  let stmt_metadata: (int, (string * string) list) Hashtbl.t = Hashtbl.create 16

  let add k v = Hashtbl.add stmt_metadata k v
  let find_all k = Hashtbl.find_all stmt_metadata k
  let reset () = Hashtbl.reset stmt_metadata
end

module Dialect_feature = struct

  let state = ref []
  let function_name = ref None

  let set_function_name name = function_name := Some name

  let add_feature feature = state := feature :: !state

  let set_collation value pos = add_feature @@ Dialect.get_collation value pos
  let set_join_source e pos = add_feature @@ Dialect.get_join_source e pos
  let set_create_table_as_select pos = add_feature @@ Dialect.get_create_table_as_select pos

  let set_on_duplicate_key pos = add_feature @@ Dialect.get_on_duplicate_key pos
  let set_on_conflict pos = add_feature @@ Dialect.get_on_conflict pos

  let set_straight_join pos = add_feature @@ Dialect.get_straight_join pos
  let set_lock_in_share_mode pos = add_feature @@ Dialect.get_lock_in_share_mode pos
  let set_fulltext_index pos = add_feature @@ Dialect.get_fulltext_index pos
  let set_unsigned_types pos = add_feature @@ Dialect.get_unsigned_types pos
  let set_autoincrement pos = add_feature @@ Dialect.get_autoincrement pos
  let set_replace_into pos = add_feature @@ Dialect.get_replace_into pos
  let set_row_locking pos = add_feature @@ Dialect.get_row_locking pos
  let set_default_expr kind expr pos = add_feature @@ Dialect.get_default_expr ~function_name:!function_name ~kind ~expr pos

  let reset () = 
    state := []; 
    function_name :=  None
end
