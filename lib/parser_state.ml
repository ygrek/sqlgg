
type mode = Normal | Ignore | Ident
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

  let add_feature feature = state := feature :: !state

  let set_collation value pos = add_feature @@ Dialect.get_collation value pos
  let set_join_source e pos = add_feature @@ Dialect.get_join_source e pos
  let set_create_table_as_select pos = add_feature @@ Dialect.get_create_table_as_select pos
  
  let reset () = state := []
end

let next_statement () =
  Hashtbl.reset Stmt_metadata.stmt_metadata;
  Dialect_feature.reset ()
