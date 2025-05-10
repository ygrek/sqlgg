
type mode = Normal | Ignore | Ident
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident

let stmt_metadata: (int, (string * string) list) Hashtbl.t = Hashtbl.create 16
