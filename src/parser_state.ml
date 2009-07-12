
type mode = Normal | Ignore
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore

