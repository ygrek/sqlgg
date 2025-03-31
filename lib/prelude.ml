
exception At of ((int * int) * exn)

let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"

let const c _ = c
let flip f x y = f y x

let tuck l x = l := x :: !l
let option_list = function Some x -> [x] | None -> []

let fail fmt = Printf.ksprintf failwith fmt
let failed ~at fmt = Printf.ksprintf (fun s -> raise (At (at, Failure s))) fmt
let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
