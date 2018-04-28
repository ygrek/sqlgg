
let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"
let flip f x y = f y x

let tuck l x = l := x :: !l

let fail fmt = Printf.ksprintf failwith fmt
let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
