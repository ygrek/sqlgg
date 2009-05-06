(* $Id$ *)

let (&) f g = function x -> f (g x)
let (>>) x f = f x
