
let ($) f g = function x -> f (g x)
let (&) f x = f x
let (>>) x f = f x

external id : 'a -> 'a = "%identity"
let catch f x = try Some (f x) with _ -> None
let tee f x = f x; x
let flip f x y = f y x

