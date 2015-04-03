
let (|>) x f = f x
let (@@) f x = f x
let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"
let flip f x y = f y x
