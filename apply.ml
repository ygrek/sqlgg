
external id : 'a -> 'a = "%identity"
let catch f x = try Some (f x) with _ -> None
let tee f x = f x; x

