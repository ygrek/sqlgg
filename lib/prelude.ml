
exception At of ((int * int) * exn)

let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"

let const c _ = c
let flip f x y = f y x

let tuck l x = l := x :: !l
let option_list = function Some x -> [x] | None -> []

let assoc_string name l = List.find_map (fun (key, x) -> if String.equal key name then Some x else None) l

let hashtbl_restore h s = Hashtbl.clear h; Hashtbl.iter (Hashtbl.replace h) s

let unique_by (type k) (module Key : Set.OrderedType with type t = k) key l =
  let module Seen = Set.Make (Key) in
  List.fold_left (fun (seen, kept) x ->
    let k = key x in
    if Seen.mem k seen then seen, kept else Seen.add k seen, x :: kept) (Seen.empty, []) l
  |> snd |> List.rev

let fail fmt = Printf.ksprintf failwith fmt
let failed ~at fmt = Printf.ksprintf (fun s -> raise (At (at, Failure s))) fmt
let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
