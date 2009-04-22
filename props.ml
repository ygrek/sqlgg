(* $Id$ *)

(** Association list *)
type t = (string * string) list 
 deriving (Show)

let get x n = try Some (List.assoc n x) with Not_found -> None
let set x n v = (n,v)::(List.remove_assoc n x)
let empty = []
