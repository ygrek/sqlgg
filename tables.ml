(** Global list of tables *)

open Printf

type table = RA.table

let all : table list ref = ref []

(** @raise Error when no such table *) 
let get_from tables name = 
  try 
    List.find (fun (n,_) -> n = name) tables 
  with Not_found -> failwith (sprintf "no such table %s" name)

let get name = get_from !all name
let get_scheme name = snd (get name)

let add v = all := v :: !all

let print () = List.iter RA.print_table !all
