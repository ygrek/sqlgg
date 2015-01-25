(** Global list of tables *)

open Printf
open ExtLib

type table = RA.table

let all : table list ref = ref []

(** FIXME table names case sensitivity? *)
let by_name name = fun (n,_) -> n = name

(** @raise Error when no such table *)
let get_from tables name =
  try
    List.find (by_name name) tables
  with Not_found -> failwith (sprintf "no such table %s" name)

let get name = get_from !all name
let get_schema name = snd (get name)
let check name = ignore (get name)

let add v =
  let (name,_) = v in
  match List.find_all (by_name name) !all with
  | [] -> all := v :: !all
  | _ -> failwith (sprintf "table %s already exists" name)

let drop name = check name; all := List.remove_if (by_name name) !all

let alter name f =
  check name;
  let alter_scheme ((n,s) as table) =
    if n = name then
      name, f s
    else
      table
  in
  all := List.map alter_scheme !all

let alter_add name col pos = alter name (fun s -> RA.Schema.add s col pos)
let alter_drop name col = alter name (fun s -> RA.Schema.drop s col)
let alter_change name oldcol col pos = alter name (fun s -> RA.Schema.change s oldcol col pos)

let print () = let out = IO.output_channel stdout in List.iter (RA.print_table out) !all
let print1 name = RA.print_table (IO.output_channel stdout) (get name)

let reset () = all := []

