(** Global list of types *)

open Printf

type registry = (string, Sql.Type.kind) Hashtbl.t

let registry : registry = Hashtbl.create 8

let add name kind =
  match Hashtbl.mem registry name with
  | true -> failwith (sprintf "duplicate type declaration for %S" name)
  | false -> Hashtbl.add registry name kind

let drop name =
  match Hashtbl.mem registry name with
  | true -> Hashtbl.remove registry name
  | false -> failwith (sprintf "no such type %S" name)

let get_opt = Hashtbl.find_opt registry

let get = Hashtbl.find registry

let reset () = Hashtbl.reset registry
