open Jsonkit.Primitives

type entry = {
  name : string;
  sql : string;
  select : Sql.select_full;
  props : Props.t list;
}
[@@deriving json, jsonschema]

(* latest declaration first *)
type t = entry list

let store : t ref = ref []

let add ~name ~sql ~select ~props =
  store := { name; sql; select; props } :: List.filter (fun e -> not (String.equal e.name name)) !store

let get name =
  match List.find_opt (fun e -> String.equal e.name name) !store with
  | Some entry -> entry
  | None -> Prelude.fail "Shared query %s not found" name

let entries () = List.rev !store

let snapshot () = !store
let restore s = store := s
