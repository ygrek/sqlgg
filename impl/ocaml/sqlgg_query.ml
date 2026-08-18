type cardinality = Zero_one | One | Nat

type kind =
  | Select of cardinality
  | Insert of string
  | Create of string
  | CreateIndex of string
  | Update of string option
  | Delete of string list
  | Alter of string list
  | Drop of string
  | CreateRoutine of string
  | CreateType of string
  | DropType of string
  | Other

type t = {
  sql : string;
  name : string;
  kind : kind;
}

let make ~sql ~name ~kind = { sql; name; kind }

let append_comment q comment = { q with sql = q.sql ^ " " ^ comment }

module Sqlcommenter = struct

let url_encode s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' -> Buffer.add_char b c
    | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) s;
  Buffer.contents b

let comment attrs =
  attrs
  |> List.map (fun (k, v) -> url_encode k, url_encode v)
  |> List.sort compare
  |> List.map (fun (k, v) -> Printf.sprintf "%s='%s'" k v)
  |> String.concat ","
  |> Printf.sprintf "/*%s*/"

let annotate attrs q =
  match attrs with
  | [] -> q
  | attrs -> append_comment q (comment attrs)

end
