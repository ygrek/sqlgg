(* OCaml code generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

(* let escape = String.replace_chars (function '\n' -> "&#x0A;" | '\r' -> "" | '"' -> "&amp;" | c -> String.make 1 c) *)
let escape x = x

let comment x fmt = Printf.ksprintf (ignore) fmt

let value n t = Xml.Element ("value",["name",n; "type",t;],[])

let param_type_to_string t = Option.map_default Type.to_string "Any" t
let params_to_values = List.mapi (fun i (n,t) -> value (param_name_to_string n i) (param_type_to_string t))
let params_to_values = List.unique & params_to_values

let scheme_to_values = List.map (fun attr -> value "" (Type.to_string attr.RA.domain))

type t = Xml.xml list ref

let start () = ref []

let generate_code x index scheme params kind props =
  let name = choose_name props kind index in
  let input = Xml.Element ("in",[],params_to_values params) in
  let output = Xml.Element ("out",[],scheme_to_values scheme) in
  let sql = escape (get_sql props kind params) in
  x:= Xml.Element ("stmt",["name",name; "sql",sql;],[input; output]) :: !x

let start_output x = ()

let finish_output x =
  Xml.Element ("sqlgg",[],!x) >> Xml.to_string_fmt >> print_endline;
  x := []

