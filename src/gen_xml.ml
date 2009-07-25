(** XML generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

type xml = | Node of (string * (string * string) list * xml list)
           | Comment of string

let xml_escape s =
  let b = Buffer.create 10 in
  let add s = Buffer.add_string b s in
  String.iter (function
  | '&' -> add "&amp;"
  | '"' -> add "&quot;"
  | '\n' -> add "&#x0A;"
  | '\r' -> ()
  | '<' -> add "&lt;"
  | '>' -> add "&gt;"
  | c -> Buffer.add_char b c) s;
  Buffer.contents b

let xml_to_string xml =
  let b = Buffer.create 1000 in
  let rec iter spaces = function
    | Node (name,attrs,children) ->
        bprintf b "\n%s<%s" spaces name;
        List.iter (fun (n,v) -> bprintf b " %s=\"%s\"" n (xml_escape v)) attrs;
        begin match children with
        | [] -> bprintf b "/>"
        | _ -> bprintf b ">"; List.iter (iter (spaces ^ " ")) children; bprintf b "\n%s</%s>" spaces name
        end
    | Comment text -> bprintf b "\n<!-- %s -->" (Gen_caml.replace_all ~str:text ~sub:"--" ~by:"&mdash;")
  in
  iter "" xml;
  Buffer.contents b

(*
let _ =
  Node ("test",["name","d\"s&quot;ds"],[]) >> xml_to_string >> print_endline
*)

let comment (x,_) fmt = Printf.ksprintf (fun s -> x := Comment s :: !x) fmt
let empty_line _ = ()

let value n t = Node ("value",["name",n; "type",t;],[])

open Gen_caml.L
open Gen_caml.T

let params_to_values = List.map (fun (n,t) -> value n t) & all_params_to_values
let schema_to_values = List.map (fun (n,t) -> value n t) & schema_to_values

type t = xml list ref * xml list ref

let start () = ref [], ref []

let generate_code (x,_) index stmt =
  let name = choose_name stmt.props stmt.kind index in
  let input = Node ("in",[],params_to_values stmt.params) in
  let output = Node ("out",[],schema_to_values stmt.schema) in
  let sql = get_sql stmt in
  x := Node ("stmt",["name",name; "sql",sql;],[input; output]) :: !x

let start_output (x,pre) = pre := !x; x := []

let finish_output (x,pre) =
  print_endline "<?xml version=\"1.0\"?>";
  List.iter (fun z -> z >> xml_to_string >> print_endline) (List.rev !pre);
  Node ("sqlgg",[],List.rev !x) >> xml_to_string >> print_endline;
  x := [];
  pre := []

let generate out _ stmts =
  start_output out;
  Enum.iteri (generate_code out) stmts;
  finish_output out

