(* OCaml code generation *)

open ExtList
open ExtString
open Operators
open Printf

open Stmt
open Gen
open Sql

let inline_values = String.concat " "

let quote = String.replace_chars (function '\n' -> "\\n\\\n" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let rec replace_all ~str ~sub ~by =
  match String.replace ~str ~sub ~by with
  | (true,s) -> replace_all ~str:s ~sub ~by
  | (false,s) -> s

let quote_comment_inline str =
  let str = replace_all ~str ~sub:"*)" ~by:"* )" in
  replace_all ~str ~sub:"(*" ~by:"( *"

let make_comment str = "(* " ^ (quote_comment_inline str) ^ " *)"
let comment fmt = Printf.kprintf (indent_endline & make_comment) fmt

let get_column attr index =
  output "(T.get_column_%s stmt %u)"
    (Type.to_string attr.RA.domain)
    index

let param_type_to_string t = Option.map_default Type.to_string "Any" t

let set_param index param =
  let (id,t) = param in
  output "T.set_param_%s stmt %u %s;"
    (param_type_to_string t)
    index
    (param_name_to_string id index)

let output_scheme_binder index scheme =
  let name = "invoke_callback" in
  output "let %s stmt =" name;
  inc_indent ();
  output "callback";
  inc_indent ();
  List.iteri (fun index attr -> get_column attr index) scheme;
  dec_indent ();
  dec_indent ();
  output "in";
  name

let output_scheme_binder index scheme =
  match scheme with
  | [] -> None
  | _ -> Some (output_scheme_binder index scheme)

let params_to_values = List.mapi (fun i (n,_) -> param_name_to_string n i)
let params_to_values = List.unique & params_to_values

let output_params_binder index params =
  output "let set_params stmt =";
  inc_indent ();
  List.iteri set_param params;
  output "()";
  dec_indent ();
  output "in";
  "set_params"

let output_params_binder index params =
  match params with
  | [] -> "(fun _ -> ())"
  | _ -> output_params_binder index params

let prepend prefix = function s -> prefix ^ s

let generate_code index scheme params kind props =
  let name = choose_name props kind index >> String.uncapitalize in
  let values = params_to_values params >> List.map (prepend "~") >> inline_values in
  let all_params = match scheme with [] -> values | _ -> "callback " ^ values in
  output "let %s db %s =" name all_params;
  inc_indent ();
  let sql = quote (get_sql props kind params) in
  let scheme_binder_name = output_scheme_binder index scheme in
  let params_binder_name = output_params_binder index params in
  begin match scheme_binder_name with
  | None ->
      output "T.execute db %s %s" sql params_binder_name
  | Some scheme_binder_name ->
      output "T.select db %s %s %s" sql scheme_binder_name params_binder_name
  end;
  dec_indent ();
  empty_line ()

let start_output () =
  output "module Sqlgg (T : Sqlgg_traits.M) = struct";
  empty_line ();
  inc_indent ()

let finish_output () =
  dec_indent ();
  output "end (* module Sqlgg *)"

