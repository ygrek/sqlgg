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
let comment () fmt = Printf.kprintf (indent_endline & make_comment) fmt

let empty_line () = print_newline ()

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

let output_schema_binder index schema =
  let name = "invoke_callback" in
  output "let %s stmt =" name;
  inc_indent ();
  output "callback";
  inc_indent ();
  List.iteri (fun index attr -> get_column attr index) schema;
  dec_indent ();
  dec_indent ();
  output "in";
  name

let output_schema_binder index schema =
  match schema with
  | [] -> None
  | _ -> Some (output_schema_binder index schema)

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

type t = unit

let start () = ()

let generate_stmt index stmt =
  let name = choose_name stmt.props stmt.kind index >> String.uncapitalize in
  let values = params_to_values stmt.params >> List.map (prepend "~") >> inline_values in
  let all_params = match stmt.schema with [] -> values | _ -> "callback " ^ values in
  output "let %s db %s =" name all_params;
  inc_indent ();
  let sql = quote (get_sql stmt.props stmt.kind stmt.params) in
  let schema_binder_name = output_schema_binder index stmt.schema in
  let params_binder_name = output_params_binder index stmt.params in
  begin match schema_binder_name with
  | None ->
      output "T.execute db %s %s" sql params_binder_name
  | Some schema_binder_name ->
      output "T.select db %s %s %s" sql schema_binder_name params_binder_name
  end;
  dec_indent ();
  empty_line ()

let generate () name stmts =
  output "module %s (T : Sqlgg_traits.M) = struct" (String.capitalize name);
  empty_line ();
  inc_indent ();
  Enum.iteri generate_stmt stmts;
  dec_indent ();
  output "end (* module %s *)" (String.capitalize name)

