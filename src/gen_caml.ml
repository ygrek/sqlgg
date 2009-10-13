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

module L = struct
  let as_lang_type = function
  | Type.Blob -> Type.to_string Type.String
  | t -> Type.to_string t

  let as_api_type = as_lang_type
end

let get_column index attr =
  sprintf "(T.get_column_%s stmt %u)"
    (L.as_lang_type attr.RA.domain)
    index

module T = Translate(L)

open L
open T

let output_schema_binder _ schema =
  let name = "invoke_callback" in
  output "let %s stmt =" name;
  indented (fun () ->
    output "callback";
    indented (fun () ->
      List.iteri (fun i a -> output "%s" (get_column i a)) schema));
  output "in";
  name

let output_select1_cb _ schema =
  let name = "get_row" in
  output "let %s stmt =" name;
  indented (fun () ->
    List.mapi get_column schema >> String.concat ", " >> indent_endline);
  output "in";
  name

let output_schema_binder index schema kind =
  match schema with
  | [] -> "execute",""
  | _ -> match kind with 
         | Select true -> "select1", output_select1_cb index schema
         | _ -> "select",output_schema_binder index schema

let is_callback stmt =
  match stmt.schema, stmt.kind with
  | [],_ -> false
  | _,Select true -> false
  | _ -> true

let params_to_values = List.map fst & params_to_values

let set_param index param =
  let (id,t) = param in
  output "T.set_param_%s p %u %s;"
    (param_type_to_string t)
    index
    (param_name_to_string id index)

let output_params_binder _ params =
  output "let set_params stmt =";
  inc_indent ();
  output "let p = T.start_params stmt %u in" (List.length params);
  List.iteri set_param params;
  output "T.finish_params p";
  dec_indent ();
  output "in";
  "set_params"

let output_params_binder index params =
  match params with
  | [] -> "T.no_params"
  | _ -> output_params_binder index params

let prepend prefix = function s -> prefix ^ s

type t = unit

let start () = ()

let generate_stmt index stmt =
  let name = choose_name stmt.props stmt.kind index >> String.uncapitalize in
  let values = params_to_values stmt.params >> List.map (prepend "~") >> inline_values in
  let all_params = values ^ (if is_callback stmt then " callback" else "") in
  output "let %s db %s =" name all_params;
  inc_indent ();
  let sql = quote (get_sql stmt) in
  let (func,callback) = output_schema_binder index stmt.schema stmt.kind in
  let params_binder_name = output_params_binder index stmt.params in
  output "T.%s db %s %s %s" func sql params_binder_name callback;
  dec_indent ();
  empty_line ()

let generate () name stmts =
(*
  let types = 
    String.concat " and " (List.map (fun s -> sprintf "%s = T.%s" s s) ["num";"text";"any"]) 
  in
*)
  output "module %s (T : Sqlgg_traits.M) = struct" (String.capitalize name);
  empty_line ();
  inc_indent ();
  Enum.iteri generate_stmt stmts;
  dec_indent ();
  output "end (* module %s *)" (String.capitalize name)

