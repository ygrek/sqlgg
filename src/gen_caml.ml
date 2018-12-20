(* OCaml code generation *)

open ExtLib
open Prelude
open Printf

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
let comment () fmt = Printf.kprintf (indent_endline $ make_comment) fmt

let empty_line () = print_newline ()

module L = struct
  let as_lang_type = function
  | Type.Blob -> Type.to_string Type.Text
  | t -> Type.to_string t

  let as_api_type = as_lang_type
end

let get_column index attr =
  sprintf "(T.get_column_%s stmt %u)"
    (L.as_lang_type attr.domain)
    index

module T = Translate(L)

(* open L *)
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
    List.mapi get_column schema |> String.concat ", " |> indent_endline);
  output "in";
  name

let output_schema_binder index schema kind =
  match schema with
  | [] -> "execute",""
  | _ -> match kind with
         | Stmt.Select (`Zero_one | `One) -> "select1", output_select1_cb index schema
         | _ -> "select",output_schema_binder index schema

let is_callback stmt =
  match stmt.schema, stmt.kind with
  | [],_ -> false
  | _, Stmt.Select (`Zero_one | `One) -> false
  | _ -> true

let params_to_values = List.map fst $ params_to_values

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

let generate_stmt style index stmt =
  let name = choose_name stmt.props stmt.kind index |> String.uncapitalize in
  let subst = Props.get_all stmt.props "subst" in
  let values = (subst @ params_to_values stmt.params) |> List.map (prepend "~") |> inline_values in
  match style, is_callback stmt with
  | (`List | `Fold), false -> ()
  | _ ->
  let all_params = values ^ (if style = `List || is_callback stmt then " callback" else "") ^ (if style = `Fold then " acc" else "") in
  output "let %s db %s =" name all_params;
  inc_indent ();
  let sql = quote (get_sql stmt) in
  let sql = match subst with
  | [] -> sql
  | vars ->
    output "let __sqlgg_sql =";
    output "  let replace_all ~str ~sub ~by =";
    output "    let rec loop str = match ExtString.String.replace ~str ~sub ~by with";
    output "    | true, str -> loop str";
    output "    | false, s -> s";
    output "    in loop str";
    output "  in";
    output "  let sql = %s in" sql;
    List.iter begin fun var ->
      output "  let sql = replace_all ~str:sql ~sub:(\"%%%%%s%%%%\") ~by:%s in" var var;
    end vars;
    output "  sql";
    output "in";
    "__sqlgg_sql"
  in
  let (func,callback) = output_schema_binder index stmt.schema stmt.kind in
  let params_binder_name = output_params_binder index stmt.params in
  if style = `Fold then output "let r_acc = ref acc in";
  if style = `List then output "let r_acc = ref [] in";
  let (bind, callback) =
    match style with
    | `Fold -> "IO.(>>=) (", sprintf "(fun x -> r_acc := %s x !r_acc))" callback
    | `List -> "IO.(>>=) (", sprintf "(fun x -> r_acc := %s x :: !r_acc))" callback
    | `Direct -> "", callback (* or empty string *)
  in
  output "%sT.%s db %s %s %s" bind func sql params_binder_name callback;
  if style = `Fold then output "(fun () -> IO.return !r_acc)";
  if style = `List then output "(fun () -> IO.return (List.rev !r_acc))";
  dec_indent ();
  empty_line ()

let generate ~gen_io name stmts =
(*
  let types =
    String.concat " and " (List.map (fun s -> sprintf "%s = T.%s" s s) ["num";"text";"any"])
  in
*)
  let (traits, io) =
    match gen_io with
    | true -> "Sqlgg_traits.M_io", "T.IO"
    | false -> "Sqlgg_traits.M", "Sqlgg_io.Blocking"
  in
  output "module %s (T : %s) = struct" (String.capitalize name) traits;
  empty_line ();
  inc_indent ();
  output "module IO = %s" io;
  empty_line ();
  List.iteri (generate_stmt `Direct) stmts;
  output "module Fold = struct";
  inc_indent ();
  List.iteri (generate_stmt `Fold) stmts;
  dec_indent ();
  output "end (* module Fold *)";
  output "";
  output "module List = struct";
  inc_indent ();
  List.iteri (generate_stmt `List) stmts;
  dec_indent ();
  output "end (* module List *)";
  dec_indent ();
  output "end (* module %s *)" (String.capitalize name)

module Generator_base = struct

  type t = unit

  let start () = ()

  let comment = comment

  let empty_line = empty_line
end

module Generator = struct
  include Generator_base
  let generate () name stmts = generate ~gen_io:false name stmts
end

module Generator_io = struct
  include Generator_base
  let generate () name stmts = generate ~gen_io:true name stmts
end
