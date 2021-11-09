(* OCaml code generation *)

open Printf
open ExtLib
open Sqlgg
open Prelude

open Gen
open Sql

module Name = struct

(* http://caml.inria.fr/pub/docs/manual-ocaml-4.07/manual049.html *)
let reserved = [
"and";
"as";
"asr";
"assert";
"begin";
"class";
"constraint";
"do";
"done";
"downto";
"else";
"end";
"exception";
"external";
"false";
"for";
"fun";
"function";
"functor";
"if";
"in";
"include";
"inherit";
"initializer";
"land";
"lazy";
"let";
"lor";
"lsl";
"lsr";
"lxor";
"match";
"method";
"mod";
"module";
"mutable";
"new";
"nonrec";
"object";
"of";
"open!";
"open";
"or";
"private";
"rec";
"sig";
"struct";
"then";
"to";
"true";
"try";
"type";
"val";
"virtual";
"when";
"while";
"with";
]

let ident ~prefix name =
  assert (prefix <> "");
  match name with
  | "" -> prefix
  | _ ->
  if List.mem name reserved then
    name ^ "_"
  else
    let name = String.map (function ('a'..'z' | 'A'..'Z' | '0'..'9' as c) -> c | _ -> '_') name in
    match name.[0] with
    | '0'..'9' | '_' -> prefix ^ name
    | _ -> String.uncapitalize_ascii name

let idents ~prefix l =
  let rec choose acc base n =
    let name = sprintf "%s%d" base n in
    if List.mem name acc then choose acc base (n+1) else name
  in
  let rec loop acc = function
  | [] -> List.rev acc
  | x::xs ->
    let x = ident ~prefix x in
    let x = if List.mem x acc then choose acc x 0 else x in
    loop (x::acc) xs
  in
  loop [] l

end

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
  sprintf "(T.get_column_%s%s stmt %u)"
    (L.as_lang_type attr.domain)
    (if is_attr_nullable attr then "_nullable" else "")
    index

module T = Translate(L)

(* open L *)
open T

let output_schema_binder _ schema =
  let name = "invoke_callback" in
  output "let %s stmt =" name;
  let args = Name.idents ~prefix:"r" (List.map (fun a -> a.name) schema) in
  let values = List.mapi get_column schema in
  indented (fun () ->
    output "callback";
    indented (fun () -> List.iter2 (output "~%s:%s") args values));
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
         | Stmt.Select `Zero_one -> "select_one_maybe", output_select1_cb index schema
         | Select `One -> "select_one", output_select1_cb index schema
         | _ -> "select",output_schema_binder index schema

let is_callback stmt =
  match stmt.schema, stmt.kind with
  | [],_ -> false
  | _, Stmt.Select (`Zero_one | `One) -> false
  | _ -> true

let list_separate f l =
  let a = ref [] in
  let b = ref [] in
  List.iter (fun x -> match f x with `Left x -> tuck a x | `Right x -> tuck b x) l;
  List.rev !a, List.rev !b

let make_variant_name i name =
  "`" ^ match name with
  | None -> sprintf "V_%d" i
  | Some n -> String.capitalize_ascii n

let vname n = make_variant_name 0 (Some n)

let match_variant_wildcard i name args =
  sprintf "%s%s" (make_variant_name i name) (match args with Some [] | None -> "" | Some _ -> " _")

let set_param index param =
  let nullable = is_param_nullable param in
  let pname = show_param_name param index in
  let ptype = show_param_type param in
  if nullable then
    output "begin match %s with None -> T.set_param_null p | Some v -> T.set_param_%s p v end;" pname ptype
  else
    output "T.set_param_%s p %s;" ptype pname

let rec set_var index var =
  match var with
  | Single p -> set_param index p
  | SingleIn _ -> ()
  | Choice (name,ctors) ->
    output "begin match %s with " (make_param_name index name);
    ctors |> List.iteri begin fun i ctor ->
      match ctor with
      | Simple (param,args) ->
        output "| %s%s -> %s"
          (make_variant_name i param.label)
          (match args with Some [] | None -> "" | Some l -> " ("^String.concat "," (names_of_vars l)^")")
          (match args with Some [] | None -> "()" | Some _ -> "");
        inc_indent ();
        List.iter (set_var index) (Option.default [] args);
        dec_indent ()
      | Verbatim (n,_) -> output "| %s -> ()" (vname n)
    end;
    output "end;"

let rec eval_count_params vars =
  let (static,choices) = list_separate (function Single _ -> `Left true | SingleIn _ -> `Left false | Choice (name,c) -> `Right (name, c)) vars in
  string_of_int (List.length @@ List.filter (fun x -> x) static) ^
  match choices with
  | [] -> ""
  | _ ->
  choices |> List.mapi begin fun i (name,ctors) ->
    sprintf " + (match %s with " (make_param_name i name) ^
    (ctors |> List.mapi (fun i ctor ->
      match ctor with
      | Verbatim (n,_) -> sprintf "%s -> 0" (vname n)
      | Simple (param,args) -> sprintf "%s -> %s" (match_variant_wildcard i param.label args) (eval_count_params @@ Option.default [] args)) |> String.concat " | ")
    ^ ")"
  end |> String.concat ""

let output_params_binder _ vars =
  output "let set_params stmt =";
  inc_indent ();
  output "let p = T.start_params stmt (%s) in" (eval_count_params vars);
  let (_ : int) =
    List.fold_left
      (fun index v ->
         match v with
         | Single _ | Choice _ -> set_var index v; index + 1
         | SingleIn _ -> index)
    0 vars
  in
  output "T.finish_params p";
  dec_indent ();
  output "in";
  "set_params"

let output_params_binder index vars =
  match vars with
  | [] -> "T.no_params"
  | _ -> output_params_binder index vars

let prepend prefix = function s -> prefix ^ s

let make_sql l =
  let b = Buffer.create 100 in
  let rec loop app = function
    | [] -> ()
    | Static "" :: tl when app -> loop app tl
    | Static s :: tl ->
      if app then bprintf b " ^ ";
      Buffer.add_string b (quote s);
      loop true tl
    | Dynamic (name, ctors) :: tl ->
      if app then bprintf b " ^ ";
      bprintf b "(match %s with" (make_param_name 0 name);
      ctors |> List.iteri (fun i (name,args,l) -> bprintf b " %s%s -> " (if i = 0 then "" else "| ") (match_variant_wildcard i name.label args); loop false l);
      bprintf b ")";
      loop true tl
  in
  Buffer.add_string b "(";
  loop false l;
  Buffer.add_string b ")";
  Buffer.contents b

let in_var_module label = function
  | Sql.Type.Tuple _ | Any | Unit _ as typ -> failwith @@ sprintf "invalid IN var type %s in @%s" (Sql.Type.show typ) label
  | typ -> Sql.Type.to_string typ

let gen_in_substitution var =
  if Option.is_none var.id.label then failwith "empty label in IN param";
  match var.typ with
  | Sql.Type.Unit _ | Int | Text | Blob | Float | Bool | Datetime | Decimal | Any | Tuple (Any | Tuple _) as typ ->
    failwith @@ sprintf "invalid inferred IN var type %s (%s)" (Sql.Type.show typ) (Option.get var.id.label)
  | Tuple typ ->
    sprintf {code| "(" ^ String.concat ", " (List.map T.Types.%s.to_literal %s) ^ ")"|code}
      (in_var_module (Option.get var.id.label) typ)
      (Option.get var.id.label)

let generate_stmt style index stmt =
  let name = choose_name stmt.props stmt.kind index |> String.uncapitalize in
  let subst = Props.get_all stmt.props "subst" in
  let inputs = (subst @ names_of_vars stmt.vars) |> List.map (prepend "~") |> inline_values in
  let insubst =
    List.filter_map
      (function
        | Single _ | Choice _ -> None
        | SingleIn p -> Some (`InVar p))
      stmt.vars
  in
  let subst = List.map (fun x -> `Var x) subst @ insubst in
  match style, is_callback stmt with
  | (`List | `Fold), false -> ()
  | _ ->
  let all_inputs = inputs ^ (if style = `List || is_callback stmt then " callback" else "") ^ (if style = `Fold then " acc" else "") in
  output "let %s db %s =" name all_inputs;
  inc_indent ();
  let sql = make_sql @@ get_sql stmt in
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
    List.iter begin function
      | `Var var ->
        output "  let sql = replace_all ~str:sql ~sub:(\"%%%%%s%%%%\") ~by:%s in" var var;
      | `InVar var ->
        output "  let sql = replace_all ~str:sql ~sub:(\"@@_sqlgg_%s@@\") ~by:(%s) in"
          (match var.id.label with None -> failwith "IN var with no label" | Some label -> label)
          (gen_in_substitution var);
    end vars;
    output "  sql";
    output "in";
    "__sqlgg_sql"
  in
  let (func,callback) = output_schema_binder index stmt.schema stmt.kind in
  let params_binder_name = output_params_binder index stmt.vars in
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
