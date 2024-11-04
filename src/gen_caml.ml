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
  open Type

  let as_lang_type = function
  | { t = Blob; nullability } -> type_name { t = Text; nullability }
  | t -> type_name t

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

let make_variant_name' i name = 
  match name with
  | None -> sprintf "V_%d" i
  | Some n -> String.capitalize_ascii n

let make_variant_name i name kind = (match kind with `Poly ->  "`" | `Classic -> "") ^ make_variant_name' i name

let vname n = make_variant_name 0 (Some n)

let match_variant_wildcard i name args kind =
  sprintf "%s%s" (make_variant_name i name kind) (match args with Some [] | None -> "" | Some _ -> " _")

let match_arg_pattern = function
  | Sql.Single _ | SingleIn _ | Choice _
  | BoolChoice _
  | ChoiceIn { param = { label = None; _ }; _ }
  | TupleList _ -> "_"
  | ChoiceIn { param = { label = Some s; _ }; _ } -> s

let match_variant_pattern i name args kind =
  sprintf "%s%s"
    (make_variant_name i name kind)
    (match args with
     | Some [] | None -> ""
     | Some l ->
     match List.map match_arg_pattern l with
     | l when List.for_all ((=) "_") l -> " _"
     | l -> sprintf " (%s)" (String.concat ", " l))

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
  | TupleList _ -> ()
  | ChoiceIn { param = name; vars; _ } ->
    output "begin match %s with" (make_param_name index name);
    output "| [] -> ()";
    output "| _ :: _ ->";
    inc_indent ();
    List.iter (set_var index) vars;
    output "()";
    dec_indent ();
    output "end;"
  | BoolChoice(_, name, vars) -> 
    output "begin match %s with" (make_param_name index name);
    [(Some "None", []); (Some "Some", vars)] |> List.iteri begin fun i (label, vars) ->
      output "| %s%s -> %s"
      (make_variant_name' i label)
      (match vars with [] -> "" | l -> " ("^String.concat "," (names_of_vars l)^")")
      (match vars with [] -> "()" | _ -> "");
      inc_indent ();
      List.iter (set_var index) vars;
      dec_indent ()
    end;
    output "end;"
  | Choice (name,ctors) ->
    output "begin match %s with" (make_param_name index name);
    ctors |> List.iteri begin fun i ctor ->
      match ctor with
      | Simple (param,args) ->
        output "| %s%s -> %s"
          (make_variant_name i param.label `Poly)
          (match args with Some [] | None -> "" | Some l -> " ("^String.concat "," (names_of_vars l)^")")
          (match args with Some [] | None -> "()" | Some _ -> "");
        inc_indent ();
        List.iter (set_var index) (Option.default [] args);
        dec_indent ()
      | Verbatim (n,_) -> output "| %s -> ()" (vname n `Poly)
    end;
    output "end;"

let rec eval_count_params vars =
  let (static, all_choices) =
    list_separate
      (function
        | Single _ -> `Left true
        | BoolChoice _ -> `Left true
        | SingleIn _ -> `Left false
        | TupleList _ -> `Left true
        | ChoiceIn { param; vars; _ } -> `Right (`ChoiceIn (param, vars))
        | Choice (name,c) -> `Right (`Choice (name, c)))
      vars
  in
  let choices, choices_in =
    list_separate
      (function
        | `Choice (name, c) -> `Left (name, c)
        | `ChoiceIn t -> `Right t)
      all_choices
  in
  let static = string_of_int (List.length @@ List.filter (fun x -> x) static) in
  let choices_in =
    match choices_in with
    | [] -> ""
    | choices_in ->
      choices_in |>
      List.mapi
        (fun i (param, vars) ->
           sprintf
             " + (match %s with [] -> 0 | _ :: _ -> %s)"
             (make_param_name i param)
             (eval_count_params vars)) |>
      String.concat ""
  in
  let choices =
    match choices with
    | [] -> ""
    | _ ->
    choices |> List.mapi begin fun i (name,ctors) ->
      sprintf " + (match %s with " (make_param_name i name) ^
      (ctors |> List.mapi (fun i ctor ->
        match ctor with
        | Verbatim (n,_) -> 
          sprintf "%s -> 0" (vname n `Poly)
        | Simple (param,args) -> sprintf "%s -> %s" (match_variant_pattern i param.label args `Poly) (eval_count_params @@ Option.default [] args)) |> String.concat " | ")
      ^ ")"
    end |> String.concat ""
  in
  static ^ choices_in ^ choices

let output_params_binder _ vars =
  output "let set_params stmt =";
  inc_indent ();
  output "let p = T.start_params stmt (%s) in" (eval_count_params vars);
  List.iteri set_var vars;
  output "T.finish_params p";
  dec_indent ();
  output "in";
  "set_params"

let rec exclude_in_vars l =
  List.filter_map
    (function
      | SingleIn _ -> None
      | Single _ as v -> Some v
      | BoolChoice (f, p, v) -> Some (BoolChoice (f, p, exclude_in_vars v))
      | TupleList _ -> None
      | ChoiceIn t -> Some (ChoiceIn { t with vars = exclude_in_vars t.vars })
      | Choice (param_id, ctors) ->
        Some (Choice (param_id, List.map exclude_in_vars_in_constructors ctors)))
    l

and exclude_in_vars_in_constructors = function
  | Verbatim _ as ctor -> ctor
  | Simple (param_id, vars) -> Simple (param_id, Option.map exclude_in_vars vars)

let output_params_binder index vars =
  match exclude_in_vars vars with
  | [] -> "T.no_params"
  | vars -> output_params_binder index vars

let in_var_module _label typ = Sql.Type.type_name typ

let gen_in_substitution var =
  if Option.is_none var.id.label then failwith "empty label in IN param";
  sprintf {code| "(" ^ String.concat ", " (List.map T.Types.%s.to_literal %s) ^ ")"|code}
    (in_var_module (Option.get var.id.label) var.typ)
    (Option.get var.id.label)

let gen_tuple_printer _label schema =
  let params = List.map (fun { name; _ } -> name) schema in
  sprintf
    {|(fun _sqlgg_idx (%s) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 then "(" else ", ("); %s Buffer.add_char _sqlgg_b ')')|}
    (String.concat ", " params)
    (String.concat " " @@
     List.mapi
     (fun idx attr ->
        let { name; domain; _ } = attr in
        (if idx = 0 then "" else {|Buffer.add_string _sqlgg_b ", "; |}) ^
        sprintf {|Buffer.add_string _sqlgg_b (%s);|}
          (let to_literal = sprintf "T.Types.%s.to_literal %s" (in_var_module name domain) in
           if is_attr_nullable attr then 
           (sprintf {|match %s with None -> "NULL" | Some v -> %s|} name (to_literal "v") )
           else to_literal name))
     schema)

let resolve_tuple_label id = match id.label with
| None -> failwith "empty label in tuple param"
| Some label -> label

let gen_tuple_substitution label schema =
  sprintf
    {|(let _sqlgg_b = Buffer.create 13 in List.iteri %s %s; Buffer.contents _sqlgg_b)|}
    (gen_tuple_printer label schema)
    label 

let make_schema_of_tuple_types label =
  List.mapi (fun idx domain -> {
    name=(sprintf "%s_%Ln" label idx); domain; extra = Constraints.empty
  })   

let make_sql l =
  let b = Buffer.create 100 in
  let rec loop app = function
    | [] -> ()
    | Static "" :: tl when app -> loop app tl
    | Static s :: tl ->
      if app then bprintf b " ^ ";
      Buffer.add_string b (quote s);
      loop true tl
    | SubstIn param :: tl ->
      if app then bprintf b " ^ ";
      Buffer.add_string b (gen_in_substitution param);
      loop true tl
    | DynamicIn (name, in_or_not_in, sqls) :: tl ->
      if app then bprintf b " ^ ";
      bprintf b "(match %s with" (make_param_name 0 name);
      bprintf b " [] -> \"%s\" | _ :: _ -> "
        (String.uppercase_ascii @@ string_of_bool @@ match in_or_not_in with `In -> false | `NotIn -> true);
      loop false sqls;
      bprintf b ")";
      loop true tl
    | Dynamic (name, ctors) :: tl ->
      if app then bprintf b " ^ ";
      bprintf b "(match %s with" (make_param_name 0 name);
      ctors |> List.iteri (fun i (name,args,l, kind) -> bprintf b " %s%s -> " (if i = 0 then "" else "| ") (match_variant_pattern i name.label args kind); loop false l);
      bprintf b ")";
      loop true tl
    | SubstTuple (id, Insertion schema) :: tl ->
      if app then bprintf b " ^ ";
      let label = resolve_tuple_label id in
      Buffer.add_string b (gen_tuple_substitution label schema);
      loop true tl
    | SubstTuple (id, Where_in types) :: tl ->
      if app then bprintf b " ^ ";
      let label = resolve_tuple_label id in
      let schema = make_schema_of_tuple_types label types in
      bprintf b "%s ^ " (quote "(");
      Buffer.add_string b (gen_tuple_substitution label schema);
      bprintf b " ^ %s" (quote ")");
      loop true tl
  in
  Buffer.add_string b "(";
  loop false l;
  Buffer.add_string b ")";
  Buffer.contents b

let generate_stmt style index stmt =
  let name = choose_name stmt.props stmt.kind index |> String.uncapitalize_ascii in
  let subst = Props.get_all stmt.props "subst" in
  let inputs = (subst @ names_of_vars stmt.vars) |> List.map (fun v -> sprintf "~%s" v) |> inline_values in
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
    List.iter (fun var -> output "  let sql = replace_all ~str:sql ~sub:(\"%%%%%s%%%%\") ~by:%s in" var var) vars;
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
  let exec = sprintf "T.%s db %s %s %s" func sql params_binder_name callback in
  let exec =
    match
      List.find_map
        (function
          | SubstTuple (id, Insertion _) -> Some id
          | SubstTuple (_, Where_in _)
          | Static _ | Dynamic _ | DynamicIn _ | SubstIn _ -> None)
        (get_sql stmt)
    with
    | None -> exec
    | Some id ->
    match id.label with
    | None -> failwith "empty label in tuple substitution"
    | Some label -> sprintf {|( match %s with [] -> IO.return { T.affected_rows = 0L; insert_id = 0L } | _ :: _ -> %s)|} label exec
  in
  output "%s%s" bind exec;
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
  output "module %s (T : %s) = struct" (String.capitalize_ascii name) traits;
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
  output "end (* module %s *)" (String.capitalize_ascii name)

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
