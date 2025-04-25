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
let comment () fmt = Printf.ksprintf (indent_endline $ make_comment) fmt

let empty_line () = print_newline ()

let enums_hash_tbl = Hashtbl.create 100

let enum_get_hash ctors = Type.Enum_kind.Ctors.elements ctors |> String.concat "_"

let enum_name = Printf.sprintf "Enum_%d"

let get_enum_name ctors = ctors |> enum_get_hash |> Hashtbl.find enums_hash_tbl |> fst |> enum_name

module L = struct
  open Type

  let as_lang_type = function
  | { t = Blob; nullability } -> type_name { t = Text; nullability }
  | { t = StringLiteral _; nullability } -> type_name { t = Text; nullability }
  | { t = Unit _; _ }
  | { t = Int; _ }
  | { t = Text; _ }
  | { t = Float; _ }
  | { t = Bool; _ }
  | { t = Datetime; _ }
  | { t = Decimal; _ }
  | { t = Union _; _ }
  | { t = Any; _ } as t -> type_name t

  let as_api_type = as_lang_type
end

let get_column index attr =
  let rec print_column attr = match attr with
  | { domain={ t = Union {ctors; _}; _ }; _ } when !Sqlgg_config.enum_as_poly_variant ->
    sprintf "(%s.get_column%s" (get_enum_name ctors)
  | { domain={ t = Union _; _ }; _ } as c -> print_column { c with domain = { c.domain with t = Text } }
  | _ -> sprintf "(T.get_column_%s%s" (L.as_lang_type attr.domain) in 
  let column = print_column attr (if is_attr_nullable attr then "_nullable" else "") in 
  sprintf "%s stmt %u)" column index

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


let make_variant_name i name ~is_poly =
  let prefix = if is_poly then "`" else "" in 
  prefix ^ match name with
  | None -> sprintf "V_%d" i
  | Some n -> String.capitalize_ascii n

let vname n = make_variant_name 0 (Some n)
  
let match_variant_pattern i name args ~is_poly =
  let variant_name = make_variant_name i name ~is_poly in
  match args with
  | None | Some [] -> variant_name
  | Some arg_list ->
    let (_, _, all_wildcard), patterns =
      List.fold_left_map (fun (seen_wildcards, seen_names, all_wc) arg ->
        let make_wildcard_param label = 
          if List.mem label seen_wildcards
            then ((seen_wildcards, seen_names, all_wc), None)
            else ((label :: seen_wildcards, seen_names, all_wc), Some "_") in
        match arg with
        | Sql.Single param | SingleIn param -> make_wildcard_param param.id.label
        | TupleList (param_id, _) -> make_wildcard_param param_id.label
        | SharedVarsGroup _
        | Choice ({ label = None; _ }, _)
        | OptionBoolChoice ({ label = None; _ }, _, _)
        | ChoiceIn { param = { label = None; _ }; _ } ->
            ((seen_wildcards, seen_names, all_wc), Some "_")
        | Choice ({ label = Some s; _ }, _)
        | OptionBoolChoice ({ label = Some s; _ }, _, _)
        | ChoiceIn { param = { label = Some s; _ }; _ } ->
            if List.mem s seen_names
            then ((seen_wildcards, seen_names, false), None)
            else ((seen_wildcards, s :: seen_names, false), Some s)
      ) ([], [], true) arg_list
    in
    let patterns = List.filter_map identity patterns in
    if patterns = [] then
      variant_name
    else if all_wildcard then
      variant_name ^ " _"
    else
      variant_name ^ " (" ^ String.concat ", " patterns ^ ")"

let rec set_param index param =
  let nullable = is_param_nullable param in
  let pname = show_param_name param index in
  let ptype = show_param_type param in
  let set_param_nullable = output "begin match %s with None -> T.set_param_null p | Some v -> %s p v end;" pname in
  match param with
  | { typ = { t=Union _; _}; _ } as c when not !Sqlgg_config.enum_as_poly_variant -> set_param index { c with typ = { c.typ with t = Text } }
  | { typ = { t=Union {ctors; _}; _}; _ } when nullable -> set_param_nullable @@ (get_enum_name ctors) ^ ".set_param" 
  | { typ = { t=Union {ctors; _}; _ }; _ } -> output "%s.set_param p %s;" (get_enum_name ctors) pname
  | param' when nullable -> set_param_nullable @@ sprintf "T.set_param_%s" (show_param_type param') 
  | _ -> output "T.set_param_%s p %s;" ptype pname
  
let rec set_var index var =
  match var with
  | Single p -> set_param index p
  | SharedVarsGroup (vars, _) -> List.iter (set_var index) vars
  | SingleIn _ | TupleList _ -> ()
  | ChoiceIn { param = name; vars; _ } ->
    output "begin match %s with" (make_param_name index name);
    output "| [] -> ()";
    output "| _ :: _ ->";
    inc_indent ();
    List.iter (set_var index) vars;
    output "()";
    dec_indent ();
    output "end;"
  | OptionBoolChoice(name, vars, _) -> 
    output "begin match %s with" (make_param_name index name);
    [(Some "None", []); (Some "Some", vars)] |> List.iteri begin fun i (label, vars) ->
      output "| %s%s -> %s"
      (make_variant_name i label ~is_poly:false)
      (match vars with [] -> "" | l -> " (" ^String.concat "," (names_of_vars l) ^ ")")
      (match vars with [] -> "()" | _ -> "");
      inc_indent ();
      List.iter (set_var index) vars;
      dec_indent ()
    end;
    output "end;"
  | Choice (name, ctors) ->
    output "begin match %s with" (make_param_name index name);
    ctors |> List.iteri begin fun i ctor ->
      match ctor with
      | Simple (param,args) ->
        output "| %s%s -> %s"
          (make_variant_name i param.label ~is_poly:true)
          (match args with Some [] | None -> "" | Some l -> " ("^String.concat "," (names_of_vars l)^")")
          (match args with Some [] | None -> "()" | Some _ -> "");
        inc_indent ();
        List.iter (set_var index) (Option.default [] args);
        dec_indent ()
      | Verbatim (n,_) -> output "| %s -> ()" (vname n ~is_poly:true)
    end;
    output "end;" 

let rec eval_count_params vars =
  let (static, choices, bool_choices, choices_in) =
    let classify_var = function
      | Single _ | TupleList _ -> `Static true
      | SingleIn _ -> `Static false
      | SharedVarsGroup (vars, _) -> `SharedVarsGroup vars
      | OptionBoolChoice (param_id, vars, _) -> `BoolChoice (param_id, vars)
      | ChoiceIn { param; vars; _ } -> `ChoiceIn (param, vars)
      | Choice (name, c) -> `Choice (name, c)
    in
    let rec group_vars (static, choices, bool_choices, choices_in) = function
      | [] -> (List.rev static, List.rev choices, List.rev bool_choices, List.rev choices_in)
      | x::xs ->
        match classify_var x with
        | `Static v -> group_vars (v::static, choices, bool_choices, choices_in) xs
        | `BoolChoice v -> group_vars (static, choices, v::bool_choices, choices_in) xs
        | `ChoiceIn v -> group_vars (static, choices, bool_choices, v::choices_in) xs
        | `Choice v -> group_vars (static, v::choices, bool_choices, choices_in) xs
        | `SharedVarsGroup vars -> 
          let static', choices', bool_choices', choices_in' = group_vars ([], [], [], []) vars in
          group_vars (static' @ static, choices' @ choices, bool_choices' @ bool_choices, choices_in' @ choices_in) xs
    in
    group_vars ([], [], [], []) vars
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
  let bool_choices = match bool_choices with
  | [] -> ""
  | _ -> bool_choices |> List.mapi begin fun i ((param_id, vars)) ->
    sprintf " + (match %s with %s -> %s | %s -> 0)"
      (make_param_name i param_id)
      (match_variant_pattern i (Some "Some") (Some vars) ~is_poly:false)
      (eval_count_params vars)
      (match_variant_pattern i (Some "None") None ~is_poly:false)
  end |> String.concat "" in
  let choices =
    match choices with
    | [] -> ""
    | _ ->
    choices |> List.mapi begin fun i (name,ctors) ->
      sprintf " + (match %s with " (make_param_name i name) ^
      (ctors |> List.mapi (fun i ctor ->
        match ctor with
        | Verbatim (n,_) -> 
          sprintf "%s -> 0" (vname n ~is_poly:true)
        | Simple (param,args) -> 
          sprintf "%s -> %s" (match_variant_pattern i param.label args ~is_poly:true) 
            (eval_count_params @@ Option.default [] args)) |> String.concat " | ")
      ^ ")"
    end |> String.concat ""
  in
  static ^ choices_in ^ choices ^ bool_choices

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
      | SharedVarsGroup (vars, p) -> Some (SharedVarsGroup (exclude_in_vars vars, p))
      | OptionBoolChoice (p, v, pos) -> Some (OptionBoolChoice (p, exclude_in_vars v, pos))
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


let make_to_literal =
  let rec go domain = match domain with 
    | { Type.t = Union _; _ } when not !Sqlgg_config.enum_as_poly_variant -> go { domain with Type.t = Text }
    | { Type.t = Union { ctors; _ }; _ } -> sprintf "%s.to_literal" (get_enum_name ctors)
    | t -> sprintf "T.Types.%s.to_literal" (Sql.Type.type_name t) in go

let gen_in_substitution var =
  if Option.is_none var.id.label then failwith "empty label in IN param";
  sprintf {code| "(" ^ String.concat ", " (List.map %s %s) ^ ")"|code}
    (make_to_literal var.typ)
    (Option.get var.id.label)

let gen_tuple_printer ~is_row _label schema =
  let params = List.map (fun { name; _ } -> name) schema in
  let open_paren = if is_row then {|then "ROW(" else ", ROW("|} else {|then "(" else ", ("|} in
  sprintf
    {|(fun _sqlgg_idx (%s) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 %s); %s Buffer.add_char _sqlgg_b ')')|}
    (String.concat ", " params)
    open_paren
    (String.concat " " @@
     List.mapi
     (fun idx attr ->
        let { name; domain; _ } = attr in
        (if idx = 0 then "" else {|Buffer.add_string _sqlgg_b ", "; |}) ^
        sprintf {|Buffer.add_string _sqlgg_b (%s);|}
          (let to_literal = sprintf "%s %s" (make_to_literal domain) in
           if is_attr_nullable attr then 
           (sprintf {|match %s with None -> "NULL" | Some v -> %s|} name (to_literal "v") )
           else to_literal name))
     schema)

let resolve_tuple_label id = match id.label with
| None -> failwith "empty label in tuple param"
| Some label -> label

let gen_tuple_substitution ~is_row label schema =
  sprintf
    {|(let _sqlgg_b = Buffer.create 13 in List.iteri %s %s; Buffer.contents _sqlgg_b)|}
    (gen_tuple_printer ~is_row label schema)
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
      ctors |> List.iteri (fun i ({ ctor; args; sql; is_poly }) -> 
        bprintf b " %s%s -> " (if i = 0 then "" else "| ") 
          (match_variant_pattern i ctor.label args ~is_poly:is_poly); loop false sql);
      bprintf b ")";
      loop true tl
    | SubstTuple (id, Insertion schema) :: tl ->
      if app then bprintf b " ^ ";
      let label = resolve_tuple_label id in
      Buffer.add_string b (gen_tuple_substitution ~is_row:false label schema);
      loop true tl
    | SubstTuple (id, Where_in types) :: tl ->
      if app then bprintf b " ^ ";
      let label = resolve_tuple_label id in
      let schema = make_schema_of_tuple_types label types in
      bprintf b "%s ^ " (quote "(");
      Buffer.add_string b (gen_tuple_substitution ~is_row:false label schema);
      bprintf b " ^ %s" (quote ")");
      loop true tl
    | SubstTuple (id, ValueRows { types; _ }) :: tl ->
        if app then bprintf b " ^ ";
        let label = resolve_tuple_label id in
        let schema = make_schema_of_tuple_types label types in
        let empty = schema 
          |> List.map (const "NULL") 
          |> String.join ", " 
          |> sprintf {|"SELECT %s WHERE FALSE"|} in
        let not_empty = gen_tuple_substitution ~is_row:true label schema in
        Buffer.add_string b @@ sprintf {|( if %s = [] then %s else ( "VALUES " ^ %s ) )|} label empty not_empty;
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
          | SubstTuple (_, ( Where_in _| ValueRows _ ))
          | Static _ | Dynamic _ | DynamicIn _ | SubstIn _ -> None)
        (get_sql stmt)
    with
    | None -> exec
    | Some id ->
    match id.label with
    | None -> failwith "empty label in tuple substitution"
    | Some label -> sprintf {|( match %s with [] -> IO.return { T.affected_rows = 0L; insert_id = None } | _ :: _ -> %s)|} label exec
  in
  output "%s%s" bind exec;
  if style = `Fold then output "(fun () -> IO.return !r_acc)";
  if style = `List then output "(fun () -> IO.return (List.rev !r_acc))";
  dec_indent ();
  empty_line ()

let sanitize_to_variant_name s =
  let normalized =
    let open String in
    s 
    |> lowercase_ascii
    |> map (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c -> c | _ -> '_')
    |> capitalize_ascii
  in match normalized.[0] with
    | '0'..'9' -> "Num_" ^ normalized
    | _ -> normalized

let generate_enum_modules stmts = 
  let open Sql.Type.Enum_kind in

  let schemas = List.concat_map (fun stmt -> stmt.Gen.schema) stmts in
  let vars = List.concat_map (fun stmt -> stmt.Gen.vars) stmts in

  let get_enum typ = match typ.Sql.Type.t with 
    | Union { ctors; _ } -> Some ctors
    | Unit _ | Int | Text | Blob | Float | Bool  | Datetime | Decimal | Any | StringLiteral  _ -> None
  in

  let schemas_to_enums schemas = schemas |> List.filter_map (fun { domain; _ } -> get_enum domain) in

  let rec vars_to_enums vars = List.concat_map (function
    | Single { typ; _ }
    | SingleIn { typ; _ } -> typ |> get_enum |> option_list
    | SharedVarsGroup (vars, _)
    | OptionBoolChoice (_, vars, _)
    | ChoiceIn { vars; _ } -> vars_to_enums vars
    | Choice (_, ctor_list) -> 
      List.concat_map ( function
        | Simple (_, vars) -> Option.map vars_to_enums vars |> option_list |> List.concat
        | Verbatim _ -> []
      ) ctor_list
    | TupleList (_, ( Where_in types | ValueRows { types; _ } )) -> 
      List.concat_map (fun typ -> typ |> get_enum |> option_list) types
    | TupleList (_, Insertion schema) -> schemas_to_enums schema
  ) vars in

  Hashtbl.reset enums_hash_tbl;
  
  let generate_enum_module enum_count enum = 
    let get_ctor_name x = x |> sanitize_to_variant_name |> vname ~is_poly:true in
    let ctor_list = Ctors.elements enum in
    output {|
    module %s = T.Make_enum(struct
      type t = [%s]
      let inj = function %s | s -> failwith (Printf.sprintf "Invalid enum value: %%s" s)
      let proj = function  %s
    end)
    |}
    (enum_name enum_count)
    (ctor_list |> List.map get_ctor_name |> String.concat " | ")
    (String.concat " "
    (List.map (fun ctor -> Printf.sprintf "| \"%s\" -> %s" (String.escaped ctor) (get_ctor_name ctor)) ctor_list))
    (String.concat "" 
    (List.map (fun ctor -> Printf.sprintf "| %s -> \"%s\"" (get_ctor_name ctor) (String.escaped ctor)) ctor_list))
  in

  indented (fun () -> 
    let result = schemas_to_enums schemas @ vars_to_enums vars in
    let (_: int * unit list) = List.fold_left_map begin fun acc enum -> 
      let hash = enum_get_hash enum in
      if Hashtbl.mem enums_hash_tbl hash then acc, ()
      else begin
        Hashtbl.add enums_hash_tbl hash (acc, enum);
        acc + 1, begin empty_line (); generate_enum_module acc enum end
      end
    end 0 result in 
    ()
  )

let generate_enum_modules stmts = if !Sqlgg_config.enum_as_poly_variant then generate_enum_modules stmts
  
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
  generate_enum_modules stmts;
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
