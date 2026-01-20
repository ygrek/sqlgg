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

(* DynamicSelect support *)

let field_name_of_param_id (p : Sql.param_id) =
  match p.label with Some s -> String.capitalize_ascii s | None -> "Field"

(* Generate pattern for match case on ctor *)
let ctor_pattern = function
  | Sql.Simple (param_id, args) ->
    let field_name = field_name_of_param_id param_id in
    (match args with Some (_ :: _) -> field_name ^ " _" | _ -> field_name)
  | Sql.Verbatim (n, _) -> String.capitalize_ascii n

type dynamic_info = {
  param_id: Sql.param_id;
  module_name: string;
  param_name: string;
  ctors: Sql.ctor list;
  schema_fields: (Sql.param_id * Sql.attr) list;
}

module L = struct
  open Type

  let as_lang_type = function
  | { t = Blob; nullability } -> type_name { t = Text; nullability }
  | { t = StringLiteral _; nullability } -> type_name { t = Text; nullability }
  | { t = FloatingLiteral _; nullability } -> type_name { t = Float; nullability }
  | { t = Decimal _; nullability; } -> type_name { t = Decimal { precision = None; scale = None }; nullability } 
  | { t = Int; _ }
  | { t = Text; _ }
  | { t = Float; _ }
  | { t = Bool; _ }
  | { t = Datetime; _ }
  | { t = Union _; _ }
  | { t = Json; _ }
  | { t = Json_path; _ }
  | { t = One_or_all; _ }
  | { t = UInt64; _ }
  | { t = Any; _ } as t -> type_name t

  let as_runtime_repr_name = function
  | { t = Blob; _ }
  | { t = Text; _ }
  | { t = Any; _ }
  | { t = Union _; _ }
  | { t = Json_path; _ }
  | { t = StringLiteral _; _ } -> "string"
  | { t = Int; _ } -> "int64"
  | { t = Float; _ }
  | { t = FloatingLiteral _; _ } -> "float"
  | { t = Bool; _ } -> "bool"
  | { t = Datetime; _ }
  | { t = Decimal _; _ } -> "float"
  | { t = Json; _ } -> "json"
  | { t = UInt64; _ } -> "uint64"
  | { t = One_or_all; _ } -> "text"


  let as_api_type = as_lang_type
end

let nullable_suffix attr = if is_attr_nullable attr then "_nullable" else ""

(* Format T.get_column call with given row variable and index expression *)
let format_get_column ~row ~idx attr =
  let null_suffix = nullable_suffix attr in
  let format_t_get_column type_name =
    sprintf "T.get_column_%s%s %s %s" type_name null_suffix row idx
  in
  match Sql.Meta.find_opt attr.meta "module" with
  | Some m ->
      let runtime_repr_name = L.as_runtime_repr_name attr.domain in
      let inner_get_column_expr = sprintf "(T.get_column_%s%s %s %s)" runtime_repr_name null_suffix row idx in
      let get_column = "get_column" in
      let get_column_name = get_column |> Sql.Meta.find_opt attr.meta |> Option.default get_column in
      sprintf "%s.%s%s %s" m get_column_name null_suffix inner_get_column_expr
  | None ->
      begin match attr.domain with
      | { t = Union { ctors; _ }; _ } ->
          sprintf "%s.get_column%s %s %s" (get_enum_name ctors) null_suffix row idx
      | _ ->
          format_t_get_column (L.as_lang_type attr.domain)
      end

let get_column index attr =
  sprintf "(%s)" (format_get_column ~row:"stmt" ~idx:(string_of_int index) attr)

module T = Translate(L)

(* open L *)
open T

let schema_to_attrs schema =
  List.filter_map (function
    | Syntax.Attr attr -> Some attr
    | Syntax.Dynamic _ -> None
  ) schema

let output_schema_binder_labeled _ schema =
  let attrs = schema_to_attrs schema in
  let name = "invoke_callback" in
  output "let %s stmt =" name;
  let args = Name.idents ~prefix:"r" (List.map (fun a -> a.Sql.name) attrs) in
  let values = List.mapi get_column attrs in
  indented (fun () ->
    output "callback";
    indented (fun () -> List.iter2 (output "~%s:%s") args values));
  output "in";
  name

let output_select1_cb _ schema =
  let attrs = schema_to_attrs schema in
  let name = "get_row" in
  output "let %s stmt =" name;
  indented (fun () ->
    List.mapi get_column attrs |> String.concat ", " |> indent_endline);
  output "in";
  name

let select_func_of_kind = function
| Stmt.Select `Zero_one -> "select_one_maybe"
| Stmt.Select `One -> "select_one"
| _ -> "select"

let output_schema_binder index schema kind =
  match schema with
  | [] -> "execute",""
  | _ ->
    let func = select_func_of_kind kind in
    match kind with
    | Stmt.Select (`Zero_one | `One) -> func, output_select1_cb index schema
    | _ -> func, output_schema_binder_labeled index schema

let is_callback stmt =
  match stmt.schema, stmt.kind with
  | [],_ -> false
  | _, Stmt.Select (`Zero_one | `One) -> false
  | _ -> true

let should_generate_for_style style stmt =
  match style with
  | `List | `Fold -> is_callback stmt
  | `Single -> (match stmt.kind, stmt.schema with | Stmt.Select (`One | `Zero_one), _ :: _ -> true | _ -> false)
  | `Direct -> true

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
        | Sql.Single (param, _) | SingleIn (param, _) -> make_wildcard_param param.id.label
        | SharedVarsGroup _
        | Choice ({ label = None; _ }, _)
        | TupleList ({ label = None; _ }, _) 
        | OptionActionChoice ({ label = None; _ }, _, _, _)
        | ChoiceIn { param = { label = None; _ }; _ } ->
          ((seen_wildcards, seen_names, all_wc), Some "_")
        | TupleList ({ label = Some s; _ }, _) 
        | Choice ({ label = Some s; _ }, _)
        | DynamicSelect ({ label = Some s; _ }, _)
        | OptionActionChoice ({ label = Some s; _ }, _, _, _)
        | ChoiceIn { param = { label = Some s; _ }; _ } ->
            if List.mem s seen_names
            then ((seen_wildcards, seen_names, false), None)
            else ((seen_wildcards, s :: seen_names, false), Some s)
        | DynamicSelect ({ label = None; _ }, _) ->
          ((seen_wildcards, seen_names, all_wc), Some "_")
      ) ([], [], true) arg_list
    in
    let patterns = List.filter_map identity patterns in
    if patterns = [] then
      variant_name
    else if all_wildcard then
      variant_name ^ " _"
    else
      variant_name ^ " (" ^ String.concat ", " patterns ^ ")"

let set_param ~meta index param =
  let nullable = is_param_nullable param in
  let pname = show_param_name param index in
  let ptype = show_param_type param in
  let set_param_nullable v r = output "begin match %s with None -> T.set_param_null p | Some %s -> %s end;" pname v r in
  let module_ = Sql.Meta.find_opt meta "module" in
  match module_ with
  | Some m ->
      let set_param = "set_param" in
      let set_param_name = set_param |> Sql.Meta.find_opt meta |> Option.default set_param in
      let runtime_repr_name = L.as_runtime_repr_name param.typ in
      if nullable then
        set_param_nullable pname @@ sprintf "T.set_param_%s p (%s);" runtime_repr_name (sprintf "%s.%s %s" m set_param_name pname)
      else
        output "T.set_param_%s p (%s);" runtime_repr_name (sprintf "%s.%s %s" m set_param_name pname)
  | None ->
      match param with
      | { typ = { t=Union { ctors; _ }; _ }; _ } when nullable ->
          set_param_nullable "v" @@ (get_enum_name ctors) ^ ".set_param p v"
      | { typ = { t=Union { ctors; _ }; _ }; _ } ->
          output "%s.set_param p %s;" (get_enum_name ctors) pname
      | param' ->
          if nullable then
            set_param_nullable "v" @@ sprintf "T.set_param_%s %s" (show_param_type param') "p v"
          else
            output "T.set_param_%s p %s;" ptype pname
  
let set_var index var =
  let execute_generators = List.iter (fun f -> f ()) in
  let with_indent action = inc_indent (); action (); dec_indent () in

  let rec filter_generators index vars = List.filter_map (aux index) vars
  
  and aux index var = 
    match var with
    | Single (p, meta) -> 
      Some (fun () -> set_param ~meta index p)
    | SharedVarsGroup (vars, _) ->
      let generators = filter_generators index vars in
      if generators = [] then None
      else Some (fun () -> execute_generators generators)
    | TupleList (p, Where_in _) ->
      aux index (ChoiceIn { param = p; vars = []; kind = `In })
    | SingleIn _ | TupleList _ -> 
      None
    | ChoiceIn { param; vars; _ } ->
      let generators = filter_generators index vars in
      if generators = [] then None
      else Some (fun () ->
        output "begin match %s with" (make_param_name index param);
        output "| [] -> ()";
        output "| _ :: _ ->";
        with_indent (fun () ->
          execute_generators generators;
          output "()"
        );
        output "end;"
      )
    | OptionActionChoice (name, vars, _, _) ->
      let generators = filter_generators index vars in
      if generators = [] then None
      else Some (fun () ->
        let seen = Hashtbl.create 16 in
        let patterns = ref [] in
        List.iter (fun var ->
          let use_var = match var with
            | Single ({ id; _ }, _) | SingleIn ({id; _}, _) | TupleList (id, _) ->
              (match id.label with
               | Some name when Hashtbl.mem seen name -> false
               | Some name -> Hashtbl.add seen name (); true  
               | None -> true)
            | ChoiceIn _ | OptionActionChoice _ 
            | SharedVarsGroup _ | Choice _ | DynamicSelect _ -> true
          in
          if use_var then
            let pattern = match aux index var with
              | Some _ -> List.hd (names_of_vars [var])
              | None -> "_"
            in
            patterns := pattern :: !patterns
        ) vars;
        let param_pattern = match List.rev !patterns with
          | [single] -> single
          | many -> "(" ^ String.concat ", " many ^ ")"
        in
        output "begin match %s with" (make_param_name index name);
        output "| None -> ()";
        output "| Some %s ->" param_pattern;
        with_indent (fun () -> execute_generators generators);
        output "end;"
      )
    | Choice (name, ctors) ->
      let unit_branches = ref [] in
      let generator_branches = ref [] in
      let has_content = ref false in
      
      List.iteri (fun i ctor ->
        match ctor with
        | Simple (param, args) ->
          let args_list = Option.default [] args in
          let inner_generators = filter_generators index args_list in
          let branch_has_content = inner_generators <> [] in
          if branch_has_content then has_content := true;
          
          let pattern_args = 
            match args, branch_has_content with
            | (Some [] | None), false -> ""
            | (Some [] | None), true ->
              (match param.label with Some n -> " " ^ n | None -> "")
            | Some _, false -> " _"
            | Some l, true -> 
              " (" ^ String.concat "," (names_of_vars l) ^ ")"
          in
          let variant_name = 
            make_variant_name i param.label ~is_poly:true 
          in
          
          if branch_has_content then
            generator_branches := (fun () ->
              output "| %s%s ->" variant_name pattern_args;
              inc_indent ();
              execute_generators inner_generators;
              dec_indent ()
            ) :: !generator_branches
          else
            unit_branches := (fun () ->
              output "| %s%s -> ()" variant_name pattern_args
            ) :: !unit_branches

        | Verbatim (n, _) ->
          unit_branches := (fun () -> 
            output "| %s -> ()" (vname n ~is_poly:true)
          ) :: !unit_branches

      ) ctors;
      
      if not !has_content then None
      else
        let all_generators = 
          List.rev_append !unit_branches (List.rev !generator_branches)
        in
        Some (fun () ->
          output "begin match %s with" (make_param_name index name);
          execute_generators all_generators;
          output "end;"
        )
    | DynamicSelect _ ->
      (* DynamicSelect params are handled separately in generate_stmt_with_dynamic *)
      None
  in
  Option.may (fun g -> g ()) (aux index var)

let rec eval_count_params vars =
  let (static, choices, bool_choices, choices_in) =
    let classify_var = function
      | ChoiceIn { param; vars; _ } -> `WhereIn (param, vars)
      | TupleList (p, Where_in _) -> `WhereIn (p, [])
      | SingleIn _ -> `Static false
      | Single _ | TupleList _ -> `Static true
      | SharedVarsGroup (vars, _) -> `SharedVarsGroup vars
      | OptionActionChoice (param_id, vars, _, _) -> `OptionActionChoice (param_id, vars)
      | Choice (name, c) -> `Choice (name, c)
      | DynamicSelect _ -> `Static false (* handled separately in generate_stmt_with_dynamic *)
    in
    let rec group_vars (static, choices, bool_choices, choices_in) = function
      | [] -> (List.rev static, List.rev choices, List.rev bool_choices, List.rev choices_in)
      | x::xs ->
        match classify_var x with
        | `Static v -> group_vars (v::static, choices, bool_choices, choices_in) xs
        | `OptionActionChoice v -> group_vars (static, choices, v::bool_choices, choices_in) xs
        | `WhereIn v -> group_vars (static, choices, bool_choices, v::choices_in) xs
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
      | OptionActionChoice (p, v, pos, kind) -> Some (OptionActionChoice (p, exclude_in_vars v, pos, kind))
      | TupleList (_, Where_in _) as v -> Some v
      | TupleList _ -> None
      | ChoiceIn t -> Some (ChoiceIn { t with vars = exclude_in_vars t.vars })
      | Choice (param_id, ctors) ->
        Some (Choice (param_id, List.map exclude_in_vars_in_constructors ctors))
      | DynamicSelect (param_id, ctors) ->
        Some (DynamicSelect (param_id, List.map exclude_in_vars_in_constructors ctors)))
    l

and exclude_in_vars_in_constructors = function
  | Verbatim _ as ctor -> ctor
  | Simple (param_id, vars) -> Simple (param_id, Option.map exclude_in_vars vars)

let output_params_binder index vars =
  match exclude_in_vars vars with
  | [] -> "T.no_params"
  | vars -> output_params_binder index vars


let make_to_literal meta typ =
  match Sql.Meta.find_opt meta "module" with
  | Some m ->
    let set_param = "set_param" in
    let set_param_name = set_param |> Sql.Meta.find_opt meta |> Option.default set_param in
    let trait_type_name = match typ.Type.t with | Union _ -> "Text" | StringLiteral _ -> "Text" | _ -> Sql.Type.type_name typ in
    sprintf "(fun v -> T.Types.%s.%s_to_literal (%s.%s v))" trait_type_name (L.as_runtime_repr_name typ) m set_param_name
  | None ->
    match typ.Type.t with
    | Union { ctors; _ } -> sprintf "%s.to_literal" (get_enum_name ctors)
    | _ -> sprintf "T.Types.%s.to_literal" (Sql.Type.type_name typ)

let gen_in_substitution meta var =
  if Option.is_none var.id.label then failwith "empty label in IN param";
  sprintf {code| "(" ^ String.concat ", " (List.map %s %s) ^ ")"|code}
    (make_to_literal meta var.typ)
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
        let { name; domain; meta; _ } = attr in
        (if idx = 0 then "" else {|Buffer.add_string _sqlgg_b ", "; |}) ^
        sprintf {|Buffer.add_string _sqlgg_b (%s);|}
          (let to_literal = sprintf "%s %s" (make_to_literal meta domain) in
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
  List.mapi (fun idx (domain, meta) -> {
    name=(sprintf "%s_%Ln" label idx); domain; extra = Constraints.empty; meta
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
    | SubstIn (param, m) :: tl ->
      if app then bprintf b " ^ ";
      Buffer.add_string b (gen_in_substitution m param);
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
    | SubstTuple (id, Where_in (types, _, _)) :: tl ->
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
        (* TODO: Implement meta for ValueRows *)
        let types = List.map (fun typ -> typ, Meta.empty()) types in
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

(* Generate stmt with multiple dynamic selects *)
let generate_stmt_with_dynamic style index stmt dynamic_infos =
  if not (should_generate_for_style style stmt) then () else
  let name = choose_name stmt.props stmt.kind index |> String.uncapitalize_ascii in
  let subst = Props.get_all stmt.props "subst" in
  let inputs = (subst @ names_of_vars stmt.vars) |> List.map (fun v -> sprintf "~%s" v) |> inline_values in
  let needs_callback_param = match style with 
    | `List | `Fold -> true 
    | `Single -> is_callback stmt
    | `Direct -> is_callback stmt 
  in
  let needs_acc_param = style = `Fold in
  let all_inputs = inputs ^ (if needs_callback_param then " callback" else "") ^ (if needs_acc_param then " acc" else "") in
  
  output "let %s db %s =" name all_inputs;
  inc_indent ();
  
  let sql_pieces = get_sql stmt in
  
  (* Generate helpers for each dynamic select *)
  List.iter (fun di ->
    output "let rec params_count_%s : type a. a %s.t -> int = function" di.param_name di.module_name;
    inc_indent ();
    List.iter (fun ctor ->
      match ctor with
      | Sql.Simple (param_id, args) ->
        let field_name = field_name_of_param_id param_id in
        let arg_count = match args with Some l -> List.length l | None -> 0 in
        if arg_count = 0 then
          output "| %s.V %s -> 0" di.module_name field_name
        else
          output "| %s.V (%s _) -> %d" di.module_name field_name arg_count
      | Sql.Verbatim (n, _) ->
        output "| %s.V %s -> 0" di.module_name (String.capitalize_ascii n)
    ) di.ctors;
    output "| Return _ -> 0";
    output "| Map (t, _) -> params_count_%s t" di.param_name;
    output "| Both (a, b) -> params_count_%s a + params_count_%s b" di.param_name di.param_name;
    dec_indent ();
    output "in";
    
    output "let field_set_%s : type a. a %s.field -> T.params -> unit = function" di.param_name di.module_name;
    inc_indent ();
    List.iter (fun ctor ->
      match ctor with
      | Sql.Simple (param_id, args) ->
        let field_name = field_name_of_param_id param_id in
        let param_types = Option.default [] args |> List.filter_map (function Sql.Single (p, _) -> Some p.typ | _ -> None) in
        begin match param_types with
        | [] ->
          output "| %s -> fun _p -> ()" field_name
        | [t] ->
          output "| %s x -> fun p -> T.set_param_%s p x" field_name (L.as_lang_type t)
        | types ->
          let vars = List.mapi (fun i _ -> sprintf "x%d" i) types in
          let pattern = sprintf "(%s)" (String.concat ", " vars) in
          let sets = List.map2 (fun v t -> sprintf "T.set_param_%s p %s" (L.as_lang_type t) v) vars types in
          output "| %s %s -> fun p -> %s" field_name pattern (String.concat "; " sets)
        end
      | Sql.Verbatim (n, _) ->
        output "| %s -> fun _p -> ()" (String.capitalize_ascii n)
    ) di.ctors;
    dec_indent ();
    output "in";
    
    output "let rec set_%s : type a. a %s.t -> T.params -> unit = function" di.param_name di.module_name;
    inc_indent ();
    output "| %s.V f -> fun p -> field_set_%s f p" di.module_name di.param_name;
    output "| Return _ -> fun _p -> ()";
    output "| Map (t, _) -> fun p -> set_%s t p" di.param_name;
    output "| Both (a, b) -> fun p -> set_%s a p; set_%s b p" di.param_name di.param_name;
    dec_indent ();
    output "in";
    
    output "let field_to_column_%s : type a. a %s.field -> string = function" di.param_name di.module_name;
    inc_indent ();
    let field_sqls = List.find_map (function
      | Gen.Dynamic (pid, ctors) when pid = di.param_id -> 
        Some (List.map (fun c -> c.Gen.ctor, c.Gen.sql) ctors)
      | _ -> None
    ) sql_pieces |> Option.default [] in
    List.iter2 (fun ctor (_, sql) ->
      let body = match ctor with Sql.Verbatim (_, v) -> quote v | _ -> make_sql sql in
      output "| %s -> %s" (ctor_pattern ctor) body
    ) di.ctors field_sqls;
    dec_indent ();
    output "in";
    
    output "let rec to_a_field_list_%s : type a. a %s.t -> %s.a_field list = function" di.param_name di.module_name di.module_name;
    inc_indent ();
    output "| %s.V f -> [Any_field f]" di.module_name;
    output "| Return _ -> []";
    output "| Map (t, _) -> to_a_field_list_%s t" di.param_name;
    output "| Both (a, b) -> to_a_field_list_%s a @ to_a_field_list_%s b" di.param_name di.param_name;
    dec_indent ();
    output "in";
    
    output "let field_read_%s : type a. a %s.field -> T.row -> int -> a = function" di.param_name di.module_name;
    inc_indent ();
    List.iter2 (fun ctor (_, attr) ->
      output "| %s -> fun row idx -> %s" (ctor_pattern ctor) (format_get_column ~row:"row" ~idx:"idx" attr)
    ) di.ctors di.schema_fields;
    dec_indent ();
    output "in";
    
    output "let rec read_%s : type a. a %s.t -> T.row -> int -> a * int = function" di.param_name di.module_name;
    inc_indent ();
    output "| %s.V f -> fun row idx -> (field_read_%s f row idx, idx + 1)" di.module_name di.param_name;
    output "| Return x -> fun _row idx -> (x, idx)";
    output "| Map (t, f) -> fun row idx -> let (v, idx') = read_%s t row idx in (f v, idx')" di.param_name;
    output "| Both (a, b) -> fun row idx -> let (va, i1) = read_%s a row idx in let (vb, i2) = read_%s b row i1 in ((va, vb), i2)" di.param_name di.param_name;
    dec_indent ();
    output "in"
  ) dynamic_infos;
  
  (* Generate set_params *)
  let other_vars = List.filter (function Sql.DynamicSelect _ -> false | _ -> true) stmt.vars in
  let static_count = eval_count_params other_vars in
  let dynamic_counts = dynamic_infos |> List.map (fun di -> 
    sprintf "params_count_%s %s" di.param_name di.param_name
  ) |> String.concat " + " in
  
  output "let set_params stmt =";
  inc_indent ();
  output "let p = T.start_params stmt (%s + %s) in" static_count dynamic_counts;
  List.iter (fun di ->
    output "set_%s %s p;" di.param_name di.param_name
  ) dynamic_infos;
  List.iteri set_var other_vars;
  output "T.finish_params p";
  dec_indent ();
  output "in";
  
  let find_di_by_pid pid = List.find (fun di -> di.param_id = pid) dynamic_infos in
  
  let rec build_parts acc pending_comma = function
    | [] -> List.rev acc
    | Gen.Static s :: rest ->
      let s_trimmed = String.trim s in
      let ends_with_comma = String.length s_trimmed > 0 && s_trimmed.[String.length s_trimmed - 1] = ',' in
      if ends_with_comma then
        let s_no_comma = String.sub s_trimmed 0 (String.length s_trimmed - 1) in
        build_parts (if s_no_comma = "" then acc else quote s_no_comma :: acc) true rest
      else
        build_parts (quote s :: acc) false rest
    | Gen.Dynamic (pid, _) :: rest ->
      let di = find_di_by_pid pid in
      let fields_expr = sprintf "(%s |> to_a_field_list_%s |> List.map (fun (%s.Any_field f) -> field_to_column_%s f))"
        di.param_name di.param_name di.module_name di.param_name in
      let dyn_expr = 
        if pending_comma then
          sprintf "(match %s with [] -> \"\" | l -> \", \" ^ String.concat \", \" l)" fields_expr
        else
          sprintf "(String.concat \", \" %s)" fields_expr
      in
      build_parts (dyn_expr :: acc) false rest
    | _ :: rest -> build_parts acc pending_comma rest
  in
  let sql_parts = build_parts [] false sql_pieces in
  let sql_expr = String.concat " ^ " sql_parts in
  
  if style = `Fold then output "let r_acc = ref acc in";
  if style = `List then output "let r_acc = ref [] in";
  
  let func = select_func_of_kind stmt.kind in
  
  (* Split schema into segments by Dynamic *)
  let rec split_schema_multi acc current = function
    | [] -> List.rev ((List.rev current, None) :: acc)
    | Syntax.Dynamic (pid, _) :: rest ->
      let di = find_di_by_pid pid in
      split_schema_multi (((List.rev current), Some di.param_name) :: acc) [] rest
    | Syntax.Attr a :: rest ->
      split_schema_multi acc (a :: current) rest
  in
  let schema_segments = split_schema_multi [] [] stmt.schema in
  
  (* Build callback body with chained reads *)
  (* For Fold/List styles, we always need callback pattern. For Direct/Single, check is_callback *)
  let needs_callback = match style with
    | `Fold | `List -> true
    | `Direct | `Single -> is_callback stmt
  in
  let build_callback_body () =
    let buf = Buffer.create 256 in
    let static_idx = ref 0 in
    let current_idx_expr = ref None in (* None means use static_idx, Some s means use that expression *)
    let reads = ref [] in
    let attr_counter = ref 0 in
    
    List.iter (fun (attrs, dyn_opt) ->
      (* Read static attrs in this segment *)
      List.iteri (fun i attr ->
        let col_idx_expr = match !current_idx_expr with
          | None -> string_of_int !static_idx
          | Some base_var -> 
            if i = 0 then base_var
            else sprintf "(%s + %d)" base_var i
        in
        let value_expr = sprintf "(%s)" (format_get_column ~row:"row" ~idx:col_idx_expr attr) in
        if needs_callback then
          reads := sprintf "~%s:%s" (name_of attr !attr_counter) value_expr :: !reads
        else
          reads := value_expr :: !reads;
        incr static_idx;
        incr attr_counter
      ) attrs;
      
      (* Read dynamic part if present *)
      match dyn_opt with
      | Some param_name ->
        let read_var = sprintf "__sqlgg_r_%s" param_name in
        let next_idx_var = sprintf "__sqlgg_idx_after_%s" param_name in
        let start_idx = match !current_idx_expr with
          | None -> string_of_int !static_idx
          | Some base_var -> sprintf "(%s + %d)" base_var (List.length attrs)
        in
        Buffer.add_string buf (sprintf "let (%s, %s) = read_%s %s row %s in " 
          read_var next_idx_var param_name param_name start_idx);
        if needs_callback then
          reads := sprintf "~%s:%s" param_name read_var :: !reads
        else
          reads := read_var :: !reads;
        current_idx_expr := Some next_idx_var
      | None -> ()
    ) schema_segments;
    
    let params = List.rev !reads in
    if needs_callback then
      Buffer.add_string buf (sprintf "callback\n          %s" (String.concat "\n          " params))
    else
      Buffer.add_string buf (sprintf "(%s)" (String.concat ", " params));
    Buffer.contents buf
  in
  
  let callback_body = build_callback_body () in
  
  let (bind_start, bind_end, full_callback) =
    match style with
    | `Fold -> "IO.(>>=) (", ")", sprintf "(fun row -> r_acc := (%s !r_acc))" callback_body
    | `List -> "IO.(>>=) (", ")", sprintf "(fun row -> r_acc := (%s) :: !r_acc)" callback_body
    | `Direct | `Single -> "", "", sprintf "(fun row -> %s)" callback_body
  in
  
  output "%sT.%s db" bind_start func;
  output "  (%s)" sql_expr;
  output "  set_params %s%s" full_callback bind_end;
  if style = `Fold then output "(fun () -> IO.return !r_acc)";
  if style = `List then output "(fun () -> IO.return (List.rev !r_acc))";
  dec_indent ();
  empty_line ()

let generate_stmt style index stmt =
  let name = choose_name stmt.props stmt.kind index |> String.uncapitalize_ascii in
  let subst = Props.get_all stmt.props "subst" in
  let inputs = (subst @ names_of_vars stmt.vars) |> List.map (fun v -> sprintf "~%s" v) |> inline_values in
  if should_generate_for_style style stmt then begin
  let needs_callback_param = match style with | `List | `Single -> true | _ -> is_callback stmt in
  let needs_acc_param = style = `Fold in
  let all_inputs = inputs ^ (if needs_callback_param then " callback" else "") ^ (if needs_acc_param then " acc" else "") in
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
  let (func,callback) =
    match style with
    | `Single ->
      (match stmt.schema with
      | [] -> ("execute", "")
      | _ ->
        let func = select_func_of_kind stmt.kind in
        (func, output_schema_binder_labeled index stmt.schema))
    | _ -> output_schema_binder index stmt.schema stmt.kind
  in
  let params_binder_name = output_params_binder index stmt.vars in
  if style = `Fold then output "let r_acc = ref acc in";
  if style = `List then output "let r_acc = ref [] in";
  let (bind, callback) =
    match style with
    | `Fold -> "IO.(>>=) (", sprintf "(fun x -> r_acc := %s x !r_acc))" callback
    | `List -> "IO.(>>=) (", sprintf "(fun x -> r_acc := %s x :: !r_acc))" callback
    | `Single | `Direct -> "", callback (* or empty string *)
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
  end

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
    | Int | Text | Blob | Float | Bool | Json | UInt64
    | Datetime | Decimal _ | FloatingLiteral _ | Any | StringLiteral  _ | Json_path | One_or_all -> None
  in

  let meta_has_module m = Sql.Meta.mem m "module" in

  let schemas_to_enums schemas =
    List.filter_map (fun ({ domain; meta; _ } : Sql.attr) ->
      if meta_has_module meta then None else get_enum domain
    ) schemas
  in
  
  let schema_columns_to_enums schema_cols =
    List.concat_map (function
      | Syntax.Attr attr -> 
        if meta_has_module attr.meta then [] else get_enum attr.domain |> option_list
      | Syntax.Dynamic (_, fields) ->
        List.concat_map (fun (_, attr) -> 
          if meta_has_module attr.meta then [] else get_enum attr.domain |> option_list
        ) fields
    ) schema_cols
  in

  let rec vars_to_enums vars =
    let enum_opt typ = typ |> get_enum |> option_list in
    let enum_opt_with_meta typ meta = if meta_has_module meta then [] else enum_opt typ in
    List.concat_map (function
      | Single ({ typ; _ }, meta)
      | SingleIn ({ typ; _ }, meta) -> enum_opt_with_meta typ meta
      | SharedVarsGroup (vars, _)
      | OptionActionChoice (_, vars, _, _)
      | ChoiceIn { vars; _ } -> vars_to_enums vars
      | Choice (_, ctor_list)
      | DynamicSelect (_, ctor_list) -> 
        List.concat_map ( function
          | Simple (_, vars) -> Option.map vars_to_enums vars |> option_list |> List.concat
          | Verbatim _ -> []
        ) ctor_list
      | TupleList (_,  ValueRows { types; _ }) -> 
        List.concat_map enum_opt types
      | TupleList (_, Where_in (types, _, _)) ->
        List.concat_map (fun (typ, meta) -> enum_opt_with_meta typ meta) types
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
    let result = schema_columns_to_enums schemas @ vars_to_enums vars in
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
  
(* Extract all DynamicSelect infos from stmt *)
let get_all_dynamic_select_infos index stmt =
  let query_name = Gen.choose_name stmt.Gen.props stmt.Gen.kind index in
  let ds_from_vars = stmt.Gen.vars |> List.filter_map (function Sql.DynamicSelect (param_id, ctors) -> Some (param_id, ctors) | _ -> None) in
  let ds_from_schema = stmt.Gen.schema |> List.filter_map (function Syntax.Dynamic (param_id, fields) -> Some (param_id, fields) | _ -> None) in
  List.mapi (fun i ((param_id, ctors), (_, schema_fields)) ->
    let param_name = Gen.make_param_name i param_id in
    let module_name = sprintf "%s_%s" (String.capitalize_ascii query_name) param_name in
    { param_id; module_name; param_name; ctors; schema_fields }
  ) (List.combine ds_from_vars ds_from_schema)

(* Generate DynamicSelect GADT modules for all stmts that need them *)
let generate_dynamic_select_modules stmts =
  List.iteri (fun index stmt ->
    get_all_dynamic_select_infos index stmt |> List.iter (fun di ->
      let module_name = di.module_name in
      let fields = List.map2 (fun ctor (_field_param_id, attr) ->
        match ctor with
        | Sql.Simple (ctor_param_id, args) ->
          let param_types = Option.default [] args |> List.filter_map (function Sql.Single (p, _) -> Some p.typ | _ -> None) in
          let param_name = match ctor_param_id.Sql.label with Some s -> String.lowercase_ascii s | None -> "v" in
          (field_name_of_param_id ctor_param_id, param_name, param_types, attr)
        | Sql.Verbatim (name, _) ->
          (String.capitalize_ascii name, String.lowercase_ascii name, [], attr)
      ) di.ctors di.schema_fields in
      
      output "module %s = struct" module_name;
      inc_indent ();
      
      output "type _ field =";
      inc_indent ();
      List.iter (fun (field_name, _param_name, param_types, attr) ->
        let result_type = attr.Sql.domain in
        let result_type_str = L.as_lang_type result_type in
        let is_nullable = result_type.Sql.Type.nullability = Sql.Type.Nullable in
        let result_full = match Sql.Meta.find_opt attr.Sql.meta "module" with
          | Some m -> if is_nullable then sprintf "%s.t option" m else sprintf "%s.t" m
          | None -> if is_nullable then sprintf "T.Types.%s.t option" result_type_str else sprintf "T.Types.%s.t" result_type_str
        in
        match param_types with
        | [] -> output "| %s : %s field" field_name result_full
        | [t] -> output "| %s : T.Types.%s.t -> %s field" field_name (L.as_lang_type t) result_full
        | types ->
          let tuple = types |> List.map (fun t -> sprintf "T.Types.%s.t" (L.as_lang_type t)) |> String.concat " * " in
          output "| %s : (%s) -> %s field" field_name tuple result_full
      ) fields;
      dec_indent ();
      empty_line ();
      
      output "include Sqlgg_traits.Dynamic(struct type nonrec 'a t = 'a field end)";
      empty_line ();
      
      List.iter (fun (field_name, param_name, param_types, _attr) ->
        match param_types with
        | [] -> output "let %s = V %s" (String.lowercase_ascii field_name) field_name
        | _ -> output "let %s %s = V (%s %s)" (String.lowercase_ascii field_name) param_name field_name param_name
      ) fields;
      
      dec_indent ();
      output "end";
      empty_line ()
    )
  ) stmts

(* Wrapper to generate stmt with or without DynamicSelect *)
let generate_stmt_wrapper style index stmt =
  let dynamic_infos = get_all_dynamic_select_infos index stmt in
  match dynamic_infos with
  | [] -> generate_stmt style index stmt
  | _ -> generate_stmt_with_dynamic style index stmt dynamic_infos

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
  generate_dynamic_select_modules stmts;
  empty_line ();
  List.iteri (generate_stmt_wrapper `Direct) stmts;
  let has_fold = List.exists is_callback stmts in
  let has_list = has_fold in
  let has_single = List.exists (fun stmt ->
    match stmt.Gen.kind, stmt.Gen.schema with
    | Stmt.Select (`One | `Zero_one), _ :: _ -> true
    | _ -> false) stmts in
  [ 
    has_single, "Single", `Single;
    has_fold,   "Fold",   `Fold;
    has_list,   "List",   `List;
  ]
  |> List.filter_map (fun (cond, name, style) -> if cond then Some (name, style) else None)
  |> List.iteri (fun i (name, style) ->
    if i > 0 then output "";
    output "module %s = struct" name;
    inc_indent ();
    List.iteri (generate_stmt_wrapper style) stmts;
    dec_indent ();
    output "end (* module %s *)" name
  );
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
