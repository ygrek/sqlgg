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
  List.fold_left (fun acc x ->
    let x = ident ~prefix x in
    (if List.mem x acc then choose acc x 0 else x) :: acc) [] l
  |> List.rev

end

let inline_values = String.concat " "

let idents_of_attrs ~prefix attrs =
  Name.idents ~prefix (List.map (fun a -> a.Sql.name) attrs)

let quote = String.replace_chars (function '\n' -> "\\n\\\n" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let rec replace_all ~str ~sub ~by =
  match String.replace ~str ~sub ~by with
  | (true,s) -> replace_all ~str:s ~sub ~by
  | (false,s) -> s

let quote_comment_inline str =
  List.fold_left (fun str (sub, by) -> replace_all ~str ~sub ~by) str ["*)", "* )"; "(*", "( *"]

let make_comment str = "(* " ^ (quote_comment_inline str) ^ " *)"
let comment () fmt = Printf.ksprintf (indent_endline $ make_comment) fmt

let empty_line () = print_newline ()

let enums_hash_tbl = Hashtbl.create 100

let enum_get_hash ctors = Type.Enum_kind.Ctors.elements ctors |> String.concat "_"

let enum_name = Printf.sprintf "Enum_%d"

let get_enum_name ctors = ctors |> enum_get_hash |> Hashtbl.find enums_hash_tbl |> fst |> enum_name

let field_name_of_param_id (p : Sql.param_id) =
  match p.value with Some s -> String.capitalize_ascii s | None -> failwith "dynamic select branch must have a label"

let scoped_field_name name =
  let name = String.lowercase_ascii name in
  if List.mem name Name.reserved then name ^ "_" else name

type dynamic_info = {
  param_id: Sql.param_id;
  module_name: string;
  param_name: string;
  ctors: Sql.ctor list;
  schema_fields: Sql.attr Sql.dynamic_field list;
}

type scoped_field = {
  field_name: string;
  field_args: Sql.var list;
  field_schema: Sql.attr Sql.dynamic_field;
  field_ctor: Sql.ctor;
}

module L = struct
  open Type

  let as_lang_type t =
    type_name @@
    match t with
    | { t = Blob; nullability } | { t = StringLiteral _; nullability } -> { t = Text; nullability }
    | { t = FloatingLiteral _; nullability } -> { t = Float; nullability }
    | { t = Decimal _; nullability } -> { t = Decimal { precision = None; scale = None }; nullability }
    | t -> t

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

let codec_get_column, codec_set_param =
  let codec name meta =
    match Sql.Meta.find_opt meta "module", Sql.Meta.find_opt meta name with
    | Some m, fn -> Some (sprintf "%s.%s" m (Option.default name fn))
    | None, fn -> fn
  in
  codec "get_column", codec "set_param"

type codec_impl =
  | Custom_codec of string
  | Enum_codec of string
  | Builtin_codec of Type.t
let resolve_codec find typ meta =
  match find meta with
  | Some f -> Custom_codec f
  | None ->
    match typ.Type.t with
    | Union { ctors; _ } when !Sqlgg_config.enum_as_poly_variant -> Enum_codec (get_enum_name ctors)
    | Union _ -> Builtin_codec { typ with Type.t = Text }
    | _ -> Builtin_codec typ

let format_get_column ~row ~idx attr =
  let null_suffix = if is_attr_nullable attr then "_nullable" else "" in
  let format_t_get_column type_name =
    sprintf "T.get_column_%s%s %s %s" type_name null_suffix row idx
  in
  match resolve_codec codec_get_column attr.domain attr.meta with
  | Custom_codec getter ->
    sprintf "%s%s (%s)" getter null_suffix (format_t_get_column (L.as_runtime_repr_name attr.domain))
  | Enum_codec m ->
    sprintf "%s.get_column%s %s %s" m null_suffix row idx
  | Builtin_codec t ->
    format_t_get_column (L.as_lang_type t)

let get_column index attr =
  sprintf "(%s)" (format_get_column ~row:"stmt" ~idx:(string_of_int index) attr)

module T = Translate(L)

(* open L *)
open T

let schema_to_attrs schema =
  List.filter_map (function
    | Sql.Attr attr -> Some attr
    | Dynamic _ -> None
  ) schema

let format_labeled_param name value = sprintf "~%s:%s" name value

let emit_row_binder name emit_body =
  output "let %s stmt =" name;
  indented emit_body;
  output "in";
  name

let output_schema_binder_labeled _ schema =
  let attrs = schema_to_attrs schema in
  let args = idents_of_attrs ~prefix:"r" attrs in
  let values = List.mapi get_column attrs in
  emit_row_binder "invoke_callback" (fun () ->
    output "callback";
    indented (fun () -> List.iter2 (fun arg value -> output "%s" (format_labeled_param arg value)) args values))

let output_select1_cb _ schema =
  let attrs = schema_to_attrs schema in
  emit_row_binder "get_row" (fun () ->
    List.mapi get_column attrs |> String.concat ", " |> indent_endline)

let select_func_of_kind = function
| Stmt.Select `Zero_one -> "select_one_maybe"
| Stmt.Select `One -> "select_one"
| _ -> "select"

let is_single_row_select stmt =
  match stmt.Gen.kind, stmt.Gen.schema with
  | Stmt.Select (`One | `Zero_one), _ :: _ -> true
  | _ -> false

let has_row_callback stmt =
  match stmt.Gen.schema, stmt.Gen.kind with
  | [], _ -> false
  | _, Stmt.Select (`Zero_one | `One) -> false
  | _ -> true

let module_kind_name = function
  | `Direct -> "Direct"
  | `Single -> "Single"
  | `Fold -> "Fold"
  | `List -> "List"

let supports_module_kind module_kind stmt =
  match module_kind with
  | `List | `Fold -> has_row_callback stmt
  | `Single -> is_single_row_select stmt
  | `Direct -> true

let emit_module ?(annotate = false) name body =
  output "module %s = struct" name;
  indented body;
  if annotate then output "end (* module %s *)" name else output "end"

let emit_module_annotated = emit_module ~annotate:true

let emit_module_kind_variants stmt emit =
  [`Fold; `List] |> List.iter (fun module_kind ->
    if supports_module_kind module_kind stmt then begin
      emit_module_annotated (module_kind_name module_kind) (fun () -> emit module_kind);
      empty_line ()
    end)

let emit_cols_module ~brand_decl ~field_names body =
  List.iter (fun line -> output "%s" line) brand_decl;
  output "include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)";
  emit_module "Cols" body;
  output "include Cols";
  output "let cols = object";
  indented (fun () ->
    List.iter (fun name -> output "method %s = Cols.%s" name name) field_names);
  output "end"

let emit_verbatim_block src =
  let ind = make_indent () in
  String.split_on_char '\n' src
  |> List.iter (fun line ->
    if line = "" then print_newline ()
    else Printf.printf "%s%s\n" ind line)

let append_func_params ~has_callback ~module_kind inputs =
  inputs
  ^ (if has_callback then " callback" else "")
  ^ (if module_kind = `Fold then " acc" else "")

let emit_func_header ~name ~extra_params ~has_callback ~format_input ~module_kind stmt =
  let subst = Props.get_all stmt.props "subst" in
  let inputs = (subst @ names_of_vars stmt.vars) |> List.map format_input |> inline_values in
  output "let %s db%s %s =" name extra_params (append_func_params ~has_callback ~module_kind inputs);
  inc_indent ();
  subst

let format_func_input ~dyn_annot dynamic_names v =
  if List.mem v dynamic_names then sprintf "(%s : %s)" v (dyn_annot v)
  else sprintf "~%s" v

let gen_func_signature ~dynamic_infos ~module_kind ~index stmt =
  let dynamic_map = List.map (fun di -> (di.param_name, di.module_name)) dynamic_infos in
  let format_input =
    format_func_input (List.map fst dynamic_map)
      ~dyn_annot:(fun v -> sprintf "_ %s.t" (List.assoc v dynamic_map))
  in
  emit_func_header
    ~name:(choose_name stmt.props stmt.kind index |> String.uncapitalize_ascii)
    ~extra_params:""
    ~has_callback:(has_row_callback stmt || (module_kind = `Single && dynamic_infos = []))
    ~format_input ~module_kind stmt

type consumer = {
  c_init : unit -> unit;
  c_bind : string * string;
  c_callback : direct:string -> fold:string -> list:string -> string;
  c_finish : unit -> unit;
}

let consumer = function
  | `Direct | `Single ->
    { c_init = (fun () -> ());
      c_bind = ("", "");
      c_callback = (fun ~direct ~fold:_ ~list:_ -> direct);
      c_finish = (fun () -> ()) }
  | `Fold ->
    { c_init = (fun () -> output "let r_acc = ref acc in");
      c_bind = ("IO.(>>=) (", ")");
      c_callback = (fun ~direct:_ ~fold ~list:_ -> fold);
      c_finish = (fun () -> output "(fun () -> IO.return !r_acc)") }
  | `List ->
    { c_init = (fun () -> output "let r_acc = ref [] in");
      c_bind = ("IO.(>>=) (", ")");
      c_callback = (fun ~direct:_ ~fold:_ ~list -> list);
      c_finish = (fun () -> output "(fun () -> IO.return (List.rev !r_acc))") }

let complete_func c =
  c.c_finish ();
  dec_indent ();
  empty_line ()

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
        let make_wildcard_param value = 
          if List.mem value seen_wildcards
            then ((seen_wildcards, seen_names, all_wc), None)
            else ((value :: seen_wildcards, seen_names, all_wc), Some "_") in
        match arg with
        | Sql.Single (param, _) | SingleIn (param, _) -> make_wildcard_param param.id.value
        | SharedVarsGroup _
        | Choice ({ value = None; _ }, _)
        | TupleList ({ value = None; _ }, _) 
        | OptionActionChoice ({ value = None; _ }, _, _, _)
        | ChoiceIn { param = { value = None; _ }; _ }
        | DynamicSelect ({ value = None; _ }, _) ->
          ((seen_wildcards, seen_names, all_wc), Some "_")
        | TupleList ({ value = Some s; _ }, _) 
        | Choice ({ value = Some s; _ }, _)
        | DynamicSelect ({ value = Some s; _ }, _)
        | OptionActionChoice ({ value = Some s; _ }, _, _, _)
        | ChoiceIn { param = { value = Some s; _ }; _ } ->
            if List.mem s seen_names
            then ((seen_wildcards, seen_names, false), None)
            else ((seen_wildcards, s :: seen_names, false), Some s)
        | DynamicSelectJoin _ ->
          ((seen_wildcards, seen_names, all_wc), None)
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
  let set_param_nullable v r = output "begin match %s with None -> T.set_param_null p | Some %s -> %s end;" pname v r in
  match resolve_codec codec_set_param param.typ meta with
  | Custom_codec setter ->
      let set = sprintf "T.set_param_%s p (%s %s);" (L.as_runtime_repr_name param.typ) setter pname in
      if nullable then set_param_nullable pname set else output "%s" set
  | Enum_codec m ->
      if nullable then set_param_nullable "v" (m ^ ".set_param p v")
      else output "%s.set_param p %s;" m pname
  | Builtin_codec t ->
      let ptype = show_param_type { param with typ = t } in
      if nullable then set_param_nullable "v" (sprintf "T.set_param_%s p v" ptype)
      else output "T.set_param_%s p %s;" ptype pname

type var_class =
  | Bound_param of Sql.Type.t Sql.param * Sql.Meta.t
  | Substituted
  | No_params
  | Where_in_group of Sql.param_id * Sql.var list
  | Option_group of Sql.param_id * Sql.var list
  | Choice_group of Sql.param_id * Sql.ctor list
  | Shared_group of Sql.var list

let classify_var = function
  | Single (p, meta) -> Bound_param (p, meta)
  | TupleList (p, Where_in _) -> Where_in_group (p, [])
  | TupleList _ -> Substituted
  | SingleIn _ | DynamicSelect _ | DynamicSelectJoin _ -> No_params
  | ChoiceIn { param; vars; _ } -> Where_in_group (param, vars)
  | OptionActionChoice (id, vars, _, _) -> Option_group (id, vars)
  | Choice (id, ctors) -> Choice_group (id, ctors)
  | SharedVarsGroup (vars, _) -> Shared_group vars

let rec has_set_params vars =
  List.exists (fun var ->
    match classify_var var with
    | Bound_param _ -> true
    | Substituted | No_params -> false
    | Where_in_group (_, vars) | Option_group (_, vars) | Shared_group vars -> has_set_params vars
    | Choice_group (_, ctors) -> has_set_params (List.concat_map Sql.ctor_vars ctors)
  ) vars

let set_var index var =
  let execute_generators = List.iter (fun f -> f ()) in
  let with_indent action = inc_indent (); action (); dec_indent () in
  let unless_empty gens use = if gens = [] then None else Some (fun () -> use gens) in

  let rec filter_generators index vars = List.filter_map (aux index) vars

  and aux index var =
    match classify_var var with
    | Bound_param (p, meta) ->
      Some (fun () -> set_param ~meta index p)
    | Substituted | No_params ->
      None
    | Shared_group vars ->
      unless_empty (filter_generators index vars) execute_generators
    | Where_in_group (param, vars) ->
      unless_empty (filter_generators index vars) (fun generators ->
        output "begin match %s with" (make_param_name index param);
        output "| [] -> ()";
        output "| _ :: _ ->";
        with_indent (fun () ->
          execute_generators generators;
          output "()"
        );
        output "end;")
    | Option_group (name, vars) ->
      unless_empty (filter_generators index vars) (fun generators ->
        let patterns =
          List.fold_left (fun (seen, acc) var ->
            let bind_name = match var with
              | Single ({ id; _ }, _) | SingleIn ({ id; _ }, _) | TupleList (id, _) -> id.value
              | _ -> None
            in
            match bind_name with
            | Some n when List.mem n seen -> (seen, acc)
            | _ ->
              let seen = Option.map_default (fun n -> n :: seen) seen bind_name in
              let pattern = if Option.is_some (aux index var) then List.hd (names_of_vars [var]) else "_" in
              (seen, pattern :: acc)
          ) ([], []) vars
          |> snd |> List.rev
        in
        let param_pattern = match patterns with
          | [single] -> single
          | many -> "(" ^ String.concat ", " many ^ ")"
        in
        output "begin match %s with" (make_param_name index name);
        output "| None -> ()";
        output "| Some %s ->" param_pattern;
        with_indent (fun () -> execute_generators generators);
        output "end;")
    | Choice_group (name, ctors) ->
      let branch i = function
        | Verbatim (n, _) ->
          `Unit (fun () -> output "| %s -> ()" (vname n ~is_poly:true))
        | Simple (param, args) ->
          let inner = filter_generators index (Option.default [] args) in
          let variant_name = make_variant_name i param.value ~is_poly:true in
          let pattern_args =
            match args, inner with
            | (Some [] | None), [] -> ""
            | (Some [] | None), _ -> (match param.value with Some n -> " " ^ n | None -> "")
            | Some _, [] -> " _"
            | Some l, _ -> " (" ^ String.concat "," (names_of_vars l) ^ ")"
          in
          (match inner with
          | [] -> `Unit (fun () -> output "| %s%s -> ()" variant_name pattern_args)
          | inner -> `Gen (fun () ->
              output "| %s%s ->" variant_name pattern_args;
              with_indent (fun () -> execute_generators inner)))
      in
      let branches = List.mapi branch ctors in
      let units = List.filter_map (function `Unit f -> Some f | `Gen _ -> None) branches in
      let gens = List.filter_map (function `Gen f -> Some f | `Unit _ -> None) branches in
      unless_empty gens (fun gens ->
        output "begin match %s with" (make_param_name index name);
        execute_generators (units @ gens);
        output "end;")
  in
  Option.may (fun g -> g ()) (aux index var)

let rec eval_count_params vars =
  let (static, choices, bool_choices, choices_in) =
    let rec group_vars (static, choices, bool_choices, choices_in) = function
      | [] -> (List.rev static, List.rev choices, List.rev bool_choices, List.rev choices_in)
      | x::xs ->
        match classify_var x with
        | Bound_param _ | Substituted -> group_vars (true::static, choices, bool_choices, choices_in) xs
        | No_params -> group_vars (false::static, choices, bool_choices, choices_in) xs
        | Option_group (id, vars) -> group_vars (static, choices, (id, vars)::bool_choices, choices_in) xs
        | Where_in_group (id, vars) -> group_vars (static, choices, bool_choices, (id, vars)::choices_in) xs
        | Choice_group (id, ctors) -> group_vars (static, (id, ctors)::choices, bool_choices, choices_in) xs
        | Shared_group vars ->
          let static', choices', bool_choices', choices_in' = group_vars ([], [], [], []) vars in
          group_vars (static' @ static, choices' @ choices, bool_choices' @ bool_choices, choices_in' @ choices_in) xs
    in
    group_vars ([], [], [], []) vars
  in
  (* one shape for every dynamic contribution: " + (match X with P -> n | ...)" *)
  let count_match name arms =
    sprintf " + (match %s with %s)" name
      (arms |> List.map (fun (p, n) -> p ^ " -> " ^ n) |> String.concat " | ")
  in
  let counts fmt groups = groups |> List.mapi fmt |> String.concat "" in
  let static = string_of_int (List.length @@ List.filter (fun x -> x) static) in
  let choices_in = choices_in |> counts (fun i (param, vars) ->
    count_match (make_param_name i param)
      ["[]", "0"; "_ :: _", eval_count_params vars])
  in
  let bool_choices = bool_choices |> counts (fun i (param_id, vars) ->
    count_match (make_param_name i param_id)
      [ match_variant_pattern i (Some "Some") (Some vars) ~is_poly:false, eval_count_params vars;
        match_variant_pattern i (Some "None") None ~is_poly:false, "0" ])
  in
  let choices = choices |> counts (fun i (name, ctors) ->
    count_match (make_param_name i name)
      (ctors |> List.mapi (fun i -> function
        | Verbatim (n, _) -> vname n ~is_poly:true, "0"
        | Simple (param, args) ->
          match_variant_pattern i param.value args ~is_poly:true,
          eval_count_params (Option.default [] args))))
  in
  static ^ choices_in ^ choices ^ bool_choices

let emit_set_params ~count emit_vars =
  output "let set_params stmt =";
  inc_indent ();
  output "let p = T.start_params stmt (%s) in" count;
  emit_vars ();
  output "T.finish_params p";
  dec_indent ();
  output "in";
  "set_params"

let rec exclude_in_vars l =
  List.filter_map
    (function
      | SingleIn _ -> None
      | TupleList (_, Where_in _) as v -> Some v
      | TupleList _ -> None
      | v -> Some (Sql.map_sub_vars exclude_in_vars v))
    l

let output_params_binder _ vars =
  match exclude_in_vars vars with
  | [] -> "T.no_params"
  | vars -> emit_set_params ~count:(eval_count_params vars) (fun () -> List.iteri set_var vars)


let make_to_literal meta typ =
  match resolve_codec codec_set_param typ meta with
  | Custom_codec setter ->
    let trait_type_name = match typ.Type.t with | Union _ -> "Text" | StringLiteral _ -> "Text" | _ -> Sql.Type.type_name typ in
    sprintf "(fun v -> T.Types.%s.%s_to_literal (%s v))" trait_type_name (L.as_runtime_repr_name typ) setter
  | Enum_codec m -> sprintf "%s.to_literal" m
  | Builtin_codec t -> sprintf "T.Types.%s.to_literal" (Sql.Type.type_name t)

let gen_in_substitution meta var =
  if Option.is_none var.id.value then failwith "empty label in IN param";
  sprintf {code| "(" ^ String.concat ", " (List.map %s %s) ^ ")"|code}
    (make_to_literal meta var.typ)
    (Option.get var.id.value)

let gen_tuple_printer ~is_row _label schema =
  let params = idents_of_attrs ~prefix:"col" schema in
  let open_paren = if is_row then {|then "ROW(" else ", ROW("|} else {|then "(" else ", ("|} in
  sprintf
    {|(fun _sqlgg_idx (%s) -> Buffer.add_string _sqlgg_b (if _sqlgg_idx = 0 %s); %s Buffer.add_char _sqlgg_b ')')|}
    (String.concat ", " params)
    open_paren
    (String.concat " " @@
     List.mapi
     (fun idx (name, attr) ->
        let { domain; meta; _ } = attr in
        (if idx = 0 then "" else {|Buffer.add_string _sqlgg_b ", "; |}) ^
        sprintf {|Buffer.add_string _sqlgg_b (%s);|}
          (let to_literal = sprintf "%s %s" (make_to_literal meta domain) in
           if is_attr_nullable attr then 
           (sprintf {|match %s with None -> "NULL" | Some v -> %s|} name (to_literal "v") )
           else to_literal name))
     (List.combine params schema))

let resolve_tuple_label id = match id.value with
| None -> failwith "empty label in tuple param"
| Some value -> value

let gen_tuple_substitution ~is_row label schema =
  sprintf
    {|(let _sqlgg_b = Buffer.create 13 in List.iteri %s %s; Buffer.contents _sqlgg_b)|}
    (gen_tuple_printer ~is_row label schema)
    label 

let make_schema_of_tuple_types label =
  List.mapi (fun idx (domain, meta) -> {
    name=(sprintf "%s_%Ln" label idx); domain; extra = Constraints.empty; meta;
  })   

let join_ctors_of_vars vars =
  let module SM = Map.Make(String) in
  let joins = List.filter_map (function
    | Sql.DynamicSelectJoin { pos; source; _ } -> Some (fst pos, source)
    | Sql.Single _ | SingleIn _ | ChoiceIn _ | Choice _ | DynamicSelect _
    | TupleList _ | OptionActionChoice _ | SharedVarsGroup _ -> None) vars
  in
  let occurrences =
    List.fold_left (fun acc (_, s) ->
      SM.add s.Sql.table.tn (1 + Option.default 0 (SM.find_opt s.Sql.table.tn acc)) acc)
      SM.empty joins
  in
  let base (_, source) =
    let tn = source.Sql.table.tn in
    let name = (Sql.join_source_name source).tn in
    if SM.find tn occurrences > 1 && name <> tn then tn ^ "_" ^ name else tn
  in
  let ctors =
    joins |> List.map base |> Name.idents ~prefix:"join" |> List.map String.capitalize_ascii
  in
  List.map2 (fun (join_id, _) ctor -> join_id, ctor) joins ctors

let join_ctor join_ctors join_id =
  try List.assoc join_id join_ctors with Not_found -> fail "unknown dynamic join %d" join_id

let cond_test ~ctor_of ~deps_of = function
  | Gen.Dep_selected (pid, dep_id) -> sprintf "List.mem %s %s" (ctor_of dep_id) (deps_of pid)

let render_cond ~ctor_of ~deps_of cond body =
  sprintf {|(if %s then %s else "")|} (cond_test cond ~ctor_of ~deps_of) body

let make_sql ~join_ctors l =
  let rec render l =
    let parts =
      match l with
      | Gen.Static "" :: tl -> quote "" :: List.filter_map piece tl
      | l -> List.filter_map piece l
    in
    match parts with
    | [] -> quote ""
    | parts -> String.concat " ^ " parts
  and piece = function
    | Static "" -> None
    | Static s -> Some (quote s)
    | SubstIn (param, m) -> Some (gen_in_substitution m param)
    | DynamicIn (name, in_or_not_in, sqls) ->
      Some (sprintf "(match %s with [] -> \"%s\" | _ :: _ -> %s)"
        (make_param_name 0 name)
        (String.uppercase_ascii @@ string_of_bool @@ match in_or_not_in with `In -> false | `NotIn -> true)
        (render sqls))
    | Dynamic (name, ctors) ->
      Some (sprintf "(match %s with %s)"
        (make_param_name 0 name)
        (ctors
         |> List.mapi (fun i { ctor; args; sql; is_poly } ->
             sprintf "%s -> %s" (match_variant_pattern i ctor.value args ~is_poly) (render sql))
         |> String.concat " | "))
    | Cond (cond, body) ->
      Some (render_cond cond (render body)
        ~ctor_of:(fun id -> join_ctor join_ctors id)
        ~deps_of:(fun pid -> make_param_name 0 pid ^ ".deps"))
    | SubstTuple (id, Insertion schema) ->
      Some (gen_tuple_substitution ~is_row:false (resolve_tuple_label id) schema)
    | SubstTuple (id, Where_in { value = (types, _); pos = _ }) ->
      let label = resolve_tuple_label id in
      let schema = make_schema_of_tuple_types label types in
      Some (sprintf "%s ^ %s ^ %s"
        (quote "(") (gen_tuple_substitution ~is_row:false label schema) (quote ")"))
    | SubstTuple (id, ValueRows { types; _ }) ->
      let label = resolve_tuple_label id in
      (* TODO: Implement meta for ValueRows *)
      let types = List.map (fun typ -> typ, Meta.empty()) types in
      let schema = make_schema_of_tuple_types label types in
      let empty = schema
        |> List.map (const "NULL")
        |> String.join ", "
        |> sprintf {|"SELECT %s WHERE FALSE"|} in
      let not_empty = gen_tuple_substitution ~is_row:true label schema in
      Some (sprintf {|( if %s = [] then %s else ( "VALUES " ^ %s ) )|} label empty not_empty)
  in
  "(" ^ render l ^ ")"

type callback_build_state = {
  bindings: string list;
  reads: string list;
  static_idx: int;
  attr_n: int;
  idx_expr: string option;
}

let emit_dynamic_select_body ~module_kind ~dynamic_infos ~in_module stmt =
  let sql_pieces = get_sql stmt in
  let join_ctors = join_ctors_of_vars stmt.Gen.vars in

  let col_ref di = di.param_name in
  let deps_ref di = di.param_name ^ ".deps" in
  let ctor_prefix di = if in_module then "" else di.module_name ^ "." in

  let other_vars = List.filter (function Sql.DynamicSelect _ -> false | _ -> true) stmt.vars in
  let static_count = eval_count_params other_vars in
  let dynamic_counts = dynamic_infos |> List.map (fun di ->
    sprintf "%s.count" (col_ref di)
  ) |> String.concat " + " in

  let find_di_by_pid pid = List.find (fun di -> di.param_id = pid) dynamic_infos in
  let is_di pid = List.exists (fun di -> di.param_id = pid) dynamic_infos in

  (* params are bound positionally, so col.set must run exactly where the
     dynamic select occurs among the other vars, not before them;
     indices skip dynamic selects to stay in sync with other_vars *)
  let set_vars_in_order () =
    let set index = function
      | Sql.DynamicSelect (pid, _) -> output "%s.set p;" (col_ref (find_di_by_pid pid)); index
      | var -> set_var index var; index + 1
    in
    let (_ : int) = List.fold_left set 0 stmt.vars in
    ()
  in
  let (_ : string) =
    emit_set_params ~count:(sprintf "%s + %s" static_count dynamic_counts) set_vars_in_order
  in

  let rec build_parts acc pending_comma = function
    | [] -> List.rev acc
    | Gen.Static s :: rest ->
      let piece, pending =
        match String.rindex_opt s ',' with
        | Some i when String.(trim (slice ~first:(i + 1) s)) = "" -> String.slice ~last:i s, true
        | _ -> s, false
      in
      let acc = if pending && String.trim piece = "" then acc else quote piece :: acc in
      build_parts acc pending rest
    | Gen.Dynamic (pid, _) :: rest when is_di pid ->
      let di = find_di_by_pid pid in
      let dyn_expr =
        if pending_comma then
          sprintf {|(match %s.column with "" -> "" | c -> ", " ^ c)|} (col_ref di)
        else
          sprintf "%s.column" (col_ref di)
      in
      build_parts (dyn_expr :: acc) false rest
    | Gen.Cond (cond, body) :: rest ->
      let Gen.Dep_selected (pid, _) = cond in
      let di = find_di_by_pid pid in
      let body_expr =
        match build_parts [] false body with
        | [] -> {|""|}
        | parts -> String.concat " ^ " parts
      in
      let expr =
        render_cond cond body_expr
          ~ctor_of:(fun dep_id -> ctor_prefix di ^ join_ctor join_ctors dep_id)
          ~deps_of:(fun _ -> deps_ref di)
      in
      build_parts (expr :: acc) pending_comma rest
    | (Gen.Dynamic _ | Gen.SubstIn _ | Gen.DynamicIn _ | Gen.SubstTuple _) as piece :: rest ->
      let expr = make_sql ~join_ctors [piece] in
      let acc = if pending_comma then expr :: quote "," :: acc else expr :: acc in
      build_parts acc false rest
  in
  let sql_expr = String.concat " ^ " (build_parts [] false sql_pieces) in

  let c = consumer module_kind in
  c.c_init ();

  let rec split_schema_multi acc current = function
    | [] -> List.rev ((List.rev current, None) :: acc)
    | Sql.Dynamic (pid, _) :: rest ->
      let di = find_di_by_pid pid in
      split_schema_multi (((List.rev current), Some di) :: acc) [] rest
    | Attr a :: rest ->
      split_schema_multi acc (a :: current) rest
  in
  let schema_segments = split_schema_multi [] [] stmt.schema in

  let labeled = has_row_callback stmt in
  let build_callback_body () =
    let col_idx_at ~base ~offset = match base with
      | None -> string_of_int offset
      | Some var when offset = 0 -> var
      | Some var -> sprintf "(%s + %d)" var offset
    in
    let process_attrs st attrs =
      List.fold_left (fun (st, i) attr ->
        let col_idx = col_idx_at ~base:st.idx_expr ~offset:(st.static_idx + i) in
        let value = sprintf "(%s)" (format_get_column ~row:"row" ~idx:col_idx attr) in
        let read = if labeled then format_labeled_param (name_of attr st.attr_n) value else value in
        ({ st with reads = read :: st.reads; attr_n = st.attr_n + 1 }, i + 1)
      ) (st, 0) attrs |> fst
    in
    let process_dynamic st di =
      let read_var = sprintf "__sqlgg_r_%s" di.param_name in
      let next_var = sprintf "__sqlgg_idx_after_%s" di.param_name in
      let start = col_idx_at ~base:st.idx_expr ~offset:st.static_idx in
      let binding = sprintf "let (%s, %s) = %s.read row %s in " read_var next_var (col_ref di) start in
      { bindings = binding :: st.bindings;
        reads = read_var :: st.reads;
        static_idx = 0;
        attr_n = st.attr_n + 1; idx_expr = Some next_var }
    in
    let process_segment st (attrs, dyn_opt) =
      let st = { (process_attrs st attrs) with static_idx = st.static_idx + List.length attrs } in
      match dyn_opt with None -> st | Some di -> process_dynamic st di
    in
    let init = { bindings = []; reads = []; static_idx = 0; attr_n = 0; idx_expr = None } in
    let final = List.fold_left process_segment init schema_segments in
    let reads = List.rev final.reads in
    String.concat "" (List.rev final.bindings) ^
    if labeled then
      sprintf "callback\n          %s" (String.concat "\n          " reads)
    else
      sprintf "(%s)" (String.concat ", " reads)
  in

  let callback_body = build_callback_body () in

  let full_callback =
    c.c_callback
      ~direct:(sprintf "(fun row -> %s)" callback_body)
      ~fold:(sprintf "(fun row -> r_acc := (%s !r_acc))" callback_body)
      ~list:(sprintf "(fun row -> r_acc := (%s) :: !r_acc)" callback_body)
  in
  let (bind_start, bind_end) = c.c_bind in

  output "%sT.%s db" bind_start (select_func_of_kind stmt.kind);
  output "  (%s)" sql_expr;
  output "  set_params %s%s" full_callback bind_end;
  complete_func c

let emit_dynamic_module_select ~module_kind ~dynamic_infos stmt =
  if not (supports_module_kind module_kind stmt) then () else
  let dynamic_names = List.map (fun di -> di.param_name) dynamic_infos in
  let format_input = format_func_input dynamic_names ~dyn_annot:(fun _ -> "_ t") in
  let (_ : string list) =
    emit_func_header ~name:"select" ~extra_params:""
      ~has_callback:(has_row_callback stmt) ~format_input ~module_kind stmt
  in
  emit_dynamic_select_body ~module_kind ~dynamic_infos ~in_module:true stmt

let emit_sql_with_subst subst stmt =
  let sql = make_sql ~join_ctors:(join_ctors_of_vars stmt.Gen.vars) @@ get_sql stmt in
  match subst with
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

let empty_exec_result = {|IO.return { T.affected_rows = 0L; insert_id = None }|}

let generate_stmt ~module_kind index stmt =
  if not (supports_module_kind module_kind stmt) then () else
  let c = consumer module_kind in
  if Props.get stmt.props "noop" <> None then begin
    let _ = gen_func_signature ~dynamic_infos:[] ~module_kind ~index stmt in
    output "ignore db;";
    output "%s" empty_exec_result;
    complete_func c
  end else
  let subst = gen_func_signature ~dynamic_infos:[] ~module_kind ~index stmt in
  let sql = emit_sql_with_subst subst stmt in
  let (func, callback) =
    match stmt.schema with
    | [] -> "execute", ""
    | _ ->
      select_func_of_kind stmt.kind,
      match module_kind, stmt.kind with
      | (`Direct | `Fold | `List), Stmt.Select (`Zero_one | `One) -> output_select1_cb index stmt.schema
      | _ -> output_schema_binder_labeled index stmt.schema
  in
  let params_binder_name = output_params_binder index stmt.vars in
  c.c_init ();
  let callback =
    c.c_callback
      ~direct:callback
      ~fold:(sprintf "(fun x -> r_acc := %s x !r_acc)" callback)
      ~list:(sprintf "(fun x -> r_acc := %s x :: !r_acc)" callback)
  in
  let (bind, bind_end) = c.c_bind in
  let exec = sprintf "T.%s db %s %s %s%s" func sql params_binder_name callback bind_end in
  let exec =
    match
      List.find_map
        (function
          | SubstTuple (id, Insertion _) -> Some id
          | SubstTuple (_, ( Where_in _| ValueRows _ ))
          | Static _ | Dynamic _ | DynamicIn _ | SubstIn _ | Cond _ -> None)
        (get_sql stmt)
    with
    | None -> exec
    | Some { value = None; _ } -> failwith "empty label in tuple substitution"
    | Some { value = Some value; _ } ->
      sprintf {|( match %s with [] -> %s | _ :: _ -> %s)|} value empty_exec_result exec
  in
  output "%s%s" bind exec;
  complete_func c

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

  let enum_unless_codec codec typ meta =
    match codec meta with Some _ -> None | None -> get_enum typ
  in

  let schemas_to_enums schemas =
    List.filter_map (fun ({ domain; meta; _ } : Sql.attr) ->
      enum_unless_codec codec_set_param domain meta
    ) schemas
  in

  let schema_columns_to_enums schema_cols =
    let attr_enum attr = option_list (enum_unless_codec codec_get_column attr.domain attr.meta) in
    List.concat_map (function
      | Sql.Attr attr -> attr_enum attr
      | Dynamic (_, fields) -> List.concat_map (fun f -> attr_enum f.Sql.field_attr) fields
    ) schema_cols
  in

  let rec vars_to_enums vars =
    let enum_opt typ = typ |> get_enum |> option_list in
    let enum_opt_with_meta typ meta = option_list (enum_unless_codec codec_set_param typ meta) in
    List.concat_map (function
      | Single ({ typ; _ }, meta)
      | SingleIn ({ typ; _ }, meta) -> enum_opt_with_meta typ meta
      | TupleList (_,  ValueRows { types; _ }) -> 
        List.concat_map enum_opt types
      | TupleList (_, Where_in { value = (types, _); pos = _ }) ->
        List.concat_map (fun (typ, meta) -> enum_opt_with_meta typ meta) types
      | TupleList (_, Insertion schema) -> schemas_to_enums schema
      | v -> vars_to_enums (Sql.sub_vars v)
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
    let (_ : int) = List.fold_left (fun acc enum ->
      let hash = enum_get_hash enum in
      if Hashtbl.mem enums_hash_tbl hash then acc
      else begin
        Hashtbl.add enums_hash_tbl hash (acc, enum);
        empty_line ();
        generate_enum_module acc enum;
        acc + 1
      end) 0 result
    in ())

let generate_enum_modules stmts = if !Sqlgg_config.enum_as_poly_variant then generate_enum_modules stmts
  
let get_all_dynamic_select_infos index stmt =
  let query_name = Gen.choose_name stmt.Gen.props stmt.Gen.kind index in
  let ds_from_vars = stmt.Gen.vars |> List.filter_map (function Sql.DynamicSelect (param_id, ctors) -> Some (param_id, ctors) | _ -> None) in
  let ds_from_schema = stmt.Gen.schema |> List.filter_map (function Sql.Dynamic (param_id, fields) -> Some (param_id, fields) | _ -> None) in
  let module_name = match ds_from_vars with
    | [_] -> fun _param_name -> String.capitalize_ascii query_name
    | _ -> fun param_name -> sprintf "%s_%s" (String.capitalize_ascii query_name) param_name
  in
  List.mapi (fun i ((param_id, ctors), (_, schema_fields)) ->
    let param_name = Gen.make_param_name i param_id in
    { param_id; module_name = module_name param_name; param_name; ctors; schema_fields }
  ) (List.combine ds_from_vars ds_from_schema)

let generate_dynamic_select_modules stmts =
  List.iteri (fun index stmt ->
    let all_dis = get_all_dynamic_select_infos index stmt in
    let single_di = List.length all_dis = 1 in
    let sql_pieces = get_sql stmt in
    let join_ctors = join_ctors_of_vars stmt.Gen.vars in
    let deps_of_field (field : _ Sql.dynamic_field) =
      match field.Sql.join_deps with
      | [] -> "[]"
      | ids -> sprintf "[%s]" (ids |> List.map (join_ctor join_ctors) |> String.concat "; ")
    in
    let brand_decl =
      [ (match join_ctors with
        | [] -> "type brand"
        | ctors -> sprintf "type brand = %s" (ctors |> List.map snd |> String.concat " | "))
      ]
    in
    all_dis |> List.iter (fun di ->
      let module_name = di.module_name in
      let field_sqls = List.find_map (function
        | Gen.Dynamic (pid, ctors) when pid = di.param_id -> 
          Some (List.map (fun c -> c.Gen.ctor, c.Gen.sql) ctors)
        | _ -> None
      ) sql_pieces |> Option.default [] in
      
      let fields = List.map2 (fun ctor field ->
        let name, args = match ctor with
          | Sql.Simple (ctor_param_id, args) -> field_name_of_param_id ctor_param_id, Option.default [] args
          | Sql.Verbatim (name, _) -> name, []
        in
        { field_name = scoped_field_name name; field_args = args; field_schema = field; field_ctor = ctor }
      ) di.ctors di.schema_fields in
      
      emit_module module_name (fun () ->
        let emit_field { field_name; field_args; field_schema; field_ctor } (_, sql) =
          let read_body = sprintf "(fun row idx -> (%s, idx + 1))" (format_get_column ~row:"row" ~idx:"idx" field_schema.Sql.field_attr) in
          let column_body = match field_ctor with
            | Sql.Verbatim (_, v) -> quote v
            | _ -> make_sql ~join_ctors sql
          in
          let count_expr = eval_count_params field_args in
          let set_helper_name = sprintf "_set_%s" field_name in
          begin match names_of_vars field_args with
          | [] -> output "let %s : _ t =" field_name
          | names -> output "let %s %s : _ t =" field_name (String.concat " " names)
          end;
          indented (fun () ->
            let set_ref =
              if field_args <> [] && has_set_params field_args then begin
                output "let %s p =" set_helper_name;
                indented (fun () ->
                  List.iteri set_var field_args;
                  output "()");
                output "in";
                set_helper_name
              end else
                "(fun _p -> ())"
            in
            output "{";
            indented (fun () ->
              output "set = %s;" set_ref;
              output "read = %s;" read_body;
              output "column = %s;" column_body;
              output "count = %s;" count_expr;
              output "deps = %s;" (deps_of_field field_schema));
            output "}")
        in
        emit_cols_module ~brand_decl
          ~field_names:(List.map (fun f -> f.field_name) fields)
          (fun () -> List.iter2 emit_field fields field_sqls);

        if single_di then begin
          empty_line ();
          emit_dynamic_module_select ~module_kind:`Direct ~dynamic_infos:[di] stmt;
          emit_module_kind_variants stmt (fun module_kind ->
            emit_dynamic_module_select ~module_kind ~dynamic_infos:[di] stmt)
        end);
      empty_line ()
    )
  ) stmts

let generate_stmt_wrapper ~module_kind index stmt =
  let dynamic_infos = get_all_dynamic_select_infos index stmt in
  match dynamic_infos with
  | [] -> generate_stmt ~module_kind index stmt
  | [_] -> ()
  | _ :: _ :: _ ->
    if supports_module_kind module_kind stmt then begin
      let _subst = gen_func_signature ~dynamic_infos ~module_kind ~index stmt in
      emit_dynamic_select_body ~module_kind ~dynamic_infos ~in_module:false stmt
    end

let generate ~gen_io ~migration_names name stmts =
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
  List.iteri (generate_stmt_wrapper ~module_kind:`Direct) stmts;
  [`Single; `Fold; `List]
  |> List.filter (fun module_kind -> List.exists (supports_module_kind module_kind) stmts)
  |> List.iteri (fun i module_kind ->
    if i > 0 then output "";
    emit_module_annotated (module_kind_name module_kind) (fun () ->
      List.iteri (generate_stmt_wrapper ~module_kind) stmts)
  );
  Option.may (fun names ->
    output "let migrations = [";
    inc_indent ();
    List.iter (fun n ->
      output "(%s, apply_%s, revert_%s);" (quote n) n n;
    ) names;
    dec_indent ();
    output "]";
    empty_line ()
  ) migration_names;
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
  let generate () name stmts = generate ~gen_io:false ~migration_names:None name stmts
end

module Generator_io = struct
  include Generator_base
  let generate () name stmts = generate ~gen_io:true ~migration_names:None name stmts
end

module Header = Gen.Make(Generator_io)

let generate_migrations name migrations =
  let named = List.mapi (fun index (m : Gen_migrations.migration) ->
    Gen.choose_name m.props m.kind index, m
  ) migrations in
  let migration_names = List.map fst named in
  let make_stmt fn_name sql =
    { Gen.schema = []; vars = []; kind = Stmt.Other;
      props = Props.set (Props.set Props.empty "name" fn_name) "sql" sql }
  in
  let revert_stmt name (m : Gen_migrations.migration) =
    match m.revert with
    | [] -> let s = make_stmt ("revert_" ^ name) "" in { s with props = Props.set s.props "noop" "" }
    | revert -> make_stmt ("revert_" ^ name) (String.concat ";\n" revert)
  in
  let stmts = List.concat_map (fun (name, (m : Gen_migrations.migration)) ->
    [make_stmt ("apply_" ^ name) (String.concat ";\n" m.apply); revert_stmt name m]
  ) named in
  Option.may (Header.generate_header ()) !Sqlgg_config.gen_header;
  generate ~gen_io:true ~migration_names:(Some migration_names) name stmts
