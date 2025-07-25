(** SQL syntax and RA *)

open Printf
open ExtLib
open Prelude
open Sql

let debug = ref false

type env = {
  tables : Tables.table list;
  schema : table_name Schema.Source.t;
  (* 
    1. CTEs = tables for the current statement (not keeping during whole .sql)
    2. It merges with global tables during source resolving
    3. The Tables field mostly stores aliases and forms a scheme
  *)
  ctes : Tables.table list;
  insert_schema : Schema.t;
  (* it is used to apply non-null comparison semantics inside WHERE expressions *)
  set_tyvar_strict: bool;
  query_has_grouping: bool;
  is_order_by: bool;
}

(* Merge global tables with ctes during resolving sources in SELECT .. FROM sources, JOIN *)
module Tables_with_derived = struct
  open Tables

  let get ~env name = get_from  (env.ctes @ Tables.all()) name

  let get_from ~env name = get_from  (env.ctes @ env.tables) name
end

type enum_ctor_value_data = { ctor_name: string; pos: pos; } [@@deriving show]

(* expr with all name references resolved to values or "functions" *)
type res_expr =
  | ResValue of Type.t (** literal value *)
  | ResParam of param * Meta.t
  | ResSelect of Type.t * vars
  | ResInTupleList of { param_id: param_id; res_in_tuple_list: res_in_tuple_list; kind: in_or_not_in; pos: pos; }
  | ResInparam of param * Meta.t
  | ResChoices of param_id * res_expr choices
  | ResInChoice of param_id * in_or_not_in * res_expr
  | ResFun of res_fun (** function kind (return type and flavor), arguments *)
  | ResOptionActions of { choice_id: param_id; res_choice: res_expr; pos: (pos * pos); kind: Sql.option_actions_kind }
  | ResCase of { case: res_expr option; branches: case_branch list; else_: res_expr option }
  [@@deriving show] 

and case_branch = { when_: res_expr; then_: res_expr; } [@@deriving show]

and res_fun = { kind: Type.func ; parameters: res_expr list; is_over_clause: bool; } [@@deriving show]  
  
and res_in_tuple_list = 
  ResTyped of (Type.t * Meta.t) list | Res of (res_expr * Meta.t) list

let empty_env = { query_has_grouping = false; 
  tables = []; schema = []; 
  insert_schema = []; 
  set_tyvar_strict = false; 
  ctes = [];
  is_order_by = false;
}

let flat_map f l = List.flatten (List.map f l)

let schema_of ~env name =
  let result = Tables_with_derived.get_from ~env name in 
  List.map (fun attr -> { Schema.Source.Attr.sources=[result |> fst]; attr; }) (result |> snd)

let get_or_failwith = function `Error s -> failwith s | `Ok t -> t

let values_or_all table names =
  let schema = Tables.get_schema table in
  match names with
  | Some names -> 
    let req_missing =
      List.filter_map
        (fun { extra; name; _ } ->
          let open Constraints in
          if inter (of_list [Autoincrement; WithDefault; NotNull]) extra = of_list [NotNull]
            && not @@ List.mem name names then Some name
          else None
        )
        schema
    in
    begin match req_missing with 
    | [] -> ()
    | fields -> 
        fail "Fields: (%s) don't have a default value" (String.concat "," fields) end;    
    Schema.project names schema
  | None -> schema

let rec get_params_of_res_expr (e:res_expr) =
  let rec loop acc e =
    match e with
    | ResSelect (_, p) -> (List.rev p) @ acc
    | ResCase { case; branches; else_ } ->
      let acc = match case with Some e -> loop acc e | None -> acc in
      let acc = List.fold_left (fun acc { when_; then_ } -> loop (loop acc when_) then_) acc branches in
      Option.map_default (loop acc) acc else_
    | ResParam (p, m) -> Single (p, m) ::acc
    | ResOptionActions{ choice_id; res_choice; pos; kind} -> 
      OptionActionChoice (choice_id, get_params_of_res_expr res_choice, pos, kind) :: acc
    | ResInTupleList { param_id; res_in_tuple_list = ResTyped types; kind; pos } -> TupleList (param_id, Where_in (types, kind, pos)) :: acc
    | ResInparam (p, m) -> SingleIn (p, m)::acc
    | ResFun { parameters; _ } -> List.fold_left loop acc parameters
    | ResInTupleList _
    | ResValue _ -> acc
    | ResInChoice (param, kind, e) -> ChoiceIn { param; kind; vars = get_params_of_res_expr e } :: acc
    | ResChoices (p,l) -> Choice (p, List.map (fun (n,e) -> Simple (n, Option.map get_params_of_res_expr e)) l) :: acc
  in
  loop [] e |> List.rev

let list_same l =
  match l with
  | [] -> None
  | x::xs -> if List.for_all (fun y -> x = y) xs then Some x else None

let rec is_grouping = function
| Value _
| Param _
| Column _
| SelectExpr _
| Inparam _
| InTupleList _
| OptionActions _
| Inserted _ -> false
| Choices (p,l) ->
  begin match list_same @@ List.map (fun (_,expr) -> Option.map_default is_grouping false expr) l with
  | None -> failed ~at:p.pos "inconsistent grouping in choice branches"
  | Some v -> v
  end
| Case { case; branches; else_ } ->
  List.exists is_grouping 
    (option_list case @ 
     List.flatten (List.map (fun { Sql.when_; then_ } -> [when_; then_]) branches) @ 
     option_list else_)
| InChoice (_, _, e) -> is_grouping e
| Fun { kind ; parameters; _ } ->
  (* grouping function of zero or single parameter or function on grouping result *)
  (Type.is_grouping kind && List.length parameters <= 1) || List.exists is_grouping parameters

let rec is_windowing = function
| Value _
| Param _
| Column _
| SelectExpr _
| Inparam _
| InTupleList _
| OptionActions _
| Inserted _ -> false
| Choices (_, l) -> List.exists (fun (_, e) -> Option.map_default is_windowing false e ) l
| InChoice (_, _, e) -> is_windowing e
| Case { case; branches; else_ } ->
  List.exists is_windowing 
  (option_list case @ List.flatten (List.map (fun { Sql.when_; then_ } -> [when_; then_]) branches) @ option_list else_)
| Fun { is_over_clause; _ } -> is_over_clause

let exists_grouping columns =
  List.exists (function Expr (e,_) -> is_grouping e | All | AllOf _ -> false) columns

let exists_windowing columns =
  List.exists (function Expr (e,_) -> is_windowing e | All | AllOf _ -> false) columns  

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
let make_unique =
  List.unique ~cmp:(fun a1 a2 ->
    (* Check if columns are from the same table (source)  *)
    a1.Schema.Source.Attr.sources = a2.sources
    && a1.attr.name = a2.attr.name
    (* Check if columns are named *)
    && a1.attr.name <> "")

let all_columns = make_unique $ Schema.cross_all

let all_tbl_columns = all_columns $ List.map snd

let resolve_column ~env {cname;tname} =
  let open Schema.Source in
  let open Attr in
  let by_name_and_sources tname name source_attr = source_attr.attr.name = 
    name && Option.map_default 
      (fun tname -> List.mem tname.tn (List.map(fun i -> i.tn) source_attr.sources)) false tname in
  let find_by t name = List.find_all (by_name_and_sources tname name) t in
  let find t name =
    match find_by t name with
    | [x] -> Some x
    | [] -> None
    | list -> Some (List.last list) in
  let result = find env.schema cname in
  let find_by_name t name = List.find_all (by_name name) t in
  let find t name =
    let err_data = from_schema t in
    match find_by_name t name with
    | [x] -> x
    | [] -> raise (Schema.Error (err_data,"missing attribute : " ^ name))
    | _ -> raise (Schema.Error (err_data,"duplicate attribute : " ^ name))
  in  
  match result with 
  | None -> find (Option.map_default (schema_of ~env) env.schema tname) cname
  | Some result -> result

let set_param_meta ~env col e = 
  let m' = (resolve_column ~env col).attr.meta in
  let rec aux = function 
  | Param (p, m) -> Param (p, Meta.merge_right m' m)  
  | Inparam (p, m) -> Inparam (p, Meta.merge_right m' m)
  | OptionActions { choice; pos; kind } -> 
    OptionActions { choice = aux choice; pos; kind }
  | e -> e 
  in
  aux e

let resolve_column_assignments ~env l =
  let open Schema.Source in 
  let open Attr in
  let all = all_tbl_columns (List.map (fun (a, b) -> a, (List.map (fun attr -> {sources=[a]; attr}) b)) env.tables) in
  let env = { env with schema = all } in
  l |> List.map begin fun (col,expr) ->
    let attr = resolve_column ~env col in
    (* autoincrement is special - nullable on insert, strict otherwise *)
    let typ = if Constraints.mem Autoincrement attr.attr.extra then 
      Sql.Type.nullable attr.attr.domain.t else attr.attr.domain in
    if !debug then eprintfn "column assignment %s type %s" col.cname (Type.show typ);
    (* add equality on param and column type *)
    let equality typ expr = Fun { kind = (F (Var 0, [Var 0; Var 0])); parameters = [Value typ; expr]; is_over_clause = false } in
    let with_default assign = if not @@ Constraints.mem WithDefault attr.attr.extra then fail "Column %s doesn't have default value" col.cname else assign in
    match expr with
    | RegularExpr (Choices (n,l)) ->
      Choices (n, List.map (fun (n,e) -> n, Option.map (equality typ) e) l) (* FIXME hack, should propagate properly *)
    | RegularExpr (OptionActions ch) ->
      OptionActions { ch with choice = (equality typ) ch.choice }  (* FIXME hack, should propagate properly *)
    | RegularExpr expr -> equality typ (set_param_meta ~env col expr)
    | WithDefaultParam (e, pos) -> with_default @@ OptionActions { choice = equality typ (set_param_meta ~env col e); pos; kind = SetDefault }
    | AssignDefault -> with_default @@ (Value typ)
  end

let get_columns_schema ~env l =
  let all = all_tbl_columns (List.map (fun (a, b) -> a, (List.map (fun attr -> {Schema.Source.Attr.sources=[a]; attr}) b)) env.tables) in
  (* FIXME col_name *)
  l |> List.map (fun col -> 
    let res = resolve_column ~env:{ env with schema = all } col in
    { res with attr = { res.attr with name = col.cname } })

let _print_env env =
  eprintfn "env: ";
  Sql.Schema.print @@ Schema.Source.from_schema env.schema;
  Tables.print stderr env.tables

let update_schema_with_aliases all_schema final_schema = 
  let applied = all_schema |> List.filter (fun s1 -> List.for_all Schema.Source.Attr.(fun s2 -> s2.attr.name <> s1.attr.name) final_schema) in  
  applied @ final_schema

let rec bool_choice_id = function
  | Column _
  | Inserted _
  | SelectExpr _
  | OptionActions _
  | Value _ -> None
  | Inparam (p, _)
  | Param (p, _) -> Some p.id
  | Fun { parameters; _ } -> List.find_map bool_choice_id parameters
  | Choices (p, _)
  | InTupleList { param_id = p; _ }
  | InChoice(p, _, _) -> Some p
  | Case { case; branches; else_ } ->
    List.find_map bool_choice_id 
    (option_list case @ 
     List.flatten (List.map (fun { Sql.when_; then_ } -> [when_; then_]) branches) @ 
     option_list else_)

let extract_meta_from_col ~env expr = 
  let rec aux = function 
    (* col_name = @param *)
    | Sql.Fun ({ parameters = ([Column a; b]); kind = Comparison; _ } as fn)
    (* col_name IN @param *)
    | Fun ({ parameters = ([Column a; (Inparam _) as b]); _ } as fn) -> 
      Fun { fn with parameters = [Column a; set_param_meta ~env a b] }
    | Sql.Fun ({ parameters = ([b; Column a]); kind = Comparison; _ } as fn)
    (* col_name IN @param *)
    | Fun ({ parameters = ([(Inparam _) as b; Column a;]); _ } as fn) -> 
      Fun { fn with parameters = [set_param_meta ~env a b; Column a;] }
    (* (col_name, ..., any_expr, col_name2) IN @param *)
    | InTupleList ({ exprs;_ } as in_tuple_list) -> 
      InTupleList { in_tuple_list with exprs = List.map aux exprs }
    | Fun ({ parameters; _ } as fn) -> 
      Fun { fn with parameters = List.map aux parameters }
    | Case { case; branches; else_ } ->
      let case = Option.map aux case in
      let branches = List.map (fun { Sql.when_; then_ } -> 
        { Sql.when_ = aux when_; then_ = aux then_ }
      ) branches in
      let else_ = Option.map aux else_ in
      Case { case; branches; else_ }
    | OptionActions ({ choice; _ } as o) -> 
      OptionActions { o with choice = aux choice; } 
    | InChoice (n, k, e) -> 
      InChoice (n, k, aux e)
    | Choices (n,l) -> 
      Choices (n, List.map (fun (n,e) -> n, Option.map aux e) l)
    | (Value _ | Param _ | Inparam _
      | SelectExpr (_, _) | Column _ | Inserted _) as e -> e
  in
  aux expr
    

(** resolve each name reference (Column, Inserted, etc) into ResValue or ResFun of corresponding type *)
let rec resolve_columns env expr =
  if !debug then
  begin
    eprintf "\nRESOLVE COLUMNS %s\n%!" (expr_to_string expr);
    eprintf "schema: "; Sql.Schema.print (Schema.Source.from_schema env.schema);
    Tables.print stderr env.tables;
  end;
  let expr = extract_meta_from_col ~env expr in
  let rec each e =
    match e with
    | Value x -> ResValue x
    | Column col -> ResValue (resolve_column ~env col).attr.domain
    | OptionActions { choice; pos; kind } ->
      let choice_id = match bool_choice_id choice with
      | Some choice_id -> choice_id
      | None -> 
        fail "BoolChoices expected a parameter, but isn't presented. Use regular Choices for this kind of logic"
      in
      ResOptionActions { res_choice = each choice; choice_id; pos; kind }
    | Inserted name ->
      let attr = try Schema.find env.insert_schema name with Schema.Error (_,s) -> fail "for inserted values : %s" s in
      ResValue attr.domain
    | Param (x, m) -> ResParam (x, m)
    | InTupleList { exprs; param_id; kind; pos } -> 
      let res_exprs = List.map (fun expr ->
        let res_expr = each expr in
        match res_expr with 
        | ResCase _
        | ResValue _
        | ResParam _
        | ResSelect _
        | ResFun _ -> res_expr
        | ResInparam _
        | ResChoices _
        | ResInTupleList _
        | ResOptionActions _
        | ResInChoice _ -> fail "unsupported expression %s kind for WHERE e IN @tuplelist" (show_res_expr res_expr)
      ) exprs in
      let res_exprs = List.map2 (fun e re ->
        match e with
        | Column col -> re, (resolve_column ~env col).attr.meta 
        | _ -> re, Meta.empty ()
      ) exprs res_exprs in
      ResInTupleList {param_id; res_in_tuple_list = Res res_exprs; kind; pos }
    | Inparam (x, m) -> ResInparam (x, m)
    | InChoice (n, k, x) -> ResInChoice (n, k, each x)
    | Choices (n,l) -> ResChoices (n, List.map (fun (n,e) -> n, Option.map each e) l)
    | Fun { kind; parameters; is_over_clause } ->
      ResFun { kind; parameters = List.map each parameters; is_over_clause }  
    | Case { case; branches; else_ } ->
      let case = Option.map each case in
      let branches = List.map (fun { Sql.when_; then_ } -> { when_ = each when_; then_ = each then_ }) branches in
      let else_ = Option.map each else_ in
      ResCase { case; branches; else_ }  
    | SelectExpr (select, usage) ->
      let (schema,p,_) = eval_select_full env select in
      let schema = Schema.Source.from_schema schema in
      (* represet nested selects as functions with sql parameters as function arguments, some hack *)
      match schema, usage with
      | [ {domain; _} ], `AsValue -> 
        (* This function should be raised? *)
        let rec with_count = function 
            | Case { case = _; branches; else_ } ->
              let then_exprs = List.map (fun b -> b.Sql.then_) branches in
              let all_results_exprs = then_exprs @ (option_list else_) in 
              List.find_map with_count all_results_exprs
            | Fun { kind = Agg Count; is_over_clause = false; _ }
            | SelectExpr (_, _) -> Some domain
            | Fun { parameters; is_over_clause = false; _ } -> List.find_map with_count parameters
            | Choices (_, chs) ->
              List.fold_left (fun acc (_, e) -> match acc with
                | None -> None
                | Some _ -> Option.map_default with_count None e
              ) (Some domain) chs
            | OptionActions { choice; _ } -> with_count choice  
            | Fun { is_over_clause = true; _ }
            | Value _| Param _| Inparam _ | InChoice _
            | Column _| Inserted _| InTupleList _ -> None
        in
        let default_null = Type.make_nullable domain in
        (* The only way to have a result in a subquery is to use the COUNT function wihout the HAVING expression. 
           Any other expression could possibly return no rows. *)
        let typ = match select.select_complete.select with 
        | ({ having = Some _; _ }, _) -> Type.nullable domain.t
        | ({ columns = [Expr(c, _)]; _ }, _) -> c |> with_count |> Option.default default_null
        | ({ columns = [_]; _ }, _) -> default_null
        | _ -> raise (Schema.Error (schema, "nested sub-select used as an expression returns more than one column"))
        in
        ResSelect (typ, p)
      | s, `AsValue -> raise (Schema.Error (s, "only one column allowed for SELECT operator in this expression"))
      | _, `Exists -> ResSelect (Type.depends Any, p)
  in
  each expr

(** assign types to parameters where possible *)
and assign_types env expr =
  let { set_tyvar_strict; _ } = env in
  let option_split = function None -> None, None | Some (x,y) -> Some x, Some y in
  let assign_params inferred x =
    let open Type in
    match x with
    | ResParam ({ id; typ; }, m) when is_any typ -> ResParam (new_param id inferred, m)
    | ResInparam ({ id; typ; }, m) when is_any typ -> ResInparam (new_param id inferred, m)
    | x -> x
  in
  let rec typeof_ (e:res_expr) = (* FIXME simplify *)
    match e with
    | ResValue t -> e, `Ok t
    | ResParam (p, _) -> e, `Ok p.typ
    | ResInparam (p, _) -> e, `Ok p.typ
    | ResSelect (t, _) -> e, `Ok t
    | ResOptionActions choice ->
      let (res_choice, t) = typeof choice.res_choice in
      let t =
        match Type.common_subtype [Type.depends Bool; get_or_failwith t] with
        | None -> `Error "no common subtype for ResOptionActions"
        | Some t -> `Ok t
      in
      ResOptionActions { choice with res_choice }, t
    | ResInTupleList { param_id; res_in_tuple_list; kind; pos } -> 
      (match res_in_tuple_list with 
      | Res res_exprs -> ResInTupleList { param_id; 
        res_in_tuple_list = ResTyped (List.map (fun (expr, meta) ->
          let typ = expr |> typeof |> snd |> get_or_failwith in 
          if Type.is_any typ then 
              fail "If you need to have a field as parameter in the left part you should specify a type"
          else typ, meta
        ) res_exprs); kind; pos }, `Ok (Type.strict Bool) 
      | ResTyped _ -> assert false
      )
    | ResInChoice (n, k, e) -> let e, t = typeof e in ResInChoice (n, k, e), t
    | ResChoices (n,l) ->
      let (e,t) = List.split @@ List.map (fun (_,e) -> option_split @@ Option.map typeof e) l in
      let t =
        match Type.common_subtype @@ List.map get_or_failwith @@ List.filter_map identity t with
        | None -> `Error "no common subtype for all choice branches"
        | Some t -> `Ok t
      in
      (* We can order by different columns with the different types *)
      let assign_any e = if env.is_order_by then e else assign_params (get_or_failwith t) e in
      ResChoices (n, List.map2 (fun (n,_) e -> n, (Option.map assign_any e)) l e), t
    | ResCase { case; branches; else_ } ->
      let (case_e, case_t) = option_split @@ Option.map typeof case in
      let (else_, else_t) = option_split @@ Option.map typeof else_ in
      let (whens_e, whens_t) = List.split @@ List.map (fun { when_; _ } -> typeof when_) branches in
      let (thens_e, thens_t) = List.split @@ List.map (fun { then_; _} -> typeof then_) branches in
      let whens_t =
        let types = List.map get_or_failwith @@ whens_t in
        match Type.common_supertype @@ Option.map_default get_or_failwith (Type.depends Bool) case_t :: types with
        | None -> failwith "no common supertype for all case when branches"
        | Some t -> t
      in
      let thens_t =
        let types = List.map get_or_failwith @@ thens_t in
        let is_exhausted = match whens_t.t with
          | Union { ctors; _ } -> 
            (* Since we have string literals, we can check if the enums are already exhausted or if a default case is required *)
            let values = Type.Enum_kind.Ctors.of_list @@ List.filter_map (function ResValue { t = StringLiteral v; _ } -> Some v | _ -> None) whens_e in
            Type.Enum_kind.Ctors.compare values ctors = 0
          | Unit _ | Int | Text | Blob | Float | Datetime | Decimal | Any | StringLiteral _ | Bool -> false in
        let exhaust_checked = if is_exhausted then types else List.map Type.make_nullable types in  
        match Type.common_supertype @@ Option.map_default (fun else_t -> types @ [get_or_failwith else_t]) exhaust_checked else_t with
        | None -> failwith "no common supertype for all case then branches"
        | Some t -> t
      in
      let thens_e = List.map (assign_params thens_t) thens_e in
      let whens_e = List.map (assign_params whens_t) whens_e in
      let else_ = Option.map (assign_params thens_t) else_ in
      let case = Option.map (assign_params whens_t) case_e in
      let branches = List.map2 (fun when_ then_ -> { when_; then_ }) whens_e thens_e in
      ResCase { case = case; branches; else_ = else_ }, `Ok thens_t
    | ResFun { kind; parameters; is_over_clause}  ->
        let open Type in
        let (params,types) = parameters |> List.map typeof |> List.split in
        let types = List.map get_or_failwith types in
        let show_func () =
          sprintf "%s applied to (%s)"
            (string_of_func kind)
            (String.concat ", " @@ List.map show types)
        in
        if !debug then eprintfn "func %s" (show_func ());
        let types_to_arg each_arg = List.map (const each_arg) types in
        let convert_args ret args = 
          let typevar = Hashtbl.create 10 in
          let resolved_typs = Hashtbl.create 10 in

          List.iteri (fun idx arg ->
            let typ = List.nth types idx in
            match arg with
            | Typ arg_ty ->
                begin match common_type arg_ty typ with
                | Some unified -> Hashtbl.add resolved_typs idx unified
                | None -> fail "types %s and %s do not match in %s" (show arg_ty) (show typ) (show_func ())
                end
            | Var i ->
                let var = Hashtbl.find_default typevar i typ in
                begin match common_type var typ with
                | Some t ->
                    if !debug then Type.(eprintfn "common_type %s %s = %s" (show var) (show typ) (show t));
                    Hashtbl.replace typevar i t
                | None ->
                    fail "types %s and %s for %s do not match in %s"
                      (show var) (show typ) (string_of_tyvar arg) (show_func ())
                end
          ) args;

          if !debug then
            Hashtbl.iter (fun i typ -> eprintfn "%s : %s" (string_of_tyvar (Var i)) (show typ)) typevar;

          let resolve_arg idx = function
            | Typ t -> Hashtbl.find_default resolved_typs idx t
            | Var i -> Hashtbl.find typevar i
          in
          let args = List.mapi resolve_arg args in
          let ret = match ret with
            | Typ t -> t
            | Var i -> Hashtbl.find typevar i
          in
          args, ret in

        (* With GROUP BY, the query returns no rows if no groups exist. With OVER clause, the query returns no rows if the outer query filter eliminates all rows.
           In both cases, if we're in a context that expects a value (like a subquery), the result should be nullable. *)
        let consider_agg_nullability typ = if (env.query_has_grouping || is_over_clause) && is_strict typ then typ else make_nullable typ in

        let rec infer_fn func types = match func, types with
        | Multi (ret, each_arg), t -> infer_fn (F (ret, types_to_arg each_arg)) t
        | Agg Count, ([] (* asterisk *) | [_]) -> strict Int, types
        | Agg Avg, [_] -> consider_agg_nullability @@ nullable Float, types
        | Agg Self, [typ] -> consider_agg_nullability typ, types
        | Agg _, _ -> fail "cannot use this grouping function with %d parameters" (List.length types)
        | F (_, args), _ when List.length args <> List.length types -> fail "wrong number of arguments : %s" (show_func ())
        | Coalesce (ret, each_arg) , _ -> 
          let args = types_to_arg each_arg in
          let args, ret = convert_args ret args in
          let has_one_strict = List.exists (fun arg ->
            match arg.nullability with 
            | Strict -> true | _ -> false
          ) types in
          let ret = if has_one_strict then
            { ret with nullability = Strict }
            else args |> common_nullability |> undepend ret in 
          ret , args
        | F (ret, args), _ ->
          let args, ret = convert_args ret args in
          let nullable = common_nullability args in
          undepend ret nullable, args
        | Ret t, _ when is_any t -> (* lame *)
          begin match common_supertype types with
          | Some t -> t, List.map (fun _ -> t) types
          | None -> { t = Any; nullability = common_nullability types }, types
          end
        | Ret ret, _ ->
          let nullability = common_nullability @@ ret :: types in (* remove this when subqueries are taken out separately *)
          { ret with nullability }, types (* ignoring arguments FIXME *)
        | Comparison, _  ->
          if set_tyvar_strict then 
            let args, ret = convert_args (Typ (strict Bool)) [Var 0; Var 0] in
            ret, List.map make_strict args 
          else 
            let args, ret = convert_args (Typ (depends Bool)) [Var 0; Var 0] in
            let nullable = common_nullability args in
            undepend ret nullable, args
        in
        let (ret,inferred_params) = infer_fn kind types in
        ResFun { kind; parameters = (List.map2 assign_params inferred_params params); is_over_clause }, `Ok ret
  and typeof expr =
    let r = typeof_ expr in
    if !debug then eprintfn "%s is typeof %s" (Type.show @@ get_or_failwith @@ snd r) (show_res_expr @@ fst r);
    r
  in
  typeof expr

and resolve_types env expr =
  let expr = expr |> resolve_columns env in
  try
    assign_types env expr
  with
    exn ->
      eprintfn "resolve_types failed with %s at:" (Printexc.to_string exn);
      eprintfn "%s" (show_res_expr expr);
      raise exn

and infer_schema env columns =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let rec propagate_meta ~env = function
    | Column col -> 
      let result = resolve_column ~env col in 
      result.attr.meta
    (* aggregated columns, ie: max, min *)
    | Fun { kind = Agg Self; parameters = [e]; _ } -> propagate_meta ~env e
    (* Or for subselect which always requests only one column, TODO: consider CTE in subselect, perhaps a rare occurrence *)
    | SelectExpr ({ select_complete = { select = ({columns = [Expr(e, _)]; from; _}, _); _ }; _ }, _) -> 
      let (env,_) = eval_nested env from in
      propagate_meta ~env e
    | Case _
    | Value _ 
    (* TODO: implement for custom props *) 
    | Param _ | Inparam _ | Choices _| InChoice _
    | Fun _ | SelectExpr _ | Inserted _ | InTupleList _
    | OptionActions _ -> Meta.empty ()
  in
  let resolve1 = function
    | All -> env.schema
    | AllOf t -> schema_of ~env t
    | Expr (e,name) ->
      let col =
        match e with
        | Column col -> resolve_column ~env col
        | e -> { 
          attr = unnamed_attribute ~meta:(propagate_meta ~env e) (resolve_types env e |> snd |> get_or_failwith);
          sources = [] 
        }
      in
      let col = Option.map_default (fun n -> {col with attr = { col.attr with name = n }}) col name in
      [ col ]
  in
  flat_map resolve1 columns

and get_params env e = e |> resolve_types env |> fst |> get_params_of_res_expr

(*
let _ =
  let e = Sub [Value Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Type.Int);] in
  e |> get_params |> to_string |> print_endline
*)

and get_params_of_columns env =
  let get = function
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params env e
  in
  flat_map get

and get_params_opt env = function
  | Some x -> get_params env x
  | None -> []

and get_params_l env l = flat_map (get_params env) l

and do_join (env,params) ((schema1,params1,_tables),join_type,join_cond) =
  let schema = Schema.Join.join join_type join_cond env.schema schema1 in
  let env = { env with schema } in
  let p = match join_cond with
  | Default | Natural | Using _ -> []
  | On e -> get_params { env with set_tyvar_strict = true } e (* TODO should use final schema (same as tables)? *)
  in
  env, params @ params1 @ p

and join env ((schema,p0,ts0),joins) =
  assert (env.schema = []);
  let all_tables = List.flatten (ts0 :: List.map (fun ((_,_,ts),_,_) -> ts) joins) in
  let env = { env with tables = env.tables @ all_tables; schema; } in
  List.fold_left do_join (env, p0) joins


and params_of_assigns env ss =
  let exprs = resolve_column_assignments ~env ss in
  get_params_l env exprs

and params_of_order order final_schema env =
  List.concat_map
    (fun (order, direction) ->
       let env = { env with schema = update_schema_with_aliases env.schema final_schema ;  } in
       let p1 = get_params_l { env with is_order_by = true } [ order ] in
       let p2 =
         match direction with
         | None | Some `Fixed -> []
         | Some (`Param p) -> [Choice (p,[Verbatim ("ASC","ASC");Verbatim ("DESC","DESC")])]
       in
       p1 @ p2)
    order

and ensure_res_expr = function
  | Value x -> ResValue x
  | Param (x, m) -> ResParam (x, m)
  | Inparam (x, m) -> ResInparam (x, m)
  | Case { case; branches; else_ }-> 
    let res_case = Option.map ensure_res_expr case in
    let res_branches = List.map (fun { Sql.when_; then_ } -> 
      { when_ = ensure_res_expr when_; then_ = ensure_res_expr then_ }
    ) branches in
    let res_else = Option.map ensure_res_expr else_ in
    ResCase { case = res_case; branches = res_branches; else_ = res_else }
  | InTupleList { param_id; _ } -> failed ~at:param_id.pos "ensure_res_expr InTupleList TBD"
  | Choices (p,_) -> failed ~at:p.pos "ensure_res_expr Choices TBD"
  | InChoice (p,_,_) -> failed ~at:p.pos "ensure_res_expr InChoice TBD"
  | Column _ | Inserted _ -> failwith "Not a simple expression"
  | Fun { kind; _ } when Type.is_grouping kind -> failwith "Grouping function not allowed in simple expression"
  | Fun { kind; parameters; is_over_clause } ->
     ResFun { kind; parameters = List.map ensure_res_expr parameters; is_over_clause } (* FIXME *)
  | SelectExpr _ -> failwith "not implemented : ensure_res_expr for SELECT"
  | OptionActions _ -> failwith  "BoolChoice is used in WHERE expr only"

and eval_nested env nested =
  (* nested selects generate new fresh schema in scope, cannot refer to outer schema,
    but can refer to attributes of tables through `tables` *)
  let env = { env with schema = [] } in
  (* FIXME resolved table schema depends on join (nullability with left), this is resolving too early *)
  match nested with
  | Some (t,l) -> join env (resolve_source env t, List.map (fun (x,jt,jc) -> resolve_source env x, jt, jc) l)
  | None -> env, []

and eval_select env { columns; from; where; group; having; } =
  let (env,p2) = eval_nested env from in
  let env = { env with query_has_grouping = List.length group > 0 } in
  let cardinality =
    if from = None then (if where = None then `One else `Zero_one)
    else if group = [] && exists_grouping columns && not @@ exists_windowing columns then `One
    else `Nat
  in
  let final_schema = infer_schema env columns in
  (* use schema without aliases here *)
  let p1 = get_params_of_columns env columns in
  let env = { env with schema = make_unique (Schema.Join.cross env.schema final_schema) } in (* enrich schema in scope with aliases *)
  (* WHERE requires explicit column source when ambiguous fields are present *)
  let p3 = get_params_opt { env with set_tyvar_strict = true; 
    (* Aliases aren't available on the WHERE stage *)
    schema = List.filter (fun i -> i.Schema.Source.Attr.sources <> []) env.schema; 
  } where in
  (* ORDER BY, HAVING, GROUP BY allow have column without explicit referring to source if it's specified in SELECT *)
  let env = { env with schema = update_schema_with_aliases env.schema final_schema } in
  let p4 = get_params_l env group in
  let p5 = get_params_opt env having in
  (final_schema, p1 @ p2 @ p3 @ p4 @ p5, env, cardinality)

(** @return final schema, params and tables that can be referenced by outside scope *)
and resolve_source env (x, alias) =
  let resolve_schema_with_alias schema = begin match alias with 
    | Some { table_name; column_aliases = Some col_schema } -> 
      let schema = Schema.compound ((List.map (fun attr -> Schema.Source.Attr.{sources=[]; attr;})) col_schema) schema in
      schema, [table_name, Schema.Source.from_schema schema]
    | Some { table_name; column_aliases = None } -> 
      let schema = List.map (fun i -> { i with Schema.Source.Attr.sources = table_name :: i.Schema.Source.Attr.sources }) schema in
      schema, [table_name, Schema.Source.from_schema schema]
    | None -> schema, [] 
  end in
  match x with
  | `Select select ->
    let (s,p,_) = eval_select_full env select in
    let tbl_alias = Option.map (fun { table_name; _ } -> table_name) alias in
    let s = List.map (fun i -> { i with Schema.Source.Attr.sources = List.concat [option_list tbl_alias; i.Schema.Source.Attr.sources] }) s in
    let s, tables = resolve_schema_with_alias s in
    s, p, tables
  | `Nested from ->
    let (env,p) = eval_nested env (Some from) in
    let s = infer_schema env [All] in
    if alias <> None then failwith "No alias allowed on nested tables";
    s, p, env.tables
  | `Table s ->
    let (name,s) = Tables_with_derived.get ~env s in
    let alias = Option.map (fun { table_name; _ } -> table_name) alias in
    let sources = (name :: option_list alias) in
    let s3 = List.map (fun attr -> { Schema.Source.Attr.attr; sources }) s  in
    s3, [], List.map (fun name -> name, s) sources
  | `ValueRows { row_constructor_list; row_order; row_limit; } ->
    (* 
      The columns of the table output from VALUES have the implicitly 
      named columns column_0, column_1, column_2, and so on
      https://dev.mysql.com/doc/refman/8.4/en/values.html
    *)
    let exprs_to_cols = List.mapi (fun idx expr -> Expr (expr, Some (Printf.sprintf "column_%d" idx))) in
    let dummy_select exprs = { columns = exprs_to_cols exprs; from = None; where = None; group = []; having = None } in
    let (s, p, _) = match row_constructor_list with
      | RowExprList [] -> failwith "Each row of a VALUES clause must have at least one column"
      | RowExprList (exprs :: xs) ->
        let unions = List.map (fun exprs -> `Union, dummy_select exprs ) xs in
        let select = dummy_select exprs in
        let select_complete = { select = select, unions; order=row_order; limit=row_limit; } in
        eval_select_full env { select_complete; cte = None }
      | RowParam { id; types; values_start_pos } ->
        List.map (fun t -> { attr = make_attribute' "" t; Schema.Source.Attr.sources = []}) 
          types, [ TupleList (id, ValueRows { types; values_start_pos }) ], Stmt.Select `Nat
    in
    let s, tables = resolve_schema_with_alias s in
    s, p, tables

and eval_select_full env { select_complete; cte } =
  let ctes, p1 = Option.map_default eval_cte ([], []) cte in
  let env = { env with ctes = ctes @ env.ctes } in
  let (s1, p2, env, cardinality) = eval_select env (fst @@ select_complete.select) in
  eval_compound ~env:{ env with tables = env.tables; } (p1 @ p2, s1, cardinality, select_complete)

and eval_cte { cte_items; is_recursive } = 
  let open Schema.Source in
  List.fold_left begin fun (acc_ctes, acc_vars) cte ->
    let env = { empty_env with ctes = acc_ctes } in
    let tbl_name = make_table_name cte.cte_name in
    let a1 = List.map (fun attr -> Attr.{ sources = []; attr }) in
    let s1, p1, _kind =
      if is_recursive then 
      begin
        match cte.stmt with
        | CteInline ({ select = select, other; _ } as stmt_) ->
          let other = other |> List.map begin fun cmb ->
            match fst cmb with
            | #cte_supported_compound_op -> cmb
            | `Except | `Intersect ->
              fail "%s: Recursive table reference in EXCEPT or INTERSECT operand is not allowed in CTEs" cte.cte_name
          end
          in
          let stmt = { stmt_ with select = select, other } in
          let s1, p1, env, cardinality = eval_select env (fst stmt.select) in
          (* UNIONed fields access by alias to itself cte *)
          let s2 = Schema.compound (Option.map_default a1 s1 cte.cols) s1 in
          let a2 = from_schema s2 in
          eval_compound ~env:{ env with ctes = (tbl_name, a2) :: env.ctes } (p1, s1, cardinality, stmt) 
        | CteSharedQuery _ -> failwith "Recursive CTEs with shared query currently are not supported"
      end    
      else (
        match cte.stmt with
        | CteInline stmt ->
          let s1, p1, env, cardinality = eval_select env (fst stmt.select) in
          eval_compound ~env:{ env with tables = env.tables } (p1, s1, cardinality, stmt)
        | CteSharedQuery shared_query_name ->
          let (_, stmt) = Shared_queries.get shared_query_name.ref_name in
          let s1, p1, kind = eval_select_full env stmt in
          s1, [SharedVarsGroup (p1, shared_query_name)], kind
      )
    in  
    let s2 = Schema.compound (Option.map_default a1 s1 cte.cols) s1 in
    (tbl_name, from_schema s2) :: acc_ctes, acc_vars @ p1 end
  ([], []) cte_items  

and eval_compound ~env result = 
  let (p1, s1, cardinality, stmt) = result in
  let { select=(_select, other); order; limit; _; } = stmt in
  let other = List.map snd other in
  let (s2l, p2l) = List.split (List.map (fun (s,p,_,_) -> s,p) @@ List.map (eval_select env) other) in
  let cardinality = if other = [] then cardinality else `Nat in
  (* ignoring tables in compound statements - they cannot be used in ORDER BY *)
  let final_schema = List.fold_left Schema.compound s1 s2l in
  let p3 = params_of_order order final_schema env in
  let (p4,limit1) = match limit with Some (p,x) -> List.map (fun p -> Single (p, Meta.empty())) p, x | None -> [],false in
  (* Schema.check_unique schema; *)
  let cardinality =
    if limit1 && cardinality = `Nat then `Zero_one
    else cardinality in
  final_schema, ( p1 @ (List.flatten p2l) @ p3 @ p4 : var list), Stmt.Select cardinality  

let update_tables ~env sources ss w =
  let schema = Schema.cross_all @@ List.map (fun (s,_,_) -> s) sources in
  let p0 = List.flatten @@ List.map (fun (_,p,_) -> p) sources in
  let tables = List.flatten @@ List.map (fun (_,_,ts) -> ts) sources in (* TODO assert equal duplicates if not unique *)
  let result = get_columns_schema ~env:{ env with tables } (List.map fst ss) in
  let env = { empty_env with 
    tables; schema; insert_schema=Schema.Source.from_schema result; } in
  let p1 = params_of_assigns env ss in
  let p2 = get_params_opt { env with set_tyvar_strict = true } w in
  p0 @ p1 @ p2

let annotate_select select types =
  let (select1,compound) = select.select in
  let rec loop acc cols types =
    match cols, types with
    | [], [] -> List.rev acc
    | (All | AllOf _) :: _, _ -> failwith "Asterisk not supported"
    | Expr (e,name) :: cols, t :: types -> loop (Expr (Fun { kind = (F (Typ t, [Typ t])); parameters = [e]; is_over_clause = false}, name) :: acc) cols types
    | _, [] | [], _ -> failwith "Select cardinality doesn't match Insert"
  in
  { select with select = { select1 with columns = loop [] select1.columns types }, compound }

let rec eval (stmt:Sql.stmt) =
  let open Stmt in
  let open Schema.Source in
  let open Attr in
  match stmt with
  | Create (name,`Schema schema) ->
      Tables.add (name, schema);
      ([],[],Create name)
  | Create (name,`Select select) ->
      let (schema,params,_) = eval_select_full empty_env select in
      Tables.add (name, from_schema schema);
      ([],params,Create name)
  | Alter (name,actions) ->
      List.iter (function
      | `Add (col,pos) -> Tables.alter_add name col pos
      | `Drop col -> Tables.alter_drop name col
      | `Change (oldcol,col,pos) -> Tables.alter_change name oldcol col pos
      | `RenameColumn (oldcol,newcol) -> Tables.rename_column name oldcol newcol
      | `RenameTable new_name -> Tables.rename name new_name
      | `RenameIndex _ -> () (* indices are not tracked yet *)
      | `None -> ()) actions;
      ([],[],Alter [name])
  | Rename l ->
    List.iter (fun (o,n) -> Tables.rename o n) l;
    ([], [], Alter (List.map fst l)) (* to have sensible target for gen_xml *)
  | Drop name ->
      Tables.drop name;
      ([],[],Drop name)
  | CreateIndex (name,table,cols) ->
      Sql.Schema.project cols (Tables.get_schema table) |> ignore; (* just check *)
      [],[],CreateIndex name
  | Insert { target=table; action=`Values (names, values); on_duplicate; } ->
    let expect = values_or_all table names in
    let t = Tables.get_schema table in
    let schema = List.map (fun attr -> { sources=[table]; attr }) t in
    let env = { empty_env with tables = [Tables.get table]; schema ; insert_schema = expect; } in
    let params, inferred = match values with
    | None -> [], Some (Values, expect)
    | Some values ->
      let vl = List.map List.length values in
      let cl = List.length expect in
      if List.exists (fun n -> n <> cl) vl then
        fail "Expecting %u expressions in every VALUES tuple" cl;
      let assigns = values |>
        List.map begin fun tuple ->
          (* pair up columns with inserted values *)
          List.combine (List.map (fun a -> {cname=a.name; tname=None}) expect) tuple
        end
      in
      params_of_assigns env (List.concat assigns), None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Insert { target=table; action=`Param (names, param_id); on_duplicate; } ->
    let expect = values_or_all table names in
    let schema = List.map (fun attr -> { Schema.Source.Attr.sources=[table]; attr }) (Tables.get_schema table) in
    let env = { empty_env with tables = [Tables.get table]; schema; insert_schema = expect; } in
    let params = [ TupleList (param_id, Insertion expect) ] in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (None, table)
  | Insert { target=table; action=`Select (names, select); on_duplicate; } ->
    let expect = values_or_all table names in
    let env = { empty_env with tables = [Tables.get table]; 
      schema = List.map (fun attr -> { sources=[table]; attr }) (Tables.get_schema table); 
      insert_schema = expect;
    } in
    let select_complete = annotate_select select.select_complete (List.map (fun a -> a.domain) expect) in
    let select = { select with select_complete } in
    let (schema,params,_) = eval_select_full env select in
    ignore (Schema.compound
      ((List.map (fun attr -> {sources=[]; attr;})) expect)
      (List.map (fun {attr; _} -> {sources=[]; attr}) schema)); (* test equal types once more (not really needed) *)
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (None,table)
  | Insert { target=table; action=`Set ss; on_duplicate; } ->
    let expect = values_or_all table (Option.map (List.map (function ({cname; tname=None},_) -> cname | _ -> assert false)) ss) in
    let env = { empty_env with tables = [Tables.get table]; 
      schema = List.map (fun attr -> { sources=[table]; attr }) (Tables.get_schema table);
      insert_schema = expect;} in
    let (params,inferred) = match ss with
    | None -> [], Some (Assign, Tables.get_schema table)
    | Some ss -> params_of_assigns env ss, None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Delete (table, where) ->
    let t = Tables.get table in
    let p = get_params_opt { empty_env with tables=[t]; 
      schema=List.map (fun attr -> { Schema.Source.Attr.sources=[t |> fst]; attr }) (t |> snd); 
      insert_schema=[]; set_tyvar_strict = true } where in
    [], p, Delete [table]
  | DeleteMulti (targets, tables, where) ->
    (* use dummy columns to verify targets match the provided tables  *)
    let select = ({ columns = [All]; from = Some tables; where; group = []; having = None }, []) in
    let select_complete = { select; order = []; limit = None} in
    let _attrs, params, _ = eval_select_full empty_env {select_complete; cte=None } in
    [], params, Delete targets
  | Set (vars, stmt) ->
    let p =
      vars |> List.map (fun (_k,e) ->
        match e with
        | Column _ -> [] (* this is not column but some db-specific identifier *)
        | _ -> get_params_of_res_expr (ensure_res_expr e)) |> List.concat
    in
    begin match stmt with
    | None -> [], p, Other
    | Some stmt -> let (schema,p2,kind) = eval stmt in (schema, p @ p2, kind)
    end
  | Update (table,ss,w,o,lim) ->
    let f, s = Tables.get table in

    let r = List.map (fun attr -> {Schema.Source.Attr.attr; sources=[f] }) s in
    let params = update_tables ~env:empty_env [r,[],[(f, s)]] ss w in
    let env = { empty_env with schema = update_schema_with_aliases [] r } in
    let p3 = params_of_order o [] { env with tables = [(f, s)] } in
    [], params @ p3 @ (List.map (fun p -> Single (p, Meta.empty())) lim), Update (Some table)
  | UpdateMulti (tables,ss,w) ->
    let sources = List.map (fun src -> resolve_source empty_env ((`Nested src), None)) tables in
    let params = update_tables ~env:empty_env sources ss w in
    [], params, Update None
  | Select select -> 
    let (schema, a, b) = eval_select_full empty_env select in
    from_schema schema , a ,b
  | CreateRoutine (name,_,_) ->
    [], [], CreateRoutine name

(* FIXME unify each choice separately *)
let unify_params l =
  if !debug then l |> List.iter (fun p -> eprintfn "var %s" (show_var p));
  let h = Hashtbl.create 10 in
  let h_choices = Hashtbl.create 10 in
  let check_choice_name ~sharing_disabled p =
    match p.label with
    | None -> () (* unique *)
    | Some n when sharing_disabled && Hashtbl.mem h_choices n -> failed ~at:p.pos "sharing choices not implemented"
    | Some n -> Hashtbl.add h_choices n ()
  in
  let remember name t =
    match name with
    | None -> () (* anonymous ie non-shared *)
    | Some name ->
    match Hashtbl.find h name with
    | exception _ -> Hashtbl.add h name t
    | t' ->
    match Type.common_type t t' with
    | Some x ->
      if !debug then eprintfn "unify var %s %s %s => %s" name (Type.show t) (Type.show t') (Type.show x);
      Hashtbl.replace h name x
    | None -> fail "incompatible types for parameter %S : %s and %s" name (Type.show t) (Type.show t')
  in
  let rec traverse = function
  | Single ({ id; typ; }, _)
  | SingleIn ({ id; typ; _ }, _) -> remember id.label typ
  | SharedVarsGroup (vars, _)
  | ChoiceIn { vars; _ } -> List.iter traverse vars
  | OptionActionChoice (_, l, _, _) -> List.iter traverse l
  | Choice (p,l) -> check_choice_name ~sharing_disabled:true p; List.iter (function Simple (_,l) -> Option.may (List.iter traverse) l | Verbatim _ -> ()) l
  | TupleList _ -> ()
  in
  let rec map = function
  | Single ({ id; typ; }, m) ->
    let typ = match id.label with None -> typ | Some name -> try Hashtbl.find h name with _ -> assert false in
    Single (new_param id (Type.undepend typ Strict), m) (* if no other clues - input parameters are strict *)
  | SingleIn ({ id; typ; }, m) ->
    let typ = match id.label with None -> typ | Some name -> try Hashtbl.find h name with _ -> assert false in
    SingleIn (new_param id (Type.undepend typ Strict), m) (* if no other clues - input parameters are strict *)
  | ChoiceIn t -> ChoiceIn { t with vars = List.map map t.vars }
  | SharedVarsGroup (vars, pos) -> SharedVarsGroup (List.map map vars, pos)
  | OptionActionChoice (p, l, pos, kind) -> OptionActionChoice (p, (List.map map l), pos, kind)
  | Choice (p, l) -> Choice (p, List.map (function Simple (n,l) -> Simple (n, Option.map (List.map map) l) | Verbatim _ as v -> v) l)
  | TupleList _ as x -> x
  in
  List.iter traverse l;
  List.map map l

let is_alpha = function
| 'a'..'z' -> true
| 'A'..'Z' -> true
| _ -> false

let common_prefix = function
| [] -> 0
| x::_ as l ->
  let rec loop i =
    if String.length x <= i then i
    else
      if List.for_all (fun s -> i < String.length s && s.[i] = x.[i]) l then
        loop (i+1)
      else
        i
  in
  let i = loop 0 in
  (* do not allow empty names or starting not with alpha *)
  if List.exists (fun s -> i = String.length s || not (is_alpha s.[i])) l then 0 else i

(* fill inferred sql for VALUES or SET *)
let complete_sql kind sql =
  match kind with
  | Stmt.Insert (Some (kind,schema), _) ->
    let (pre,each,post) = match kind with
    | Values -> "(", (fun _ -> ""), ")"
    | Assign -> "", (fun name -> name ^" = "), ""
    in
    let module B = Buffer in
    let b = B.create 100 in
    B.add_string b sql;
    B.add_string b " ";
    B.add_string b pre;
    let params = ref [] in
    let first = common_prefix @@ List.map (fun attr -> attr.Sql.name) schema in
    schema |> List.iter (fun attr ->
      if !params <> [] then B.add_string b ",";
      let attr_ref_prefix = each attr.Sql.name in
      let attr_name = String.slice ~first attr.Sql.name in
      let attr_ref = "@" ^ attr_name in
      let pos_start = B.length b + String.length attr_ref_prefix in
      let pos_end = pos_start + String.length attr_ref in
      (* autoincrement is special - nullable on insert, strict otherwise *)
      let typ = if Constraints.mem Autoincrement attr.extra then Sql.Type.nullable attr.domain.t else attr.domain in
      let param = Single (new_param {label=Some attr_name; pos=(pos_start,pos_end)} typ, Meta.empty()) in
      B.add_string b attr_ref_prefix;
      B.add_string b attr_ref;
      tuck params param;
    );
    B.add_string b post;
    (B.contents b, List.rev !params)
  | _ -> (sql,[])

let parse sql =
  let open Parser in
  let { statement; dialect_features } = parse_stmt sql in
  let (schema,p1,kind) = eval statement in
  let (sql,p2) = complete_sql kind sql in
  (sql, schema, unify_params (p1 @ p2), kind, dialect_features)
  
let eval_select select_full =
  let (schema, p1, kind) = eval @@ Select select_full in
  (schema, unify_params p1, kind)
