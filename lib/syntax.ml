(** SQL syntax and RA *)

open Printf
open ExtLib
open Prelude
open Sql

let debug = ref false

type env = {
  tables : Tables.table list;
  schema : Schema.t;
  insert_schema : Schema.t;
}

(* expr with all name references resolved to values or "functions" *)
type res_expr =
  | ResValue of Type.t (** literal value *)
  | ResParam of param
  | ResInparam of param
  | ResChoices of param_id * res_expr choices
  | ResInChoice of param_id * [`In | `NotIn] * res_expr
  | ResFun of Type.func * res_expr list (** function kind (return type and flavor), arguments *)
  [@@deriving show]

let empty_env = { tables = []; schema = []; insert_schema = []; }

let flat_map f l = List.flatten (List.map f l)

let schema_of tables name = snd @@ Tables.get_from tables name

let get_or_failwith = function `Error s -> failwith s | `Ok t -> t

let values_or_all table names =
  let schema = Tables.get_schema table in
  match names with
  | Some names -> Schema.project names schema
  | None -> schema

let rec get_params_of_res_expr (e:res_expr) =
  let rec loop acc e =
    match e with
    | ResParam p -> Single p::acc
    | ResInparam p -> SingleIn p::acc
    | ResFun (_,l) -> List.fold_left loop acc l
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
| Inserted _ -> false
| Choices (p,l) ->
  begin match list_same @@ List.map (fun (_,expr) -> Option.map_default is_grouping false expr) l with
  | None -> failed ~at:p.pos "inconsistent grouping in choice branches"
  | Some v -> v
  end
| InChoice (_, _, e) -> is_grouping e
| Fun (func,args) ->
  (* grouping function of zero or single parameter or function on grouping result *)
  (Type.is_grouping func && List.length args <= 1) || List.exists is_grouping args

let exists_grouping columns =
  List.exists (function Expr (e,_) -> is_grouping e | All | AllOf _ -> false) columns

let cross = List.fold_left Schema.cross []

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
let all_columns = Schema.make_unique $ cross
let all_tbl_columns = all_columns $ List.map snd

let resolve_column tables schema {cname;tname} =
  Schema.find (Option.map_default (schema_of tables) schema tname) cname

(* HACK hint expression to unify with the column type *)
let rec hint attr expr =
  (* associate parameter with column *)
  let expr = match expr with Param p -> Param { p with attr = Some attr } | e -> e in
  (* go one level deep into choices *)
  match expr with
  | Choices (n,l) -> Choices (n, List.map (fun (n,e) -> n, Option.map (hint attr) e) l)
  | _ -> Fun (F (Var 0, [Var 0; Var 0]), [Value attr.domain;expr])

let resolve_column_assignments tables l =
  let all = all_tbl_columns tables in
  l |> List.map begin fun (col,expr) ->
    let attr = resolve_column tables all col in
    hint attr expr
  end

let get_columns_schema tables l =
  let all = all_tbl_columns tables in
  (* FIXME col_name *)
  l |> List.map (fun col -> { (resolve_column tables all col) with name = col.cname })

let _print_env env =
  eprintfn "env: ";
  Sql.Schema.print env.schema;
  Tables.print stderr env.tables

(** resolve each name reference (Column, Inserted, etc) into ResValue or ResFun of corresponding type *)
let rec resolve_columns env expr =
  if !debug then
  begin
    eprintf "\nRESOLVE COLUMNS %s\n%!" (expr_to_string expr);
    eprintf "schema: "; Sql.Schema.print env.schema;
    Tables.print stderr env.tables;
  end;
  let rec each e =
    match e with
    | Value x -> ResValue x
    | Column col -> ResValue (resolve_column env.tables env.schema col).domain
    | Inserted name ->
      let attr = try Schema.find env.insert_schema name with Schema.Error (_,s) -> fail "for inserted values : %s" s in
      ResValue attr.domain
    | Param x -> ResParam x
    | Inparam x -> ResInparam x
    | InChoice (n, k, x) -> ResInChoice (n, k, each x)
    | Choices (n,l) -> ResChoices (n, List.map (fun (n,e) -> n, Option.map each e) l)
    | Fun (r,l) ->
      ResFun (r,List.map each l)
    | SelectExpr (select, usage) ->
      let rec params_of_var = function
        | Single p -> [ResParam p]
        | SingleIn p -> [ResParam p]
        | ChoiceIn { vars; _ } -> as_params vars
        | Choice (_,l) -> l |> flat_map (function Simple (_, vars) -> Option.map_default as_params [] vars | Verbatim _ -> [])
        | TupleList (p, _) -> failed ~at:p.pos "FIXME TupleList in SELECT subquery"
      and as_params p = flat_map params_of_var p in
      let (schema,p,_) = eval_select_full env select in
      (* represet nested selects as functions with sql parameters as function arguments, some hack *)
      match schema, usage with
      | [ {domain;_} ], `AsValue ->
        ResFun (Type.Ret domain, as_params p)
      | s, `AsValue -> raise (Schema.Error (s, "only one column allowed for SELECT operator in this expression"))
      | _, `Exists -> ResFun (Type.Ret Any, as_params p)
  in
  each expr

(** assign types to parameters where possible *)
and assign_types expr =
  let option_split = function None -> None, None | Some (x,y) -> Some x, Some y in
  let rec typeof (e:res_expr) = (* FIXME simplify *)
    match e with
    | ResValue t -> e, `Ok t
    | ResParam p -> e, `Ok p.typ
    | ResInparam p -> e, `Ok p.typ
    | ResInChoice (n, k, e) -> let e, t = typeof e in ResInChoice (n, k, e), t
    | ResChoices (n,l) ->
      let (e,t) = List.split @@ List.map (fun (_,e) -> option_split @@ Option.map typeof e) l in
      let t =
        match List.map get_or_failwith @@ List.filter_map identity t with
        | [] -> assert false
        | t::ts -> List.fold_left (fun acc t -> match acc with None -> None | Some prev -> Type.common_subtype prev t) (Some t) ts
      in
      let t = match t with None -> `Error "no common subtype for all choice branches" | Some t -> `Ok t in
      ResChoices (n, List.map2 (fun (n,_) e -> n,e) l e), t
    | ResFun (func,params) ->
        let open Type in
        let (params,types) = params |> List.map typeof |> List.split in
        let types = List.map get_or_failwith types in
        let show () =
          sprintf "%s applied to (%s)"
            (string_of_func func)
            (String.concat ", " @@ List.map to_string types)
        in
        let func =
          match func with
          | Multi (ret,each_arg) -> F (ret, List.map (fun _ -> each_arg) types)
          | x -> x
        in
        let (ret,inferred_params) = match func, types with
        | Multi _, _ -> assert false (* rewritten into F above *)
        | Agg, [typ]
        | Group typ, _ -> typ, types
        | Agg, _ -> fail "cannot use this grouping function with %d parameters" (List.length types)
        | F (_, args), _ when List.length args <> List.length types -> fail "wrong number of arguments : %s" (show ())
        | F (ret, args), _ ->
          let typevar = Hashtbl.create 10 in
          let l = List.map2 begin fun arg typ ->
            match arg with
            | Typ arg -> common_type arg typ
            | Var i ->
              let arg =
                match Hashtbl.find typevar i with
                | exception Not_found -> Hashtbl.replace typevar i typ; typ
                | t -> t
              in
              (* prefer more precise type *)
              if arg = Type.Any then Hashtbl.replace typevar i typ;
              common_type arg typ
          end args types
          in
          let convert = function Typ t -> t | Var i -> Hashtbl.find typevar i in
          if List.fold_left (&&) true l then
            convert ret, List.map convert args
          else
            fail "types do not match : %s" (show ())
        | Ret Any, _ -> (* lame *)
          begin match List.filter ((<>) Any) types with
          | [] -> Any, types
          (* make a best guess, return type same as for parameters when all of single type *)
          | h::tl when List.for_all (matches h) tl -> h, List.map (fun _ -> h) types
          (* "expand" to floats, when all parameters numeric and above rule didn't match *)
          | l when List.for_all (function Int | Float -> true | _ -> false) l -> Float, List.map (function Any -> Float | x -> x) types
          | _ -> Any, types
          end
        | Ret ret, _ -> ret, types (* ignoring arguments FIXME *)
        in
        let assign inferred x =
          match x with
          | ResParam { id; typ = Any; attr; } -> ResParam (new_param ?attr id inferred)
          | ResInparam { id; typ = Any; attr; } -> ResInparam (new_param ?attr id inferred)
          | x -> x
        in
        ResFun (func,(List.map2 assign inferred_params params)), `Ok ret
  in
  typeof expr

and resolve_types env expr =
  let expr = resolve_columns env expr in
  try
    let (expr',t as r) = assign_types expr in
    if !debug then eprintf "resolved types %s : %s\n%!" (show_res_expr expr') (Type.to_string @@ get_or_failwith t);
    r
  with
    exn ->
      eprintfn "resolve_types failed with %s at:" (Printexc.to_string exn);
      eprintfn "%s" (show_res_expr expr);
      raise exn

and infer_schema env columns =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let resolve1 = function
    | All -> env.schema
    | AllOf t -> schema_of env.tables t
    | Expr (e,name) ->
      let col =
        match e with
        | Column col -> resolve_column env.tables env.schema col
        | _ -> make_attribute "" (resolve_types env e |> snd |> get_or_failwith) Constraints.empty
      in
      let col = Option.map_default (fun n -> {col with name = n}) col name in
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

and do_join (env,params) ((schema1,params1,_tables),kind) =
  let schema = match kind with
  | `Cross
  | `Search _
  | `Default -> Schema.cross env.schema schema1
  | `Natural -> Schema.natural env.schema schema1
  | `Using l -> Schema.join_using l env.schema schema1
  in
  let env = { env with schema } in
  let p = match kind with
  | `Cross | `Default | `Natural | `Using _ -> []
  | `Search e -> get_params env e (* TODO should use final schema (same as tables)? *)
  in
  env, params @ params1 @ p

and join env ((schema,p0,ts0),joins) =
  assert (env.schema = []);
  let all_tables = List.flatten (ts0 :: List.map (fun ((_,_,ts),_) -> ts) joins) in
  let env = { env with tables = env.tables @ all_tables; schema; } in
  List.fold_left do_join (env, p0) joins

and params_of_assigns env ss =
  let exprs = resolve_column_assignments env.tables ss in
  get_params_l env exprs

and params_of_order order final_schema tables =
  List.concat @@
  List.map
    (fun (order, direction) ->
       let env = { tables; schema=(final_schema :: (List.map snd tables) |> all_columns); insert_schema = []; } in
       let p1 = get_params_l env [ order ] in
       let p2 =
         match direction with
         | None | Some `Fixed -> []
         | Some (`Param p) -> [Choice (p,[Verbatim ("ASC","ASC");Verbatim ("DESC","DESC")])]
       in
       p1 @ p2)
    order

and ensure_res_expr = function
  | Value x -> ResValue x
  | Param x -> ResParam x
  | Inparam x -> ResInparam x
  | Choices (p,_) -> failed ~at:p.pos "ensure_res_expr Choices TBD"
  | InChoice (p,_,_) -> failed ~at:p.pos "ensure_res_expr InChoice TBD"
  | Column _ | Inserted _ -> failwith "Not a simple expression"
  | Fun (func,_) when Type.is_grouping func -> failwith "Grouping function not allowed in simple expression"
  | Fun (x,l) -> ResFun (x,List.map ensure_res_expr l) (* FIXME *)
  | SelectExpr _ -> failwith "not implemented : ensure_res_expr for SELECT"

and eval_nested env nested =
  (* nested selects generate new fresh schema in scope, cannot refer to outer schema,
    but can refer to attributes of tables through `tables` *)
  let env = { env with schema = [] } in
  match nested with
  | Some (t,l) -> join env (resolve_source env t, List.map (fun (x,k) -> resolve_source env x, k) l)
  | None -> env, []

and eval_select env { columns; from; where; group; having; } =
  let (env,p2) = eval_nested env from in
  let cardinality =
    if from = None then (if where = None then `One else `Zero_one)
    else if group = [] && exists_grouping columns then `One
    else `Nat
  in
  let final_schema = infer_schema env columns in
  (* use schema without aliases here *)
  let p1 = get_params_of_columns env columns in
  let env = Schema.{ env with schema = cross env.schema final_schema |> make_unique } in (* enrich schema in scope with aliases *)
  let p3 = get_params_opt env where in
  let p4 = get_params_l env group in
  let p5 = get_params_opt env having in
  (final_schema, p1 @ p2 @ p3 @ p4 @ p5, env.tables, cardinality)

(** @return final schema, params and tables that can be referenced by outside scope *)
and resolve_source env (x,alias) =
  match x with
  | `Select select ->
    let (s,p,_) = eval_select_full env select in
    s, p, (match alias with None -> [] | Some name -> [name,s])
  | `Nested s ->
    let (env,p) = eval_nested env (Some s) in
    let s = infer_schema env [All] in
    if alias <> None then failwith "No alias allowed on nested tables";
    s, p, env.tables
  | `Table s ->
    let (name,s) = Tables.get s in
    s, [], List.map (fun name -> name, s) (name :: option_list alias)

and eval_select_full env { select=(select,other); order; limit; } =
  let (s1,p1,tbls,cardinality) = eval_select env select in
  let (s2l,p2l) = List.split (List.map (fun (s,p,_,_) -> s,p) @@ List.map (eval_select env) other) in
  if false then
    eprintf "cardinality=%s other=%u\n%!"
            (Stmt.cardinality_to_string cardinality)
            (List.length other);
  let cardinality = if other = [] then cardinality else `Nat in
  (* ignoring tables in compound statements - they cannot be used in ORDER BY *)
  let final_schema = List.fold_left Schema.compound s1 s2l in
  let p3 = params_of_order order final_schema tbls in
  let (p4,limit1) = match limit with Some (p,x) -> List.map (fun p -> Single p) p, x | None -> [],false in
  (*                 Schema.check_unique schema; *)
  let cardinality =
    if limit1 && cardinality = `Nat then `Zero_one
                                    else cardinality in
  final_schema,(p1@(List.flatten p2l)@p3@p4 : var list), Stmt.Select cardinality


let update_tables sources ss w =
  let schema = cross @@ (List.map (fun (s,_,_) -> s) sources) in
  let p0 = List.flatten @@ List.map (fun (_,p,_) -> p) sources in
  let tables = List.flatten @@ List.map (fun (_,_,ts) -> ts) sources in (* TODO assert equal duplicates if not unique *)
  let env = { tables; schema; insert_schema=get_columns_schema tables (List.map fst ss); } in
  let p1 = params_of_assigns env ss in
  let p2 = get_params_opt env w in
  p0 @ p1 @ p2

let annotate_select select types =
  let (select1,compound) = select.select in
  let rec loop acc cols types =
    match cols, types with
    | [], [] -> List.rev acc
    | (All | AllOf _) :: _, _ -> failwith "Asterisk not supported"
    | Expr (e,name) :: cols, t :: types -> loop (Expr (Fun (F (Typ t, [Typ t]), [e]), name) :: acc) cols types
    | _, [] | [], _ -> failwith "Select cardinality doesn't match Insert"
  in
  { select with select = { select1 with columns = loop [] select1.columns types }, compound }

let eval (stmt:Sql.stmt) =
  let open Stmt in
  match stmt with
  | Create (name,`Schema schema) ->
      Tables.add (name,schema);
      ([],[],Create name)
  | Create (name,`Select select) ->
      let (schema,params,_) = eval_select_full empty_env select in
      Tables.add (name,schema);
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
    let env = { tables = [Tables.get table]; schema = Tables.get_schema table; insert_schema = expect; } in
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
          (* resolve DEFAULTs *)
          |> List.map (function (col,`Expr e) -> col, e | (col,`Default) -> col, Fun (Type.identity, [Column col]))
        end
      in
      params_of_assigns env (List.concat assigns), None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Insert { target=table; action=`Param (names, param_id); on_duplicate; } ->
    let expect = values_or_all table names in
    let env = { tables = [Tables.get table]; schema = Tables.get_schema table; insert_schema = expect; } in
    let params = [ TupleList (param_id, expect) ] in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (None, table)
  | Insert { target=table; action=`Select (names, select); on_duplicate; } ->
    let expect = values_or_all table names in
    let env = { tables = [Tables.get table]; schema = Tables.get_schema table; insert_schema = expect; } in
    let select = annotate_select select (List.map (fun a -> a.domain) expect) in
    let (schema,params,_) = eval_select_full env select in
    ignore (Schema.compound expect schema); (* test equal types once more (not really needed) *)
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (None,table)
  | Insert { target=table; action=`Set ss; on_duplicate; } ->
    let expect = values_or_all table (Option.map (List.map (function ({cname; tname=None},_) -> cname | _ -> assert false)) ss) in
    let env = { tables = [Tables.get table]; schema = Tables.get_schema table; insert_schema = expect; } in
    let (params,inferred) = match ss with
    | None -> [], Some (Assign, Tables.get_schema table)
    | Some ss -> params_of_assigns env ss, None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Delete (table, where) ->
    let t = Tables.get table in
    let p = get_params_opt { tables=[t]; schema=snd t; insert_schema=[]; } where in
    [], p, Delete [table]
  | DeleteMulti (targets, tables, where) ->
    (* use dummy columns to verify targets match the provided tables  *)
    let columns = List.map (fun tn -> AllOf tn) targets in
    let select = ({ columns; from = Some tables; where; group = []; having = None }, []) in
    let _attrs, params, _ = eval_select_full empty_env { select; order = []; limit = None } in
    [], params, Delete targets
  | Set (_name, e) ->
    let p = match e with
      | Column _ -> [] (* this is not column but some db-specific identifier *)
      | _ -> get_params_of_res_expr (ensure_res_expr e)
    in
    [], p, Other
  | Update (table,ss,w,o,lim) ->
    let t = Tables.get table in
    let params = update_tables [snd t,[],[t]] ss w in
    let p3 = params_of_order o [] [t] in
    [], params @ p3 @ (List.map (fun p -> Single p) lim), Update (Some table)
  | UpdateMulti (tables,ss,w) ->
    let sources = List.map (resolve_source empty_env) tables in
    let params = update_tables sources ss w in
    [], params, Update None
  | Select select -> eval_select_full empty_env select
  | CreateRoutine (name,ret,_) ->
    [], [], CreateRoutine (name, ret)

(* FIXME unify each choice separately *)
let unify_params l =
  let h = Hashtbl.create 10 in
  let h_choices = Hashtbl.create 10 in
  let check_choice_name p =
    match p.label with
    | None -> () (* unique *)
    | Some n when Hashtbl.mem h_choices n -> failed ~at:p.pos "sharing choices not implemented"
    | Some n -> Hashtbl.add h_choices n ()
  in
  let remember name t =
    match name with
    | None -> () (* anonymous ie non-shared *)
    | Some name ->
    match Hashtbl.find h name with
    | exception _ -> Hashtbl.add h name t
    | t' ->
    match Sql.Type.common_subtype t t' with
    | Some x -> Hashtbl.replace h name x
    | None -> fail "incompatible types for parameter %S : %s and %s" name (Type.show t) (Type.show t')
  in
  let rec traverse = function
  | Single { id; typ; attr=_ } -> remember id.label typ
  | SingleIn { id; typ; _ } -> remember id.label typ
  | ChoiceIn { vars; _ } -> List.iter traverse vars
  | Choice (p,l) -> check_choice_name p; List.iter (function Simple (_,l) -> Option.may (List.iter traverse) l | Verbatim _ -> ()) l
  | TupleList _ -> ()
  in
  let rec map = function
  | Single { id; typ; attr } -> Single (new_param id ?attr (match id.label with None -> typ | Some name -> try Hashtbl.find h name with _ -> assert false))
  | SingleIn { id; typ; attr } -> SingleIn (new_param id ?attr (match id.label with None -> typ | Some name -> try Hashtbl.find h name with _ -> assert false))
  | ChoiceIn t -> ChoiceIn { t with vars = List.map map t.vars }
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
      let param = Single (new_param ~attr {label=Some attr_name; pos=(pos_start,pos_end)} attr.domain) in
      B.add_string b attr_ref_prefix;
      B.add_string b attr_ref;
      tuck params param;
    );
    B.add_string b post;
    (B.contents b, List.rev !params)
  | _ -> (sql,[])

let parse sql =
  let (schema,p1,kind) = eval @@ Parser.parse_stmt sql in
  let (sql,p2) = complete_sql kind sql in
  (sql, schema, unify_params (p1 @ p2), kind)
