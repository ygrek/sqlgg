(** SQL syntax and RA *)

open Printf
open Prelude
open Sql

let debug = ref false

type env = {
  tables : Tables.table list;
  joined_schema : Schema.t;
  insert_schema : Schema.t;
}

let empty_env = { tables = []; joined_schema = []; insert_schema = []; }

let collect f l = List.flatten (List.map f l)

(* FIXME *)
let schema_as_params = List.map (fun attr -> (Some attr.name,(0,0)), Some attr.domain)

let schema_of tables name = snd @@ Tables.get_from tables name

let values_or_all table names =
  let schema = Tables.get_schema table in
  match names with
  | Some names -> Schema.project names schema
  | None -> schema

let list_filter_map = ExtList.List.filter_map

let get_params_q e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Func (_,l) -> List.fold_left loop acc l
    | `Value _ -> acc
  in
  loop [] e |> List.rev

let rec is_singular = function
| Value _
| Param _ -> true
| Column _ -> false
| Fun (func,args) ->
  (* grouping function of zero or single parameter or function of all singular values *)
  (Type.is_grouping func && List.length args <= 1) || List.for_all is_singular args
| Select _ -> false (* ? *)
| Inserted _ -> false (* ? *)

let test_all_grouping columns =
  List.for_all (function Expr (e,_) -> is_singular e | All | AllOf _ -> false) columns

let cross = List.fold_left Schema.cross []

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
let all_columns = Schema.make_unique $ cross
let all_tbl_columns = all_columns $ List.map snd

let resolve_column tables joined_schema {cname;tname} =
  Schema.find (Option.map_default (schema_of tables) joined_schema tname) cname

let resolve_column_assignments tables l =
  let all = all_tbl_columns tables in
  l |> List.map begin fun (col,expr) ->
    (* hint expression to unify with the column type *)
    let typ = (resolve_column tables all col).domain in
    Fun (Type.(Ret Any), [Value typ;expr])
  end

let get_columns_schema tables l =
  let all = all_tbl_columns tables in
  (* FIXME col_name *)
  l |> List.map (fun col -> { name = col.cname; domain = (resolve_column tables all col).domain; })

(** replace each name reference (Column, Inserted, etc) with Value of corresponding type *)
let rec resolve_columns env expr =
  if !debug then
  begin
    eprintf "\nRESOLVE COLUMNS %s\n%!" (expr_to_string expr);
    eprintf "schema: "; Sql.Schema.print env.joined_schema;
    Tables.print stderr env.tables;
  end;
  let rec each e =
    match e with
    | Value x -> `Value x
    | Column col -> `Value (resolve_column env.tables env.joined_schema col).domain
    | Inserted name ->
      let attr = try Schema.find env.insert_schema name with Schema.Error (_,s) -> fail "for inserted values : %s" s in
      `Value attr.domain
    | Param x -> `Param x
    | Fun (r,l) ->
      `Func (r,List.map each l)
    | Select (select,single) ->
      let as_params = List.map (fun x -> `Param x) in
      let (schema,p,_) = eval_select_full env select in
      match schema,single with
      | [ {domain;_} ], true -> `Func (Type.Ret domain, as_params p)
      | s, true -> raise (Schema.Error (s, "only one column allowed for SELECT operator in this expression"))
      | _ -> fail "not implemented: multi-column select in expression"
  in
  each expr

(** assign types to parameters where possible *)
and assign_types expr =
  let rec typeof (e:expr_q) = (* FIXME simplify *)
    match e with
    | `Value t -> e, t
    | `Param (_,t) -> e, t
    | `Func (func,params) ->
        let open Type in
        let (params,types) = params |> List.map typeof |> List.split in
        let show () =
          sprintf "%s applied to (%s)"
            (string_of_func func)
            (String.concat ", " @@ List.map to_string types)
        in
        let (ret,inferred_params) = match func, types with
        | Agg, [typ] -> typ, types
        | Group (ret,false), [_]
        | Group (ret,true), _ -> ret, types
        | (Agg | Group _), _ -> fail "cannot use this grouping function with %d parameters" (List.length types)
        | F (_, args), _ when List.length args <> List.length types -> fail "types do not match : %s" (show ())
        | F (ret, args), _ ->
          let typevar = Hashtbl.create 10 in
          let l = List.map2 begin fun arg typ ->
            match arg with
            | Typ arg -> matches arg typ
            | Var i ->
              let arg =
                match Hashtbl.find typevar i with
                | exception Not_found -> Hashtbl.replace typevar i typ; typ
                | t -> t
              in
              (* prefer more precise type *)
              if arg = Type.Any then Hashtbl.replace typevar i typ;
              matches arg typ
          end args types
          in
          let convert = function Typ t -> t | Var i -> Hashtbl.find typevar i in
          if List.fold_left (&&) true l then
            convert ret, List.map convert args
          else
            fail "types do not match : %s" (show ())
        | Ret Any, _ -> (* lame - make a best guess, return type same as for parameters *)
          begin match List.filter ((<>) Any) types with
          | [] -> Any, types
          | h::tl when List.for_all (matches h) tl -> h, List.map (fun _ -> h) types
          | _ -> Any, types
          end
        | Ret ret, _ -> ret, types (* ignoring arguments FIXME *)
        | Poly ret, _ ->
          match List.filter ((<>) Any) types with
          | [] -> ret, types
          | h::tl when List.for_all (matches h) tl -> ret, List.map (fun _ -> h) types
          | _ -> fail "all parameters should have same type : %s" (show ())
        in
        let assign inferred x =
          match x with
          | `Param (n,Any) -> `Param (n, inferred)
          | x -> x
        in
        `Func (func,(List.map2 assign inferred_params params)), ret
  in
  typeof expr

and resolve_types env expr =
  let expr = resolve_columns env expr in
  try
    let (expr',t as r) = assign_types expr in
    if !debug then eprintf "resolved types %s : %s\n%!" (show_expr_q expr') (Type.to_string t);
    r
  with
    exn ->
      eprintfn "resolve_types failed with %s at:" (Printexc.to_string exn);
      eprintfn "%s" (show_expr_q expr);
      raise exn

and infer_schema env columns =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let resolve1 = function
    | All -> env.joined_schema
    | AllOf t -> schema_of env.tables t
    | Expr (e,name) ->
      let col =
        match e with
        | Column col -> resolve_column env.tables env.joined_schema col
        | _ -> attr "" (resolve_types env e |> snd)
      in
      let col = Option.map_default (fun n -> {col with name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

and test_all_const columns =
  let rec is_const = function
  | Fun (_,args) -> List.for_all is_const args
  | Select _ -> false (* FIXME ? *)
  | Column _ -> false
  | _ -> true
  in
  let test = function
  | Expr (e,_) -> is_const e
  | _ -> false
  in
  List.for_all test columns

and get_params env e = e |> resolve_types env |> fst |> get_params_q

(*
let _ =
  let e = Sub [Value Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Type.Int);] in
  e |> get_params |> to_string |> print_endline
*)

and params_of_columns env =
  let get = function
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params env e
  in
  collect get

and get_params_opt env = function
  | Some x -> get_params env x
  | None -> []

and get_params_l env l = collect (get_params env) l

and do_join (env,params) (((_, schema1),params1),kind) =
  let joined_schema = match kind with
  | `Cross
  | `Search _
  | `Default -> Schema.cross env.joined_schema schema1
  | `Natural -> Schema.natural env.joined_schema schema1
  | `Using l -> Schema.join_using l env.joined_schema schema1
  in
  let env = { env with joined_schema } in
  let p = match kind with
  | `Cross | `Default | `Natural | `Using _ -> []
  | `Search e -> get_params env e (* TODO should use final schema (same as tables)? *)
  in
  env, params @ params1 @ p

and join env ((t0,p0),joins) =
  assert (env.joined_schema = []);
  let all_tables = List.fold_left (fun acc ((table,_),_) -> table::acc) [t0] joins in
  let env = { env with tables = env.tables @ all_tables; joined_schema = snd t0 } in
  List.fold_left do_join (env, p0) joins

and params_of_assigns env ss =
  let exprs = resolve_column_assignments env.tables ss in
  get_params_l env exprs

and params_of_order o final_schema tables =
  get_params_l { tables; joined_schema=(final_schema :: (List.map snd tables) |> all_columns); insert_schema = []; } o

and ensure_simple_expr = function
  | Value x -> `Value x
  | Param x -> `Param x
  | Column _ | Inserted _ -> failwith "Not a simple expression"
  | Fun (func,_) when Type.is_grouping func -> failwith "Grouping function not allowed in simple expression"
  | Fun (x,l) -> `Func (x,List.map ensure_simple_expr l) (* FIXME *)
  | Select _ -> failwith "not implemented : ensure_simple_expr for SELECT"

and eval_select env { columns; from; where; group; having; } =
  (* nested selects generate new fresh schema in scope, cannot refer to outer schema,
    but can refer to attributes of tables through `tables` *)
  let env = { env with joined_schema = [] } in
  let (env,p2) =
    match from with
    | Some (t,l) -> join env (resolve_source env t, List.map (fun (x,k) -> resolve_source env x, k) l)
    | None -> env, []
  in
  let singlerow = group = [] && test_all_grouping columns in
  let singlerow2 = where = None && group = [] && test_all_const columns in
  let p1 = params_of_columns env columns in
  let p3 = get_params_opt env where in
  let p4 = get_params_l env group in
  let p5 = get_params_opt env having in
  let cardinality = if singlerow then `One else
                    if singlerow2 then `Zero_one else `Nat in
  (infer_schema env columns, p1 @ p2 @ p3 @ p4 @ p5, env.tables, cardinality)

and resolve_source env (x,alias) =
  let src = match x with
  | `Select select -> let (s,p,_,_) = eval_select env select in ("",s), p
  | `Table s -> Tables.get s, []
  in
  match alias with
  | Some name -> let ((_,s),p) = src in ((name,s),p)
  | None -> src

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
  let (p4,limit1) = match limit with | Some x -> x | None -> [],false in
  (*                 Schema.check_unique schema; *)
  let cardinality =
    if limit1 && cardinality = `Nat then `Zero_one
                                    else cardinality in
  final_schema,(p1@(List.flatten p2l)@p3@p4), Stmt.Select cardinality


let update_tables tables ss w =
  let (tables,params) = List.split tables in
  let env = { tables; joined_schema=cross @@ List.map snd tables; insert_schema=get_columns_schema tables (List.map fst ss); } in
  let p1 = params_of_assigns env ss in
  let p2 = get_params_opt env w in
  (List.flatten params) @ p1 @ p2

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
      | `None -> ()) actions;
      ([],[],Alter name)
  | Drop name ->
      Tables.drop name;
      ([],[],Drop name)
  | CreateIndex (name,table,cols) ->
      Sql.Schema.project cols (Tables.get_schema table) |> ignore; (* just check *)
      [],[],CreateIndex name
  | Insert { target=table; action=`Values (names, values); on_duplicate; } ->
    let expect = values_or_all table names in
    let env = { tables = [Tables.get table]; joined_schema = expect; insert_schema = expect; } in
    let params, inferred = match values with
    | None -> [], Some (Values, expect)
    | Some values ->
      let vl = List.map List.length values in
      let cl = List.length expect in
      if List.exists (fun n -> n <> cl) vl then
        fail "Expecting %u expressions in every VALUES tuple" cl;
      let assigns = List.map (fun tuple -> List.combine (List.map (fun a -> {cname=a.name; tname=None}) expect) tuple) values in
      params_of_assigns env (List.concat assigns), None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Insert { target=table; action=`Select (names, select); on_duplicate; } ->
    let expect = values_or_all table names in
    let env = { tables = [Tables.get table]; joined_schema = expect; insert_schema = expect; } in
    let select = annotate_select select (List.map (fun a -> a.domain) expect) in
    let (schema,params,_) = eval_select_full env select in
    ignore (Schema.compound expect schema); (* test equal types once more (not really needed) *)
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (None,table)
  | Insert { target=table; action=`Set ss; on_duplicate; } ->
    let expect = values_or_all table (Option.map (List.map (function ({cname; tname=None},_) -> cname | _ -> assert false)) ss) in
    let env = { tables = [Tables.get table]; joined_schema = expect; insert_schema = expect; } in
    let (params,inferred) = match ss with
    | None -> [], Some (Assign, Tables.get_schema table)
    | Some ss -> params_of_assigns env ss, None
    in
    let params2 = params_of_assigns env (Option.default [] on_duplicate) in
    [], params @ params2, Insert (inferred,table)
  | Delete (table, where) ->
    let t = Tables.get table in
    let p = get_params_opt { tables=[t]; joined_schema=snd t; insert_schema=[]; } where in
    [], p, Delete table
  | Set (_name, e) ->
    let p = match e with
      | Column _ -> [] (* this is not column but some db-specific identifier *)
      | _ -> get_params_q (ensure_simple_expr e)
    in
    [], p, Other
  | Update (table,ss,w,o,lim) ->
    let params = update_tables [Tables.get table,[]] ss w in
    let p3 = params_of_order o [] [Tables.get table] in
    [], params @ p3 @ lim, Update (Some table)
  | UpdateMulti (tables,ss,w) ->
    let tables = List.map (resolve_source empty_env) tables in
    let params = update_tables tables ss w in
    [], params, Update None
  | Select select -> eval_select_full empty_env select
  | CreateRoutine _ ->
    [], [], Other
