(** SQL syntax and RA *)

open Printf
open Prelude
open Sql

type env = { tables : Tables.table list; }

let empty_env = { tables = [] }

let collect f l = List.flatten (List.map f l)

(* FIXME *)
let schema_as_params = List.map (fun attr -> (Some attr.name,(0,0)), Some attr.domain)

let values_or_all table names =
  let schema = Tables.get_schema table in
  match names with
  | Some names -> Schema.project names schema
  | None -> schema

let list_filter_map = ExtList.List.filter_map

let show_expr_q e = Show.show<expr_q> (e)

let get_params_q e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Func (_,l) -> List.fold_left loop acc l
    | `Value _ -> acc
  in
  loop [] e |> List.rev

let test_all_grouping columns =
  let test = function
  (* grouping function of zero or single parameter *)
  | Expr (Fun (func,args,_),_) when Type.is_grouping func && List.length args <= 1 -> true
  | _ -> false
  in
  List.for_all test columns

let cross = List.fold_left Schema.cross []

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
let all_columns = Schema.make_unique $ cross
let all_tbl_columns = all_columns $ List.map snd

let split_column_assignments tables l =
  let cols = ref [] in
  let exprs = ref [] in
  let all = all_tbl_columns tables in
  List.iter (fun ((cname,tname as col),expr) ->
    cols := col :: !cols;
    let schema =
      match tname with
      | Some name -> Tables.get_from tables name |> snd
      | None -> all
    in
    (* hint expression to unify with the column type *)
    let typ = (Schema.find schema cname).domain in
    exprs := (Fun (Type.(Ret Any), [Value typ;expr], `None)) :: !exprs) l;
  (List.rev !cols, List.rev !exprs)

(** replace every Column with Value of corresponding type *)
let rec resolve_columns tables joined_schema expr =
(*
  eprintf "\nRESOLVE COLUMNS\n%s\n%!" (expr_to_string expr);
  Tables.print stderr tables;
  Sql.Schema.print joined_schema;
*)
  let schema_of_table name = name |> Tables.get_from tables |> snd in
  let rec each e =
    match e with
    | Value x -> `Value x
    | Column (name,table) ->
      let attr = Schema.find (Option.map_default schema_of_table joined_schema table) name in
      `Value attr.domain
    | Param x -> `Param x
    | Fun (r,l,select) ->
      let p = params_of_select {tables} select in
      `Func (r,p @ List.map each l)
  in
  each expr

(** assign types to parameters where possible *)
and assign_types expr =
  let rec typeof e = (* FIXME simplify *)
    match e with
    | `Value t -> e, t
    | `Param (_,t) -> e, t
    | `Func (func,params) ->
        let (params,types) = params |> List.map typeof |> List.split in
        let show () =
          sprintf "%s (%s)"
            (Type.string_of_func func)
            (String.concat ", " @@ List.map Type.to_string types)
        in
        let (ret,inferred_params) = match func, types with
        | Type.Agg, [typ] -> typ, types
        | Type.Group ret, [_] -> ret, types
        | (Type.Agg | Type.Group _), _ -> fail "cannot use grouping function with %d parameters" (List.length types)
        | Type.Func (ret,args), _ when List.length args = List.length types
                                    && List.fold_left (&&) true (List.map2 Type.matches args types) -> ret, args
        | Type.Func _, _ ->
          fail "types do not match : %s" (show ())
        | Type.Ret Type.Any, _ -> (* lame - make a best guest, return type same as for parameters *)
          begin match List.filter ((<>) Type.Any) types with
          | [] -> Type.Any, types
          | h::tl when List.for_all ((=) h) tl -> h, List.map (fun _ -> h) types
          | _ -> Type.Any, types
          end
        | Type.Ret ret, _ -> ret, types (* ignoring arguments FIXME *)
        | Type.Poly ret, _ ->
          match List.filter ((<>) Type.Any) types with
          | [] -> ret, types
          | h::tl when List.for_all ((=) h) tl -> ret, List.map (fun _ -> h) types
          | _ -> fail "all parameters should have same type : %s" (show ())
        in
        let assign inferred x =
          match x with
          | `Param (n,Type.Any) -> `Param (n, inferred)
          | x -> x
        in
        `Func (func,(List.map2 assign inferred_params params)), ret
  in
  typeof expr

and resolve_types tables joined_schema expr =
  let expr = resolve_columns tables joined_schema expr in
  try
    assign_types expr
  with
    exn ->
      printfn "resolve_types failed with %s at:" (Printexc.to_string exn);
      printfn "%s" (show_expr_q expr);
      raise exn

and infer_schema columns tables joined_schema =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let schema name = name |> Tables.get_from tables |> snd in
  let resolve1 = function
    | All -> joined_schema
    | AllOf t -> schema t
    | Expr (e,name) ->
      let col = begin
      match e with
      | Column (name,Some t) -> Schema.find (schema t) name
      | Column (name,None) -> Schema.find joined_schema name
      | _ -> attr "" (resolve_types tables joined_schema e |> snd)
      end in
      let col = Option.map_default (fun n -> {col with name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

and test_all_const columns =
  let rec is_const = function
  | Fun (_,args,`None) -> List.for_all is_const args
  | Fun (_,_,_) -> false (* FIXME ? *)
  | Column _ -> false
  | _ -> true
  in
  let test = function
  | Expr (e,_) -> is_const e
  | _ -> false
  in
  List.for_all test columns

and get_params tables joined_schema e =
  e |> resolve_types tables joined_schema |> fst |> get_params_q

(*
let _ =
  let e = Sub [Value Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Type.Int);] in
  e |> get_params |> to_string |> print_endline
*)

and params_of_columns tables j_s =
  let get tables j_s = function
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params tables j_s e
  in
  collect (get tables j_s)

and get_params_opt tables j_s = function
  | Some x -> get_params tables j_s x
  | None -> []

and get_params_l tables j_s l = collect (get_params tables j_s) l

and do_join env (tables,params,schema) ((table1,params1),kind) =
  let (_,schema1) = table1 in
  let tables = tables @ [table1] in
  let schema = match kind with
  | `Cross
  | `Search _
  | `Default -> Schema.cross schema schema1
  | `Natural -> Schema.natural schema schema1
  | `Using l -> Schema.join_using l schema schema1
  in
  let p = match kind with
  | `Cross | `Default | `Natural | `Using _ -> []
  | `Search e -> get_params env.tables schema e
  in
  tables,params @ params1 @ p , schema

and join env ((t0,p0),joins) =
  let all_tables = List.fold_left (fun acc ((table,_),_) -> table::acc) [t0] joins in
  let env = {tables = env.tables @ all_tables} in
  let (tables,params,joined_schema) = List.fold_left (do_join env) ([t0],p0,snd t0) joins in
(*   let joined_schema = tables |> List.map snd |> List.flatten in *)
  (tables,params,joined_schema)

and params_of_assigns tables ss =
  let (_,exprs) = split_column_assignments tables ss in
  get_params_l tables (cross (List.map snd tables)) exprs

and params_of_order o final_schema tables =
  get_params_l tables (final_schema :: (List.map snd tables) |> all_columns) o

and ensure_simple_expr = function
  | Value x -> `Value x
  | Param x -> `Param x
  | Column _ -> failwith "Not a simple expression"
  | Fun (func,_,_) when Type.is_grouping func -> failwith "Grouping function not allowed in simple expression"
  | Fun (x,l,`None) -> `Func (x,List.map ensure_simple_expr l) (* FIXME *)
  | Fun (_,_,_) -> failwith "not implemented : ensure_simple_expr with SELECT"

and eval_select env { columns; from; where; group; having; } =
  let (tbls,p2,joined_schema) =
    match from with
    | Some (t,l) -> join env (resolve_source env t, List.map (fun (x,k) -> resolve_source env x, k) l)
    | None -> [], [], []
  in
  let tbls = env.tables @ tbls in
  let singlerow = group = [] && test_all_grouping columns in
  let singlerow2 = where = None && group = [] && test_all_const columns in
  let p1 = params_of_columns tbls joined_schema columns in
  let p3 = get_params_opt tbls joined_schema where in
  let p4 = get_params_l tbls joined_schema group in
  let p5 = get_params_opt tbls joined_schema having in
  let cardinality = if singlerow then `One else
                    if singlerow2 then `Zero_one else `Nat in
  (infer_schema columns tbls joined_schema, p1 @ p2 @ p3 @ p4 @ p5, tbls, cardinality)

and resolve_source env (x,alias) =
  let src = match x with
  | `Select select -> let (s,p,_,_) = eval_select env select in ("",s), p
  | `Table s -> Tables.get s, []
  in
  match alias with
  | Some name -> let ((_,s),p) = src in ((name,s),p)
  | None -> src

and eval_select_full env (select,other,order,limit) =
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

and params_of_select env s =
  let make = List.map (fun x -> `Param x) in
  match s with
  | `None -> []
  | `Select s -> let (_,p,_) = eval_select_full env s in make p
  | `Single select ->
    match eval_select_full env select with
    | [_],p,_ -> make p
    | s,_,_ -> raise (Schema.Error (s,"only one column allowed for SELECT operator in this expression"))


let update_tables tables ss w =
  let (tables,params) = List.split tables in
  let p1 = params_of_assigns tables ss in
  let p2 = get_params_opt tables (all_tbl_columns tables) w in
  (List.flatten params) @ p1 @ p2

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
  | Insert (table,`Values (names, values)) ->
    let expect = values_or_all table names in
    let params, inferred = match values with
    | None -> [], Some (Values, expect)
    | Some values ->
      let vl = List.length values in
      let cl = List.length expect in
      if vl <> cl then
        fail "Expected %u expressions in VALUES list, %u provided" cl vl;
      let assigns = List.combine (List.map (fun a -> a.name, None) expect) values in
      params_of_assigns [Tables.get table] assigns, None
    in
    [], params, Insert (inferred,table)
  | Insert (table,`Select (names, select)) ->
    let (schema,params,_) = eval_select_full empty_env select in
    let expect = values_or_all table names in
    ignore (Schema.compound expect schema); (* test equal types *)
    [], params, Insert (None,table)
  | Insert (table, `Set ss) ->
    let (params,inferred) = match ss with
    | None -> [], Some (Assign, Tables.get_schema table)
    | Some ss -> params_of_assigns [Tables.get table] ss, None
    in
    [], params, Insert (inferred,table)
  | Delete (table, where) ->
    let t = Tables.get table in
    let p = get_params_opt [t] (snd t) where in
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
