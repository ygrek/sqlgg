(** SQL syntax and RA *)

open Stmt
open Prelude
open Sql

type expr = [ `Value of Type.t (** literal value *)
            | `Param of param
            | `Func of (Type.t * bool) * expr list (** return type, grouping, parameters *)
            | `Column of string * string option (** name, table *)
            ]
            deriving (Show)

type expr_q = [ `Value of Type.t (** literal value *)
            | `Param of param
            | `Func of (Type.t * bool) * expr_q list (** return type, grouping, parameters *)
            ]
            deriving (Show)

let expr_to_string = Show.show<expr>

type column =
  | All
  | AllOf of string
  | Expr of expr * string option (** name *)
  deriving (Show)

type columns = column list deriving (Show)

let collect f l = List.flatten (List.map f l)

(* FIXME *)
let schema_as_params = List.map (fun attr -> (Some attr.RA.name,(0,0)), Some attr.RA.domain)

(** replace every Column with Value of corresponding type *)
let resolve_columns tables joined_schema expr =
  let schema_of_table name = name |> Tables.get_from tables |> snd in
  let rec each e =
    match e with
    | `Value x -> `Value x
    | `Column (name,table) ->
      let attr = RA.Schema.find (Option.map_default schema_of_table joined_schema table) name in
      `Value attr.RA.domain
    | `Param x -> `Param x
    | `Func (r,l) -> `Func (r,(List.map each l))
  in
  each expr

(** assign types to parameters where possible *)
let assign_types expr =
  let rec typeof e = (* FIXME simplify *)
    match e with
    | `Value t -> e, t
    | `Func ((ret,g),l) ->
(** Assumption: sql functions/operators have type schema 'a -> ... -> 'a -> 'a -> 'b
    i.e. all parameters of some equal type *)
        let (l,t) = l |> List.map typeof |> List.split in
        let t = match List.filter ((<>) Type.Any) t with
        | [] -> Type.Any
        | h::t -> if List.for_all ((=) h) t then h else Type.Any
        in
        let assign = function
        | `Param (n,Type.Any) -> `Param (n,t)
        | x -> x
        in
        let ret = if Type.Any <> ret then ret else t in
        `Func ((ret,g),(List.map assign l)),ret
    | `Param (_,t) -> e, t
  in
  typeof expr

let show_e e = Show.show<expr_q> (e) |> print_endline

let resolve_types tables joined_schema expr =
  let expr = resolve_columns tables joined_schema expr in
  if false then show_e expr;
  let (expr,_ as r) = assign_types expr in
  if false then print_newline @@ show_e expr;
  r

let infer_schema columns tables joined_schema =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let schema name = name |> Tables.get_from tables |> snd in
  let resolve1 = function
    | All -> joined_schema
    | AllOf t -> schema t
    | Expr (e,name) ->
      let col = begin
      match e with
      | `Column (name,Some t) -> RA.Schema.find (schema t) name
      | `Column (name,None) -> RA.Schema.find joined_schema name
      | _ -> RA.attr "" (resolve_types tables joined_schema e |> snd)
      end in
      let col = Option.map_default (fun n -> {col with RA.name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

let test_all_grouping columns =
  let test = function
  (* grouping function of zero or single parameter *)
  | Expr (`Func ((_,true),args),_) when List.length args <= 1 -> true 
  | _ -> false
  in
  List.for_all test columns

let test_all_const columns =
  let rec is_const = function
  | `Func (_,args) -> List.for_all is_const args
  | `Column _ -> false
  | _ -> true
  in
  let test = function
  | Expr (e,_) -> is_const e
  | _ -> false
  in
  List.for_all test columns

let get_params_q e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Func (_,l) -> List.fold_left loop acc l
    | `Value _ -> acc
  in
  loop [] e |> List.rev

let get_params tables joined_schema e =
  e |> resolve_types tables joined_schema |> fst |> get_params_q

(*
let _ =
  let e = Sub [Value Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Type.Int);] in
  e |> get_params |> to_string |> print_endline
*)

let params_of_column tables j_s = function
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params tables j_s e

let params_of_columns tables j_s = collect (params_of_column tables j_s)

let get_params_opt tables j_s = function
  | Some x -> get_params tables j_s x
  | None -> []

let get_params_l tables j_s l = collect (get_params tables j_s) l

let do_join (tables,params,schema) ((table1,params1),kind) =
  let (_,schema1) = table1 in
  let tables = tables @ [table1] in
  let schema = match kind with
  | `Cross
  | `Search _
  | `Default -> RA.Schema.cross schema schema1
  | `Natural -> RA.Schema.natural schema schema1
  | `Using l -> RA.Schema.join_using l schema schema1
  in
  let p = match kind with
  | `Cross | `Default | `Natural | `Using _ -> []
  | `Search e -> get_params tables schema e
  in
  tables,params @ params1 @ p , schema

let join ((t0,p0),joins) =
  let (tables,params,joined_schema) = List.fold_left do_join ([t0],p0,snd t0) joins in
(*   let joined_schema = tables |> List.map snd |> List.flatten in *)
  (tables,params,joined_schema)

let cross = List.fold_left RA.Schema.cross []

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
let all_columns = RA.Schema.make_unique $ cross
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
    let typ = (RA.Schema.find schema cname).RA.domain in
    exprs := (`Func ((Type.Any,false), [`Value typ;expr])) :: !exprs) l;
  (List.rev !cols, List.rev !exprs)

let params_of_assigns tables ss =
  let (_,exprs) = split_column_assignments tables ss in
  get_params_l tables (cross (List.map snd tables)) exprs

let params_of_order o final_schema tables =
  get_params_l tables (final_schema :: (List.map snd tables) |> all_columns) o

let rec ensure_simple_expr = function
  | `Value _ | `Param _ as x -> x
  | `Column _ -> failwith "Not a simple expression"
  | `Func ((_,grouping),_) when grouping -> failwith "Grouping function not allowed in simple expression"
  | `Func (x,l) -> `Func(x,List.map ensure_simple_expr l)

