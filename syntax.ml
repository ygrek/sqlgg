(* SQL syntax and RA *)

open Stmt
open Operators
open ListMore
open Apply

type expr = [ `Value of Sql.Type.t (** literal value *)
            | `Param of param
            | `Func of Sql.Type.t option * expr list (** return type, parameters *)
            | `Column of string * string option (** name, table *)
            ]
            deriving (Show)

type expr_q = [ `Value of Sql.Type.t (** literal value *)
            | `Param of param
            | `Func of Sql.Type.t option * expr_q list (** return type, parameters *)
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
  let schema_of_table name = name >> Tables.get_from tables >> snd in
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
    | `Value t -> e, Some t
    | `Func (ret,l) ->
(** Assumption: sql functions/operators have type schema 'a -> ... -> 'a -> 'a -> 'b
    i.e. all parameters of some equal type *)
        let (l,t) = l >> List.map typeof >> List.split in
        let t = match List.filter_valid t with
        | [] -> None
        | h::t -> if List.for_all ((=) h) t then Some h else None
        in
(*
        print_endline (Show.show<expr_q list>(l));
        print_endline (Show.show<Sql.Type.t option>(t));
*)
        let assign = function
        | `Param (n,None) -> `Param (n,t)
        | x -> x
        in
        let ret = if Option.is_some ret then ret else t in
        `Func (ret,(List.map assign l)),ret
    | `Param (_,t) -> e, t
  in
  typeof expr

let show_e e = Show.show<expr_q> (e) >> print_endline

let resolve_types tables joined_schema expr =
  expr
  >> resolve_columns tables joined_schema
(*   >> tee show_e  *)
  >> assign_types

let infer_schema columns tables joined_schema =
(*   let all = tables >> List.map snd >> List.flatten in *)
  let schema name = name >> Tables.get_from tables >> snd in
  let resolve1 = function
    | All -> joined_schema
    | AllOf t -> schema t
    | Expr (e,name) ->
      let col = begin
      match e with
      | `Column (name,Some t) -> RA.Schema.find (schema t) name
      | `Column (name,None) -> RA.Schema.find joined_schema name
      | _ -> RA.attr "" (Option.default Sql.Type.Text (resolve_types tables joined_schema e >> snd))
      end in
      let col = Option.map_default (fun n -> {col with RA.name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

let get_params e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Func (_,l) -> List.fold_left loop acc l
    | `Value _ -> acc
  in
  loop [] e >> List.rev

let get_params tables joined_schema e =
  e >> resolve_types tables joined_schema >> fst >> get_params

(*
let _ =
  let e = Sub [Value Sql.Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Sql.Type.Int);] in
  e >> get_params >> to_string >> print_endline
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
(*   let joined_schema = tables >> List.map snd >> List.flatten in *)
  (tables,params,joined_schema)

let split_column_assignments schema l =
  let cols = ref [] in
  let exprs = ref [] in
  List.iter (fun (col,expr) ->
    cols := col :: !cols;
    (* hint expression to unify with the column type *)
    let typ = (RA.Schema.find schema col).RA.domain in
    exprs := (`Func (None, [`Value typ;expr])) :: !exprs) l;
  (List.rev !cols,List.rev !exprs)

let params_of_assigns t ss =
  let (_,exprs) = split_column_assignments (snd t) ss in
  get_params_l [t] (snd t) exprs

