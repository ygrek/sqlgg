(* SQL syntax and RA *)

open Stmt
open Operators

type expr = | Value of Sql.Type.t
            | Param of param
            | Sub of expr list
            | Column of string * string option (** name, table *)
            deriving (Show)

let expr_to_string = Show.show<expr>

type column = 
  | All 
  | AllOf of string
  | Expr of expr * string option (** name *)
  deriving (Show)

type columns = column list deriving (Show)

let collect f l = List.flatten (List.map f l)

let get_scheme columns tables =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let resolve1 = function
    | All -> all
    | AllOf t -> scheme t
    | Expr (e,name) -> 
      let col = begin 
      match e with
      | Column (name,Some t) -> RA.Scheme.find (scheme t) name
      | Column (name,None) -> RA.Scheme.find all name
      | _ -> RA.attr "" Sql.Type.Text (* some expression *)
      end in
      let col = Option.map_default (fun n -> {col with RA.name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

let get_params e =
  let rec loop acc e =
    match e with
    | Param p -> p::acc
    | Sub l -> List.fold_left loop acc l
    | Column _ | Value _ -> acc
  in
  loop [] e >> List.rev

(*
let _ = 
  let e = Sub [Value Sql.Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Sql.Type.Int);] in
  e >> get_params >> to_string >> print_endline
*)

let params_of_column = function 
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params e

let params_of_columns = collect params_of_column

let get_params_opt = function
  | Some x -> get_params x
  | None -> []

let get_params_l = collect get_params

let scheme_as_params = List.map (fun attr -> Named attr.RA.name, Some attr.RA.domain)

(** replace every Column with Value of corresponding type *)
let rebuild expr tables =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let rec each e =
    match e with
    | Value _ -> e
    | Column (name,x) -> 
      let attr = RA.Scheme.find (Option.map_default scheme all x) name in
      Value attr.RA.domain
    | Param _ -> e
    | Sub l -> Sub (List.map each l)
  in
  each expr

(** assign types to parameters where possible *)
let assign_types expr =
  let rec typeof e =
    match e with
    | Column _ -> assert false; None
    | Value t -> Some t
    | Sub l -> 
        let l = List.map typeof l in
        List.fold_left 

