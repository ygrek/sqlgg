(* SQL syntax and RA *)

open Stmt
open Operators
open ListMore

type expr = [ `Value of Sql.Type.t
            | `Param of param
            | `Sub of expr list
            | `Column of string * string option (** name, table *)
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

let get_scheme columns tables =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let resolve1 = function
    | All -> all
    | AllOf t -> scheme t
    | Expr (e,name) -> 
      let col = begin 
      match e with
      | `Column (name,Some t) -> RA.Scheme.find (scheme t) name
      | `Column (name,None) -> RA.Scheme.find all name
      | _ -> RA.attr "" Sql.Type.Text (* some expression *)
      end in
      let col = Option.map_default (fun n -> {col with RA.name = n}) col name in
      [ col ]
  in
  collect resolve1 columns

let scheme_as_params = List.map (fun attr -> Named attr.RA.name, Some attr.RA.domain)

(** replace every Column with Value of corresponding type *)
let rebuild tables expr =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let rec each e =
    match e with
    | `Value x -> `Value x
    | `Column (name,x) ->
      let attr = RA.Scheme.find (Option.map_default scheme all x) name in
      `Value attr.RA.domain
    | `Param x -> `Param x
    | `Sub l -> `Sub (List.map each l)
  in
  each expr

(** assign types to parameters where possible *)
let assign_types expr =
  let rec typeof e =
    match e with
    | `Value t -> e, Some t
    | `Sub l -> 
        let t = match l >> List.map typeof >> List.map snd >> List.filter_valid with
        | [] -> None
        | h::t -> if List.for_all ((=) h) t then Some h else None
        in
        let assign = function
        | `Param (n,None) -> `Param (n,t)
        | x -> x
        in
        `Sub (List.map assign l), t
    | `Param (_,t) -> e, t
  in
  typeof expr >> fst

let get_params e =
  let rec loop acc e =
    match e with
    | `Param p -> p::acc
    | `Sub l -> List.fold_left loop acc l
    | `Column _ | `Value _ -> acc
  in
  loop [] e >> List.rev

let get_params tables e = 
  get_params (assign_types (rebuild tables e))
(*   e >> rebuild tables >> assign_types >> get_params *)

(*
let _ = 
  let e = Sub [Value Sql.Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Sql.Type.Int);] in
  e >> get_params >> to_string >> print_endline
*)

let params_of_column tables = function 
  | All | AllOf _ -> []
  | Expr (e,_) -> get_params tables e

let params_of_columns tables = collect (params_of_column tables)

let get_params_opt tables = function
  | Some x -> get_params tables x
  | None -> []

let get_params_l tables l = collect (get_params tables) l

