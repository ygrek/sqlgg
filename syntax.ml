(* SQL syntax and RA *)

open Stmt.Raw
open Operators

type expr = | Value of Sql.Type.t
            | Param of Stmt.Raw.param
            | Sub of expr list
            | Column of string * string option (** name, table *)
            deriving (Show)

type column = 
  | All 
  | AllOf of string
  | Expr of expr * string option (** name *)
  deriving (Show)

type columns = column list deriving (Show)

let resolve columns tables =
  let all = tables >> List.map snd >> List.flatten in
  let scheme name = name >> Tables.get_from tables >> snd in
  let resolve1 = function
    | All -> all
    | AllOf t -> scheme t
    | Expr (e,name) -> 
      let col = 
      begin match e with
      | Column (name,Some t) -> RA.Scheme.find (scheme t) name
      | Column (name,None) -> RA.Scheme.find all name
      | _ -> RA.Scheme.attr "" Sql.Type.Text
      end in
      let col = Option.map_default (fun n -> {col with RA.Scheme.name = n}) col name in
      [ col ]
  in
  columns >> List.map resolve1 >> List.flatten

