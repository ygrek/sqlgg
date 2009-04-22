(*
*)

(*open ListMore*)
open Printf
open Operators

module Type = Sql.Type

module Scheme =
struct

  type attr = {name : string; domain : Type.t;}
    deriving (Show)

  let attr n d = {name=n;domain=d}

  type t = attr list
    deriving (Show)

  exception Error of t * string

  let find t name =
    match List.find_all (fun attr -> attr.name = name) t with
    | [x] -> x
    | [] -> raise (Error (t,"missing attribute " ^ name))
    | _ -> raise (Error (t,"duplicate attribute " ^ name))

  let project names t = List.map (find t) names

  let rename t before after =
    List.map (fun attr -> 
      match attr.name with
      | x when x = before -> { attr with name=after }
      | _ -> attr ) t
    
  let cross t1 t2 = t1 @ t2

  let print v = print_endline (Show.show<t>(v))

(*
  let of_table t =
    List.map (fun col -> {Attr.name = None; orig = Some (col,t); domain = col.Col.sqltype}) t.Table.cols
*)
end

type table = string * Scheme.t deriving (Show)

let print_table t = print_endline (Show.show<table>(t))

(*
open Scheme

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)
