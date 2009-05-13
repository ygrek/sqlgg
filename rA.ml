(*
 Relational Algebra
*)

open ListMore
open Printf
open Operators

module Type = Sql.Type

type attr = {name : string; domain : Type.t;}
  deriving (Show)

let attr n d = {name=n;domain=d}

module Scheme =
struct
  type t = attr list
    deriving (Show)

  exception Error of t * string

  let find t name =
    match List.find_all (fun attr -> attr.name = name) t with
    | [x] -> x
    | [] -> raise (Error (t,"missing attribute : " ^ name))
    | _ -> raise (Error (t,"duplicate attribute : " ^ name))

  let is_unique t =
    let t1 = List.unique ~cmp:(fun a1 a2 -> a1.name = a2.name) t in
    List.length t1 = List.length t

  let check_unique t = is_unique t || raise (Error (t,"duplicate attributes"))

  let project names t = List.map (find t) names

  let rename t before after =
    List.map (fun attr ->
      match attr.name with
      | x when x = before -> { attr with name=after }
      | _ -> attr ) t

  let cross t1 t2 = t1 @ t2

  (** [contains t attr] tests whether schema [t] contains attribute [attr] *)
  let contains t attr = find t attr.name = attr

  let check_contains t attr =
    if not (contains t attr) then
      raise (Error (t,"type mismatch for attribute " ^ attr.name))

  let sub l a = List.filter (fun x -> not (List.mem x a)) l

  let natural t1 t2 =
    let (common,t1only) = List.partition (fun x -> List.mem x t2) t1 in
    let t2only = sub t2 common in
    common @ t1only @ t2only

  let join_using l t1 t2 =
    let common = List.map (find t1) l in
    List.iter (check_contains t2) common;
    common @ sub t1 common @ sub t2 common

  (* FIXME? should be less strict -- check only types *)
  let compound t1 t2 =
    if t1 <> t2 then
      raise (Error (t1, (Show.show<t>(t1)) ^ " not equal to " ^ (Show.show<t>(t2))))
    else
      t1

  let to_string x = Show.show<t>(x)
  let print x = print_endline (to_string x)

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
