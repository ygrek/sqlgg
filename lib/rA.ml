(*
 Relational Algebra
*)

open Printf
open ExtLib

module Type = Sql.Type

type attr = {name : string; domain : Type.t;}
  deriving (Show)

let attr n d = {name=n;domain=d}

module Schema =
struct
  type t = attr list
    deriving (Show)

  exception Error of t * string

  (** FIXME attribute case sensitivity? *)
  let by_name name = function attr -> attr.name = name
  let find_by_name t name = List.find_all (by_name name) t

  let find t name =
    match find_by_name t name with
    | [x] -> x
    | [] -> raise (Error (t,"missing attribute : " ^ name))
    | _ -> raise (Error (t,"duplicate attribute : " ^ name))

  let make_unique = List.unique ~cmp:(fun a1 a2 -> a1.name = a2.name && a1.name <> "")
  let is_unique t = List.length (make_unique t) = List.length t
  let check_unique t = is_unique t || raise (Error (t,"duplicate attributes"))

  let project names t = List.map (find t) names

  let change_inplace t before after =
    List.map (fun attr ->
      match by_name before attr with
      | true -> after
      | false -> attr ) t

  let cross t1 t2 = t1 @ t2

  (** [contains t attr] tests whether schema [t] contains attribute [attr] *)
  let contains t attr = find t attr.name = attr

  let check_contains t attr =
    if not (contains t attr) then
      raise (Error (t,"type mismatch for attribute " ^ attr.name))

  let sub l a = List.filter (fun x -> not (List.mem x a)) l

  let to_string v = v |> List.map (fun attr -> sprintf "%s %s" (Type.to_string attr.domain) attr.name) |>
    String.concat ", " |> sprintf "[%s]"
  let names t = t |> List.map (fun attr -> attr.name) |> String.concat "," |> sprintf "[%s]"

  let natural_ t1 t2 =
    let (common,t1only) = List.partition (fun x -> List.mem x t2) t1 in
    if 0 = List.length common then failwith "natural'";
    let t2only = sub t2 common in
    common @ t1only @ t2only

  let natural t1 t2 =
    try natural_ t1 t2 with
    | _ -> raise (Error (t1,"no common attributes for natural join of " ^ 
                             (names t1) ^ " and " ^ (names t2)))

  let join_using l t1 t2 =
    let common = List.map (find t1) l in
    List.iter (check_contains t2) common;
    common @ sub t1 common @ sub t2 common

  let check_types t1 t2 =
    List.iter2 (fun a1 a2 ->
      match a1.domain, a2.domain with
      | Type.Any, _
      | _, Type.Any -> ()
      | x, y when x = y -> ()
      | _ -> raise (Error (t1, sprintf "Atributes do not match : %s of type %s and %s of type %s" 
        a1.name (Type.to_string a1.domain)
        a2.name (Type.to_string a2.domain)))) t1 t2

  let check_types t1 t2 = 
    try check_types t1 t2 with 
    | List.Different_list_size _ -> raise (Error (t1, (to_string t1) ^ " differs in size to " ^ (to_string t2)))

  let compound t1 t2 = check_types t1 t2; t1

  let add t col pos =
    match find_by_name t col.name with
    | [] ->
      begin
      match pos with
      | `First -> col::t
      | `Default -> t @ [col]
      | `After name -> 
        try
          let (i,_) = List.findi (fun _ attr -> by_name name attr) t in
          let (l1,l2) = List.split_nth (i+1) t in
          l1 @ (col :: l2)
        with
          Not_found -> raise (Error (t,"Can't insert column " ^ col.name ^ " after non-existing column " ^ name))
      end
    | _ -> raise (Error (t,"Already has column " ^ col.name))

  let drop t col =
    ignore (find t col);
    List.remove_if (by_name col) t

  let change t oldcol col pos =
    match pos with
    | `Default -> change_inplace t oldcol col
    | `First | `After _ -> add (drop t oldcol) col pos

  let to_string x = Show.show<t>(x)
  let print x = prerr_endline (to_string x)

end

type table = string * Schema.t deriving (Show)

let print_table out (name,schema) =
  IO.write_line out name;
  schema |> List.iter (fun {name=name;domain=domain} ->
    IO.printf out "%10s %s\n" (Type.to_string domain) name);
  IO.write_line out ""

(*
open Schema

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)
