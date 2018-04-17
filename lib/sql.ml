(** *)

open Printf
open ExtLib

module Type =
struct
  type t = | Int | Text | Blob | Float | Bool | Datetime | Any
    [@@deriving show {with_path=false}]

  let to_string = show

  let matches x y =
    match x,y with
    | Any, _ | _, Any -> true
    | _ -> x = y

  let order x y =
    if x = y then
      `Equal
    else
      match x,y with
      | Any, t | t, Any -> `Order (t, Any)
      | Int, Float | Float, Int -> `Order (Int,Float)
      | Text, Blob | Blob, Text -> `Order (Text,Blob)
      | Int, Datetime | Datetime, Int -> `Order (Int,Datetime)
      | _ -> `No

  let common_type f x y =
    match order x y with
    | `Equal -> Some x
    | `Order p -> Some (f p)
    | `No -> None

  let common_supertype = common_type snd
  let common_subtype = common_type fst
  let common_type x y = Option.is_some @@ common_subtype x y

  type tyvar = Typ of t | Var of int
  let string_of_tyvar = function Typ t -> to_string t | Var i -> sprintf "'%c" (Char.chr @@ Char.code 'a' + i)

  type func =
  | Group of t * bool (* 'a -> t ; bool = multi-column *)
  | Agg (* 'a -> 'a *)
  | Poly of t (* 'a -> 'a -> t *) (* = F (Typ t, [Var 0; Var 0]) *)
  | Multi of tyvar * tyvar (* 'a -> ... -> 'a -> 'b *)
  | Ret of t (* _ -> t *) (* TODO eliminate *)
  | F of tyvar * tyvar list

  let fixed ret args = F (Typ ret, List.map (fun t -> Typ t) args)

  let pp_func pp =
    let open Format in
  function
  | Agg -> fprintf pp "|'a| -> 'a"
  | Group (ret,multi) -> fprintf pp "|%s'a| -> %s" (if multi then "{...} as " else "") (to_string ret)
  | Poly ret -> fprintf pp "'a -> 'a -> %s" (to_string ret)
  | Ret ret -> fprintf pp "_ -> %s" (to_string ret)
  | F (ret, args) -> fprintf pp "%s -> %s" (String.concat " -> " @@ List.map string_of_tyvar args) (string_of_tyvar ret)
  | Multi (ret, each_arg) -> fprintf pp "{ %s }+ -> %s" (string_of_tyvar each_arg) (string_of_tyvar ret)

  let string_of_func = Format.asprintf "%a" pp_func

  let is_grouping = function
  | Group _ | Agg -> true
  | Ret _ | Poly _ | F _ | Multi _ -> false
end

module Constraint =
struct
  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
    [@@deriving show {with_path=false}]

  type t = | PrimaryKey | NotNull | Unique | Autoincrement | OnConflict of conflict_algo
    [@@deriving show {with_path=false}]
end

type attr = {name : string; domain : Type.t;}
  [@@deriving show {with_path=false}]

let attr n d = {name=n;domain=d}

module Schema =
struct
  type t = attr list
    [@@deriving show]

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

  let to_string = show
  let print x = prerr_endline (to_string x)

end

type table = string * Schema.t [@@deriving show]
type schema = Schema.t

let print_table out (name,schema) =
  IO.write_line out name;
  schema |> List.iter (fun {name=name;domain=domain} ->
    IO.printf out "%10s %s\n" (Type.to_string domain) name);
  IO.write_line out ""

(** optional name and start/end position in string *)
type param_id = string option * (int * int) [@@deriving show]
type param = param_id * Type.t [@@deriving show]
type params = param list [@@deriving show]

let params_to_string = show_params

type alter_pos = [ `After of string | `Default | `First ]
type alter_action = [ `Add of attr * alter_pos | `Drop of string | `Change of string * attr * alter_pos | `None ]

type select_result = (schema * param list)

type int_or_param = [`Const of int | `Limit of param]
type limit_t = [ `Limit | `Offset ]
type col_name = {
  cname : string; (** column name *)
  tname : string option; (** table name *)
}
and limit = (param_id * Type.t) list * bool
and source1 = [ `Select of select | `Table of string ]
and source = source1 * string option
and join_cond = [ `Cross | `Search of expr | `Default | `Natural | `Using of string list ]
and select = {
  columns : column list;
  from : (source * (source * join_cond) list) option;
  where : expr option;
  group : expr list;
  having : expr option;
}
and select_full = {
  select : select * select list;
  order : expr list;
  limit : limit option;
}
and expr =
  | Value of Type.t (** literal value *)
  | Param of param
  | Fun of Type.func * expr list (** parameters *)
  | Select of select_full * bool (* single *)
  | Column of col_name
  | Inserted of string (** inserted value *)
and column =
  | All
  | AllOf of string
  | Expr of expr * string option (** name *)
  [@@deriving show {with_path=false}]

type columns = column list [@@deriving show]

type expr_q = [ `Value of Type.t (** literal value *)
            | `Param of param
            | `Func of Type.func * expr_q list (** return type, grouping, parameters *)
            ]
            [@@deriving show]

let expr_to_string = show_expr

type assignments = (col_name * expr) list

type insert_action =
{
  target : string;
  action : [ `Set of assignments option
           | `Values of (string list option * expr list list option) (* column names * list of value tuples *)
           | `Select of (string list option * select_full) ];
  on_duplicate : assignments option;
}

type stmt =
| Create of string * [ `Schema of schema | `Select of select_full ]
| Drop of string
| Alter of string * alter_action list
| CreateIndex of string * string * string list (* index name, table name, columns *)
| Insert of insert_action
| Delete of string * expr option
| Set of string * expr
| Update of string * assignments * expr option * expr list * param list (* where, order, limit *)
| UpdateMulti of source list * assignments * expr option
| Select of select_full
| CreateRoutine of string * Type.t option * (string * Type.t * expr option) list

(*
open Schema

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)

module SMap = Map.Make(String)

let functions = ref SMap.empty

let add_function k ret =
  let add map k v =
    let k = String.lowercase k in
    if SMap.mem k map then
      failwith (sprintf "Function %S already defined" k)
    else
      SMap.add k v map
  in
  functions := add !functions k ret

let get_function name =
  try
    SMap.find (String.lowercase name) !functions
  with
    Not_found -> failwith (sprintf "Unknown function %S" name)

let () =
  let module T = Type in
  let func ret l = List.iter (fun x -> add_function x ret) l in
  func T.Agg ["max";"min";"sum"]; (* TODO in sqlite3 min(a,b) acts as least(a,b), while min(a) is grouping *)
  func T.(Group (Int,true)) ["count"];
  func T.(Group (Float,false)) ["avg"];
  func T.(fixed Text [Text;Text]) ["strftime"];
  func T.(fixed Text [Text]) ["lower";"upper"];
  func T.(Ret Any) ["coalesce"];
  func T.(Ret Int) ["length"; "random";"unix_timestamp"];
  func T.(F (Var 0, [Var 0; Var 0])) ["nullif";"ifnull"];
  func T.(Multi (Var 0, Var 0)) ["least";"greatest"];
  func T.(Multi (Typ Text, Typ Text)) ["concat"];
  ()
