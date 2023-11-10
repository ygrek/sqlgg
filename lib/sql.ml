(** *)

open Printf
open ExtLib
open Prelude

module Type =
struct
  type t =
    | Unit of [`Interval]
    | Int
    | Text
    | Blob
    | Float
    | Bool
    | Datetime
    | Decimal
    | Any
    [@@deriving show {with_path=false}]

  let to_string = show

  let matches x y =
    match x,y with
    | Any, _ | _, Any -> true
    | _ -> x = y

  let is_unit = function Unit _ -> true | _ -> false

  let order x y =
    if x = y then
      `Equal
    else
      match x,y with
      | Any, t | t, Any -> `Order (t,Any)
      | Int, Float | Float, Int -> `Order (Int,Float)
      (* arbitrary decision : allow int<->decimal but require explicit cast for floats *)
      | Decimal, Int | Int, Decimal -> `Order (Int,Decimal)
      | Text, Blob | Blob, Text -> `Order (Text,Blob)
      | Int, Datetime | Datetime, Int -> `Order (Int,Datetime)
      | Text, Datetime | Datetime, Text -> `Order (Datetime,Text)
      | _ -> `No

  let common_type f x y =
    match order x y with
    | `Equal -> Some x
    | `Order p -> Some (f p)
    | `No -> None

  let common_supertype = common_type snd
  let common_subtype = common_type fst
  let common_type x y = Option.is_some @@ common_subtype x y

  type tyvar = Typ of t | Var of int | Nulable of tyvar
  
  let rec string_of_tyvar = function 
    | Typ t -> to_string t
    | Var i -> sprintf "'%c" (Char.chr @@ Char.code 'a' + i)
    | Nulable v -> string_of_tyvar v

  type func =
  | Group of t (* _ -> t *)
  | Agg (* 'a -> 'a *)
  | Multi of tyvar * tyvar (* 'a -> ... -> 'a -> 'b *)
  | Ret of t (* _ -> t *) (* TODO eliminate *)
  | F of tyvar * tyvar list

  let monomorphic ret args = F (Typ ret, List.map (fun t -> Typ t) args)
  let fixed = monomorphic

  let identity = F (Var 0, [Var 0])

  let pp_func pp =
    let open Format in
  function
  | Agg -> fprintf pp "|'a| -> 'a"
  | Group ret -> fprintf pp "|_| -> %s" (to_string ret)
  | Ret ret -> fprintf pp "_ -> %s" (to_string ret)
  | F (ret, args) -> fprintf pp "%s -> %s" (String.concat " -> " @@ List.map string_of_tyvar args) (string_of_tyvar ret)
  | Multi (ret, each_arg) -> fprintf pp "{ %s }+ -> %s" (string_of_tyvar each_arg) (string_of_tyvar ret)

  let string_of_func = Format.asprintf "%a" pp_func

  let is_grouping = function
  | Group _ | Agg -> true
  | Ret _ | F _ | Multi _ -> false
end

module Constraint =
struct
  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
    [@@deriving show{with_path=false}, ord]

  type t = | PrimaryKey | NotNull | Null | Unique | Autoincrement | OnConflict of conflict_algo
    [@@deriving show{with_path=false}, ord]
end

module Constraints = struct
  include Set.Make(Constraint)
  let show s = [%derive.show: Constraint.t list] (elements s)
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

type attr = {name : string; domain : Type.t; extra : Constraints.t; }
  [@@deriving show {with_path=false}]

let make_attribute name domain extra =
  if Constraints.mem Null extra && Constraints.mem NotNull extra then fail "Column %s can be either NULL or NOT NULL, but not both" name;
  {name;domain;extra}

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
    ignore (find t before);
    List.map (fun attr ->
      match by_name before attr with
      | true -> after
      | false -> attr ) t

  let exists t name =
    match (find t name : attr) with
    | _ -> true
    | exception _ -> false

  let rename t oldname newname =
    if not (exists t oldname) then raise @@ Error (t, "no such column : " ^ oldname);
    if exists t newname then raise @@ Error (t, "column already exists : " ^ newname);
    List.map (fun attr -> if attr.name = oldname then { attr with name = newname } else attr) t

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

type table_name = { db : string option; tn : string } [@@deriving show]
let show_table_name { db; tn } = match db with Some db -> sprintf "%s.%s" db tn | None -> tn
let make_table_name ?db tn = { db; tn }
type schema = Schema.t [@@deriving show]
type table = table_name * schema [@@deriving show]

let print_table out (name,schema) =
  IO.write_line out (show_table_name name);
  schema |> List.iter begin fun {name;domain;extra} ->
    IO.printf out "%10s %s %s\n" (Type.to_string domain) name (Constraints.show extra)
  end;
  IO.write_line out ""

(** optional name and start/end position in string *)
type param_id = { label : string option; pos : int * int; } [@@deriving show]
type param = { id : param_id; typ : Type.t; attr : attr option; } [@@deriving show]
let new_param ?attr id typ = { id; typ; attr }
type params = param list [@@deriving show]
type ctor =
| Simple of param_id * var list option
| Verbatim of string * string
and var =
| Single of param
| SingleIn of param
| ChoiceIn of { param: param_id; kind : [`In | `NotIn]; vars: var list }
| Choice of param_id * ctor list
| TupleList of param_id * schema
[@@deriving show]
type vars = var list [@@deriving show]

type alter_pos = [ `After of string | `Default | `First ]
type alter_action = [
  | `Add of attr * alter_pos
  | `RenameTable of table_name
  | `RenameColumn of string * string
  | `RenameIndex of string * string
  | `Drop of string
  | `Change of string * attr * alter_pos
  | `None ]

type select_result = (schema * param list)

type direction = [ `Fixed | `Param of param_id ] [@@deriving show]

type int_or_param = [`Const of int | `Limit of param]
type limit_t = [ `Limit | `Offset ]
type col_name = {
  cname : string; (** column name *)
  tname : table_name option;
}
and limit = param list * bool
and nested = source * (source * join_cond) list
and source = [ `Select of select_full | `Table of table_name | `Nested of nested ] * table_name option (* alias *)
and join_cond = [ `Cross | `Search of expr | `Default | `Natural | `Using of string list ]
and select = {
  columns : column list;
  from : nested option;
  where : expr option;
  group : expr list;
  having : expr option;
}
and select_full = {
  select : select * select list;
  order : order;
  limit : limit option;
}
and order = (expr * direction option) list
and 'expr choices = (param_id * 'expr option) list
and expr =
  | Value of Type.t (** literal value *)
  | Param of param
  | Inparam of param
  | Choices of param_id * expr choices
  | InChoice of param_id * [`In | `NotIn] * expr
  | Fun of Type.func * expr list (** parameters *)
  | SelectExpr of select_full * [ `AsValue | `Exists ]
  | Column of col_name
  | Inserted of string (** inserted value *)
and column =
  | All
  | AllOf of table_name
  | Expr of expr * string option (** name *)
  [@@deriving show {with_path=false}]

type columns = column list [@@deriving show]

let expr_to_string = show_expr

type assignments = (col_name * expr) list

type insert_action =
{
  target : table_name;
  action : [ `Set of assignments option
           | `Values of (string list option * [ `Expr of expr | `Default ] list list option) (* column names * list of value tuples *)
           | `Param of (string list option * param_id)
           | `Select of (string list option * select_full) ];
  on_duplicate : assignments option;
}

type stmt =
| Create of table_name * [ `Schema of schema | `Select of select_full ]
| Drop of table_name
| Alter of table_name * alter_action list
| Rename of (table_name * table_name) list
| CreateIndex of string * table_name * string list (* index name, table name, columns *)
| Insert of insert_action
| Delete of table_name * expr option
| DeleteMulti of table_name list * nested * expr option
| Set of string * expr
| Update of table_name * assignments * expr option * order * param list (* where, order, limit *)
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

module Function : sig

val lookup : string -> int -> Type.func

val add : int -> Type.func -> string -> unit
val exclude : int -> string -> unit
val monomorphic : Type.t -> Type.t list -> string -> unit
val multi : ret:Type.tyvar -> Type.tyvar -> string -> unit
val multi_polymorphic : string -> unit

val multi_polymorphic_nullable : string -> unit
val sponge : Type.func

end = struct

let h = Hashtbl.create 10

let add_ narg typ name =
  let name = String.lowercase_ascii name in
  if Hashtbl.mem h (name,narg) then
    let func = match narg with None -> sprintf "%S" name | Some n -> sprintf "%S of %d arguments" name n in
    fail "Function %s already registered" func
  else
    Hashtbl.add h (name,narg) typ

let exclude narg name = add_ (Some narg) None name
let add_multi typ name = add_ None (Some typ) name
let add narg typ name = add_ (Some narg) (Some typ) name

let sponge = Type.(Multi (Typ Any, Typ Any))

let lookup name narg =
  let name = String.lowercase_ascii name in
  match Hashtbl.find h (name,Some narg) with
  | None ->
    eprintfn "W: wrong number of arguments for known function %S, treating as untyped" name;
    sponge
  | Some t -> t
  | exception _ ->
  match Hashtbl.find h (name,None) with
  | None -> assert false
  | Some t -> t
  | exception _ ->
    eprintfn "W: unknown function %S of %d arguments, treating as untyped" name narg;
    sponge

let monomorphic ret args name = add (List.length args) Type.(monomorphic ret args) name
let multi_polymorphic name = add_multi Type.(Multi (Var 0, Var 0)) name

let multi_polymorphic_nullable name = add_multi Type.(Multi (Nulable(Var 0), Var 0)) name

let multi ~ret args name = add_multi Type.(Multi (ret, args)) name

end

let () =
  let open Type in
  let open Function in
  let (||>) x f = List.iter f x in
  "count" |> add 0 (Group Int); (* count( * ) - asterisk is treated as no parameters in parser *)
  "count" |> add 1 (Group Int);
  "avg" |> add 1 (Group Float);
  ["max";"min";"sum"] ||> add 1 Agg;
  ["max";"min"] ||> multi_polymorphic; (* sqlite3 *)
  ["lower";"upper";"unhex";"md5";"sha";"sha1";"sha2"] ||> monomorphic Text [Text];
  "hex" |> monomorphic Text [Int];
  "length" |> monomorphic Int [Text];
  ["random"] ||> monomorphic Int [];
  "floor" |> monomorphic Int [Float];
  "nullif" |> add 2 (F (Var 0, [Var 0; Var 0]));
  "ifnull" |> add 2 (F (Var 0, [Nulable (Var 0); Var 0]));
  ["least";"greatest";] ||> multi_polymorphic;
  "coalesce" |> multi_polymorphic_nullable;
  "strftime" |> exclude 1; (* requires at least 2 arguments *)
  ["concat";"concat_ws";"strftime"] ||> multi ~ret:(Typ Text) (Typ Text);
  "date" |> monomorphic Datetime [Datetime];
  "time" |> monomorphic Text [Datetime];
  "julianday" |> multi ~ret:(Typ Float) (Typ Text);
  "from_unixtime" |> monomorphic Datetime [Int];
  "from_unixtime" |> monomorphic Text [Int;Text];
  ["pow"; "power"] ||> monomorphic Float [Float;Int];
  "unix_timestamp" |> monomorphic Int [];
  "unix_timestamp" |> monomorphic Int [Datetime];
  ["timestampdiff";"timestampadd"] ||> monomorphic Int [Unit `Interval;Datetime;Datetime];
  "any_value" |> add 1 (F (Var 0,[Var 0])); (* 'a -> 'a but not aggregate *)
  "substring" |> monomorphic Text [Text; Int];
  "substring" |> monomorphic Text [Text; Int; Int];
  "substring_index" |> monomorphic Text [Text; Text; Int];
  "last_insert_id" |> monomorphic Int [];
  "last_insert_id" |> monomorphic Int [Int];
  ()
