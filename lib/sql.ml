(** *)

open Printf
open ExtLib
open Prelude

module Type =
struct
  type kind =
    | Unit of [`Interval]
    | Int
    | Text
    | Blob
    | Float
    | Bool
    | Datetime
    | Decimal
    | Any (* FIXME - Top and Bottom ? *)
    [@@deriving eq, show{with_path=false}]
    (* TODO NULL is currently typed as Any? which actually is a misnormer *)

  type nullability =
  | Nullable (** can be NULL *)
  | Strict (** cannot be NULL *)
  | Depends (** unknown, to be determined *)
  [@@deriving eq, show{with_path=false}]

  type t = { t : kind; nullability : nullability; }[@@deriving eq, show{with_path=false}]

  let nullability nullability = fun t -> { t; nullability }
  let strict = nullability Strict
  let depends = nullability Depends
  let nullable = nullability Nullable
  let make_nullable { t; nullability=_ } = nullable t

  let make_strict { t; nullability=_ } = strict t

  let is_strict { nullability; _ } = nullability = Strict

  let (=) : t -> t -> bool = equal

  let show { t; nullability; } = show_kind t ^ (match nullability with Nullable -> "?" | Depends -> "??" | Strict -> "")
  let _ = pp
  let pp pf t = Format.pp_print_string pf (show t)

  let type_name t = show_kind t.t

  let is_any { t; nullability = _ } = equal_kind t Any

  let is_unit = function { t = Unit _; _ } -> true | _ -> false

  (** @return (subtype, supertype) *)
  let order_kind x y =
    if equal_kind x y then
      `Equal
    else
      match x,y with
      | Any, t | t, Any -> `Order (t,t)
      | Int, Float | Float, Int -> `Order (Int,Float)
      (* arbitrary decision : allow int<->decimal but require explicit cast for floats *)
      | Decimal, Int | Int, Decimal -> `Order (Int,Decimal)
      | Text, Blob | Blob, Text -> `Order (Text,Blob)
      | Int, Datetime | Datetime, Int -> `Order (Int,Datetime)
      | Text, Datetime | Datetime, Text -> `Order (Datetime,Text)
      | _ -> `No

  let order_nullability x y =
    match x,y with
    | Depends, Depends -> `Equal Depends
    | Nullable, Nullable -> `Equal Nullable
    | Strict, Strict -> `Equal Strict
    | Depends, n
    | n, Depends -> `Equal n (* Order ? *)
    | Strict, Nullable -> `Strict_Nullable
    | Nullable, Strict -> `Nullable_Strict

  let common_nullability = List.fold_left (fun acc t ->
    match acc, t.nullability with
    | _, Nullable
    | Nullable, _ -> Nullable
    | _, Strict
    | Strict, _ -> Strict
    | Depends, Depends -> Depends
    ) Depends

  let common_nullability l = match common_nullability l with Depends -> Strict | n -> n
  let undepend t nullability = if equal_nullability t.nullability Depends then { t with nullability } else t

  let common_type_ order x y =
    match order_nullability x.nullability y.nullability, order_kind x.t y.t with
    | _, `No -> None
    | `Equal nullability, `Order pair -> Some {t = order pair; nullability}
    | `Equal nullability, `Equal -> Some { x with nullability }
    | (`Nullable_Strict|`Strict_Nullable), `Equal -> Some (nullable x.t) (* FIXME need nullability order? *)
    | (`Nullable_Strict|`Strict_Nullable), `Order pair -> Some (nullable @@ order pair)

  let common_type_l_ order = function
  | [] -> None
  | t::ts -> List.fold_left (fun acc t -> match acc with None -> None | Some prev -> common_type_ order prev t) (Some t) ts

  let subtype = common_type_ fst
  let supertype = common_type_ snd
  let common_subtype = common_type_l_ fst
  let common_supertype = common_type_l_ snd

  let common_type = subtype

  let has_common_type x y = Option.is_some @@ subtype x y

  type tyvar = Typ of t | Var of int
  let string_of_tyvar = function Typ t -> show t | Var i -> sprintf "'%c" (Char.chr @@ Char.code 'a' + i)

  type agg_fun = Self | Count | Avg (* max, min | count | avg *)

  type func =
  | Agg of agg_fun (* 'a -> 'a | 'a -> t *)
  | Multi of tyvar * tyvar (* 'a -> ... -> 'a -> 'b *)
  | Coalesce of tyvar * tyvar
  | Comparison
  | Ret of t (* _ -> t *) (* TODO eliminate *)
  | F of tyvar * tyvar list

  let monomorphic ret args = F (Typ ret, List.map (fun t -> Typ t) args)
  let fixed ret args = monomorphic (depends ret) (List.map depends args)

  let identity = F (Var 0, [Var 0])

  let pp_func pp =
    let open Format in
  function
  | Agg Self -> fprintf pp "|'a| -> 'a"
  | Agg Avg -> fprintf pp "|'a| -> float"
  | Agg Count -> fprintf pp "|'a| -> int"
  | Ret ret -> fprintf pp "_ -> %s" (show ret)
  | F (ret, args) -> fprintf pp "%s -> %s" (String.concat " -> " @@ List.map string_of_tyvar args) (string_of_tyvar ret)
  | Multi (ret, each_arg) | Coalesce (ret, each_arg) -> fprintf pp "{ %s }+ -> %s" (string_of_tyvar each_arg) (string_of_tyvar ret)
  | Comparison -> fprintf pp "'a -> 'a -> %s" (show_kind Bool)

  let string_of_func = Format.asprintf "%a" pp_func

  let is_grouping = function
  | Agg _ -> true
  | Ret _ | F _ | Multi _ | Coalesce _  | Comparison -> false
end

module Constraint =
struct
  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
    [@@deriving show{with_path=false}, ord]

  type t = | PrimaryKey | NotNull | Null | Unique | Autoincrement | OnConflict of conflict_algo | WithDefault
    [@@deriving show{with_path=false}, ord]
end

module Constraints = struct
  include Set.Make(Constraint)
  let show s = [%derive.show: Constraint.t list] (elements s)
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

type attr = {name : string; domain : Type.t; extra : Constraints.t; }
  [@@deriving show {with_path=false}]

let make_attribute name kind extra =
  if Constraints.mem Null extra && Constraints.mem NotNull extra then fail "Column %s can be either NULL or NOT NULL, but not both" name;
  let domain = Type.{ t = Option.default Int kind; nullability = if List.exists (fun cstrt -> Constraints.mem cstrt extra) [NotNull; PrimaryKey]
    then Strict else Nullable } in
  {name;domain;extra}

let unnamed_attribute domain = {name="";domain;extra=Constraints.empty}

let make_attribute' ?(extra = Constraints.empty) name domain = { name; domain; extra }

module Schema =
struct
  type t = attr list
    [@@deriving show]

  exception Error of t * string

  module Source = struct
    module Attr = struct
      type 'a t = { attr: attr; sources: 'a list } [@@deriving show]

      let by_name name sattr = sattr.attr.name = name
    end

    type 'a t = 'a Attr.t list

    let find_by_name t name = List.find_all (Attr.by_name name) t

    let find t name =
      match find_by_name t name with
      | [x] -> x
      | [] -> raise (Error (List.map (fun i -> i.Attr.attr) t,"missing attribute : " ^ name))
      | _ -> raise (Error (List.map (fun i -> i.Attr.attr) t,"duplicate attribute : " ^ name))

    let mem_by_name t a =
      match find_by_name t a.Attr.attr.name with
      | [_] -> true
      | [] -> false
      | _ -> raise (Error (List.map (fun i -> i.Attr.attr) t,"duplicate attribute : " ^ a.attr.name))

    let sub_by_name l del = List.filter (fun x -> not (mem_by_name del x)) l

    let from_schema list = List.map (fun sattr -> sattr.Attr.attr) list
  end

  let raise_error t fmt = Printf.ksprintf (fun s -> raise (Error (t,s))) fmt

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

  let to_string v = v |> List.map (fun attr -> sprintf "%s %s" (Type.show attr.domain) attr.name) |>
    String.concat ", " |> sprintf "[%s]"
  let names t = t |> List.map (fun attr -> attr.name) |> String.concat "," |> sprintf "[%s]"

  module Join = struct

    type 'a condition = On of 'a | Default | Natural | Using of string list [@@deriving show]
    type typ = Left | Right | Full | Inner [@@deriving show]

    let cross t1 t2 = t1 @ t2

    (* TODO check that attribute types match (ignoring nullability)? *)
    let natural t1 t2 =
      let (common,t1only) = List.partition (fun a -> Source.mem_by_name t2 a) t1 in
      Source.Attr.(
        if 0 = List.length common then
          let t1_attrs = List.map (fun i -> i.attr) t1 in
          raise (Error (t1_attrs,"no common attributes for natural join of " ^
           (names (t1_attrs)) ^ " and " ^ (names (List.map (fun i -> i.attr) t2))))
      );
      common @ t1only @ Source.sub_by_name t2 common

    let using l t1 t2 =
      let common = List.map (Source.find t1) l in
      List.iter (fun a -> let _ = Source.find t2 a.Source.Attr.attr.name in ()) common;
      common @ Source.sub_by_name t1 common @ Source.sub_by_name t2 common

    let join typ cond a b =
      let nullable = List.map (fun data ->
        Source.Attr.{data with attr={data.attr with domain = Type.make_nullable data.attr.domain}}) in
      let action = match cond with Default | On _ -> cross | Natural -> natural | Using l -> using l in
      match typ with
      | Inner -> action a b
      | Left -> action a (nullable b)
      | Right -> action (nullable a) b
      | Full -> action (nullable a) (nullable b)

  end

  let cross_all l = List.fold_left Join.cross [] l

  let compound t1 t2 =
    let open Source in
    let open Attr in
    if List.length t1 <> List.length t2 then
      raise (Error (List.map (fun i -> i.attr) t1, (to_string (List.map (fun i -> i.attr) t1))
          ^ " differs in size to " ^ (to_string (List.map (fun i -> i.attr) t2))));
    let show_name i a =
      match a.name with
      | "" -> sprintf "column %d (of %d)" (i+1) (List.length t1)
      | s -> s
    in
    List.combine t1 t2
    |> List.mapi begin fun i (a1,a2) ->
      match Type.supertype a1.attr.domain a2.attr.domain with
      | Some t -> { a1 with attr = { a1.attr with domain=t } }
      | None -> raise (Error (List.map (fun i -> i.attr) t1, sprintf "Attributes do not match : %s of type %s and %s of type %s"
        (show_name i a1.attr) (Type.show a1.attr.domain)
        (show_name i a2.attr) (Type.show a2.attr.domain)))
    end

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
    IO.printf out "%10s %s %s\n" (Type.show domain) name (Constraints.show extra)
  end;
  IO.write_line out ""

(** optional name and start/end position in string *)
type param_id = { label : string option; pos : int * int; } [@@deriving show]
type param = { id : param_id; typ : Type.t; } [@@deriving show]
let new_param id typ = { id; typ; }
type params = param list [@@deriving show]
type ctor =
| Simple of param_id * var list option
| Verbatim of string * string
and var =
| Single of param
| SingleIn of param
| ChoiceIn of { param: param_id; kind : [`In | `NotIn]; vars: var list }
| Choice of param_id * ctor list
| TupleList of param_id * tuple_list_kind
and tuple_list_kind = Insertion of schema | Where_in of Type.t list
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

type cte_supported_compound_op = [ `Union | `Union_all ] [@@deriving show]

type compound_op = [ cte_supported_compound_op | `Except | `Intersect ] [@@deriving show]

type int_or_param = [`Const of int | `Limit of param]
type limit_t = [ `Limit | `Offset ]
type col_name = {
  cname : string; (** column name *)
  tname : table_name option;
}
and limit = param list * bool
and nested = source * (source * Schema.Join.typ * join_condition) list
and source = [ `Select of select_full | `Table of table_name | `Nested of nested ] * table_name option (* alias *)
and join_condition = expr Schema.Join.condition
and select = {
  columns : column list;
  from : nested option;
  where : expr option;
  group : expr list;
  having : expr option;
}
and cte_item = { cte_name: string; cols: schema option; stmt: select_complete; }
and cte = { cte_items: cte_item list; is_recursive: bool; }
and select_complete = {
  select : select * (compound_op * select) list;
  order : order;
  limit : limit option;
}
and select_full = { select_complete: select_complete; cte: cte option; }
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
  | InTupleList of expr list * param_id
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
| Set of (string * expr) list * stmt option
| Update of table_name * assignments * expr option * order * param list (* where, order, limit *)
| UpdateMulti of nested list * assignments * expr option
| Select of select_full
| CreateRoutine of table_name * Type.kind option * (string * Type.kind * expr option) list (* table_name represents possibly namespaced function name *)

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
val add_multi: Type.func -> string -> unit
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

let sponge = let open Type in let any = depends Any in Multi (Typ any, Typ any)

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
let multi ~ret args name = add_multi Type.(Multi (ret, args)) name

end

let () =
  let open Type in
  let open Function in
  let (||>) x f = List.iter f x in
  let int = strict Int in
  let float = strict Float in
  let text = strict Text in
  let datetime = strict Datetime in
  let bool = strict Bool in
  "count" |> add 0 (Agg Count); (* count( * ) - asterisk is treated as no parameters in parser *)
  "count" |> add 1 (Agg Count);
  ["max";"min";"sum";] ||> add 1 (Agg Self);
  "avg" |> add 1 (Agg (Avg));
  ["max";"min"] ||> multi_polymorphic; (* sqlite3 *)
  ["lower";"upper";"unhex";"md5";"sha";"sha1";"sha2"; "trim"; "to_base64"] ||> monomorphic text [text];
  "hex" |> monomorphic text [int];
  "length" |> monomorphic int [text];
  ["random"] ||> monomorphic int [];
  "rand" |> monomorphic int [];
  "rand" |> monomorphic int [int];
  "floor" |> monomorphic int [float];
  "nullif" |> add 2 (F (Var 0 (* TODO nullable *), [Var 0; Var 0]));
  "ifnull" |> add 2 (F (Var 0, [Var 1; Var 0]));
  ["least";"greatest";] ||> multi_polymorphic;
  "strftime" |> exclude 1; (* requires at least 2 arguments *)
  ["concat";"concat_ws";"strftime"] ||> multi ~ret:(Typ text) (Typ text);
  "date" |> monomorphic datetime [datetime];
  "time" |> monomorphic text [datetime];
  "julianday" |> multi ~ret:(Typ float) (Typ text);
  "from_unixtime" |> monomorphic datetime [int];
  "from_unixtime" |> monomorphic text [int;text];
  ["pow"; "power"] ||> monomorphic float [float;int];
  "unix_timestamp" |> monomorphic int [];
  "unix_timestamp" |> monomorphic int [datetime];
  ["timestampdiff";"timestampadd"] ||> monomorphic int [strict @@ Unit `Interval;datetime;datetime];
  "any_value" |> add 1 (F (Var 0,[Var 0])); (* 'a -> 'a but not aggregate *)
  ["substring"; "sha2"] ||> monomorphic text [text; int];
  "substring" |> monomorphic text [text; int; int];
  "substring_index" |> monomorphic text [text; text; int];
  "last_insert_id" |> monomorphic int [];
  "last_insert_id" |> monomorphic int [int];
  add_multi Type.(Coalesce (Var 0, Var 0)) "coalesce";
  "uuid" |> monomorphic text [];
  "uuid_short" |> monomorphic int [];
  "is_uuid" |> monomorphic bool [text];
  ["date_add"; "date_sub"] ||> monomorphic datetime [datetime; datetime];
  "date_format" |> monomorphic text [datetime; text];
  "json_remove" |> multi ~ret:(Typ text) (Typ text);
  "json_array" |> multi ~ret:(Typ text) (Typ text);
  "json_set" |> add 3 (F (Typ text, [Typ text; Typ text; Var 0]));
  ()
