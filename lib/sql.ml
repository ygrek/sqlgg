(** *)

open Printf
open ExtLib
open Prelude

type pos = (int * int) [@@deriving show]

type 'a located  = { value : 'a; pos : pos } [@@deriving show, make]
type 'a collated = { collated: 'a; collation: string located option } [@@deriving show, make]

module Type =
struct

  module Enum_kind = struct

    module Ctors =  struct 
      include Set.Make(String)
  
      let pp fmt s = 
        Format.fprintf fmt "{%s}" 
          (String.concat "; " (elements s))  
    end

    type t = Ctors.t [@@deriving eq, show{with_path=false}]

    let make ctors = Ctors.of_list ctors
  end

  type union = { ctors: Enum_kind.t; is_closed: bool } [@@deriving eq, show{with_path=false}]

  type decimal = { precision: int option; scale: int option } [@@deriving eq, show{with_path=false}]

  type kind =
    | Int
    | UInt64
    | Text
    | Blob
    | Float
    | Bool
    | Datetime
    | Decimal of decimal
    | Union of union
    | StringLiteral of string
    | FloatingLiteral of float
    | Json_path
    | One_or_all
    | Json
    | Any (* FIXME - Top and Bottom ? *)
    [@@deriving eq, show{with_path=false}]
    (* TODO NULL is currently typed as Any? which actually is a misnormer *)

    let show_kind = function
      | Union { ctors; _ } -> sprintf "Union (%s)" (String.concat "| " (Enum_kind.Ctors.elements ctors))
      | StringLiteral l -> sprintf "StringLiteral (%s)" l
      | FloatingLiteral f -> sprintf "FloatingLiteral (%g)" f
      | Decimal { precision = Some p; scale = Some s } -> sprintf "Decimal(%d,%d)" p s
      | Decimal { precision = Some p; scale = None } -> sprintf "Decimal(%d)" p
      | Decimal _ -> "Decimal"
      | k -> show_kind k

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
  
  let make_enum_kind ctors = Union { ctors = (Enum_kind.make ctors); is_closed = true }

  let is_strict { nullability; _ } = nullability = Strict

  let is_nullable { nullability; _ } = nullability = Nullable

  let (=) : t -> t -> bool = equal

  let show { t; nullability; } = show_kind t ^ (match nullability with Nullable -> "?" | Depends -> "??" | Strict -> "")
  let _ = pp
  let pp pf t = Format.pp_print_string pf (show t)

  let type_name t = show_kind t.t

  let is_any { t; nullability = _ } = equal_kind t Any

  let is_one_or_all s = List.mem (String.lowercase_ascii s) ["one"; "all"]

  let check_exact_exact_number value { precision; scale } =
  match precision, scale with
  | Some p, Some s ->
      let max =
        (10. ** float_of_int (p - s)) -. (10. ** (-. float_of_int s))
      in
      value >= -.max && value <= max
  | _ -> true

  (** @return (subtype, supertype) *)
  let order_kind x y =  
    match x, y with
    | x, y when equal_kind x y -> `Equal
    | StringLiteral a, StringLiteral b -> 
      `StringLiteralUnion (Union { ctors = (Enum_kind.make [a; b]); is_closed = false })

    | StringLiteral a, Union u | Union u, StringLiteral a ->
      let b = u.ctors in
      let b' = Enum_kind.Ctors.add a b in
      begin match Enum_kind.Ctors.mem a b, u.is_closed with
      | true, true -> `Equal
      | true, false -> `Order (StringLiteral a, Union { u with ctors = b' })
      | false, false -> `StringLiteralUnion (Union { ctors = b'; is_closed = false })
      | false, true -> `No
      end
    | StringLiteral _  as x , Text -> `Order (x, Text)
    | Text, (StringLiteral _ as x) -> `Order (x, Text)

    | Text, (Union _ as x) -> `Order (x, Text)
    | Union { ctors = a; _ } as x1, (Union { ctors = b ;_ } as x2)  when Enum_kind.Ctors.subset b a -> `Order (x2, x1)

    | StringLiteral x, Datetime | Datetime, StringLiteral x -> `Order (Datetime, StringLiteral x)
    | StringLiteral x, Blob | Blob, StringLiteral x -> `Order (Blob, StringLiteral x)
    | Any, t | t, Any -> `Order (t, t)

    | Int, Decimal dec | Decimal dec, Int -> `Order (Int, Decimal dec)
    | Decimal d1, Decimal d2 when d1 <> d2 -> 
        let scale = match d1.scale, d2.scale with
          | None, _ | _, None -> None
          | Some a, Some b -> Some (max a b) in
        let common = Decimal { precision = None; scale } in
        `Order (common, common)
    | FloatingLiteral _, FloatingLiteral _ -> `Order (Float, Float)
    | Int, Float | Float, Int -> `Order (Int, Float)
    | Decimal _, Float | Float, Decimal _ -> `No
    | FloatingLiteral _, Int | Int, FloatingLiteral _ -> `Order (Int, Float)
    | FloatingLiteral x, Float | Float, FloatingLiteral x -> `Order (FloatingLiteral x, Float)
    | FloatingLiteral f, Decimal dec | Decimal dec, FloatingLiteral f -> 
        if check_exact_exact_number f dec then `Equal else `No
    (* UInt64 cannot be a subtype of Float: double precision only guarantees exact 
     representation up to 2^53 (~9e15), but UInt64 can hold values up to 2^64-1 (~18e18).
     Converting large UInt64 values to Float would lose precision *)
    | UInt64, Int | Int, UInt64 -> `Order (Int, UInt64)
    | Text, Blob | Blob, Text -> `Order (Text, Blob)
    | Int, Datetime | Datetime, Int -> `Order (Int, Datetime)
    | Text, Datetime | Datetime, Text -> `Order (Datetime, Text)

    (*  JSON literal validation:
       sqlgg can statically validate JSON string literals at compile time:
       
       Valid JSON literals are accepted:
       '{"valid": "example"}' -> StringLiteral is subtype of Json
       '["array", "example"]' -> StringLiteral is subtype of Json
       '"simple string"'      -> StringLiteral is subtype of Json
       
       Invalid JSON literals are rejected:
       '{NOTVALID|}' -> No subtype relation, compile error
       '{missing: quotes}' -> No subtype relation, compile error
       
       However, sqlgg cannot validate JSON strings constructed dynamically:
       CONCAT('{"key": "', user_input, '"}') -> Text type, no validation
       CONCAT('{"key": "', column_name, '"}') -> Text type, no validation (column value unknown)
       JSON_EXTRACT(dynamic_column, '$.path') -> Json type, runtime validation
       
       This static validation helps catch JSON syntax errors early in development
       while still allowing dynamic JSON construction when needed. 
    *)

    | Json, StringLiteral x | StringLiteral x, Json -> 
      begin match Yojson.Safe.from_string x with
        | _ -> `Order (StringLiteral x, Json)
        | exception Yojson.Json_error _ -> `No
      end
    | Text, Json | Json, Text -> `Order (Json, Text)
    | Blob, Json | Json, Blob -> `Order (Json, Blob)

    | (Json_path, StringLiteral x | StringLiteral x, Json_path) 
        when Sqlgg_json_path.Json_path.is_valid x -> `Order (StringLiteral x, Json_path)
    | Json_path, Text | Text, Json_path -> `Order (Json_path, Text)

    | (One_or_all, StringLiteral x | StringLiteral x, One_or_all) when is_one_or_all x -> `Order (StringLiteral x, One_or_all)
    | Json, One_or_all | One_or_all, Json -> `Order (One_or_all, Text)

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
    | `Equal nullability, `Order pair -> `CommonType pair |> order |> Option.map (fun t -> { t = t; nullability })
    | `Equal nullability, `Equal -> Some { x with nullability }
    | (`Nullable_Strict|`Strict_Nullable), `Equal -> Some (nullable x.t) (* FIXME need nullability order? *)
    | (`Nullable_Strict|`Strict_Nullable), `Order pair -> `CommonType pair |> order |> Option.map nullable
    | `Equal nullability, `StringLiteralUnion t -> `StringLiteralUnion t |> order |> Option.map (fun t -> { t = t; nullability })
    | (`Nullable_Strict | `Strict_Nullable), `StringLiteralUnion t -> `StringLiteralUnion t |> order |> Option.map nullable

  let common_type_l_ order = function
  | [] -> None
  | t::ts -> List.fold_left (fun acc t -> match acc with None -> None | Some prev -> common_type_ order prev t) (Some t) ts

  let get_subtype = function 
  | `CommonType t -> Some (fst t)
  | `StringLiteralUnion t -> Some t

  let get_supertype = function 
  | `CommonType t -> Some (snd t)
  | `StringLiteralUnion t -> Some t

  let subtype = common_type_ get_subtype
  let supertype = common_type_ get_supertype
  
  let common_subtype types = 
    match common_type_l_ get_subtype types with
    | Some { t = FloatingLiteral _; nullability } -> Some { t = Float; nullability }
    | result -> result

  let common_supertype types = 
    match common_type_l_ get_supertype types with
    | Some { t = FloatingLiteral _; nullability } -> Some { t = Float; nullability }
    | result -> result

  let common_type = subtype

  let has_common_type x y = Option.is_some @@ subtype x y

  type tyvar = Typ of t | Var of int [@@deriving show{with_path=false}]
  let string_of_tyvar = function Typ t -> show t | Var i -> sprintf "'%c" (Char.chr @@ Char.code 'a' + i)
end

module Constraint =
struct
  module StringSet = struct
    include Set.Make(String)
    let show s = [%derive.show: string list] (elements s)
    let pp fmt s = Format.fprintf fmt "%s" (show s)
  end

  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
    [@@deriving show{with_path=false}, ord]

  type composite = | CompositePrimary of StringSet.t | CompositeUnique of StringSet.t
    [@@deriving show{with_path=false}, ord]

  type t = | PrimaryKey | NotNull | Null | Unique | Autoincrement | OnConflict of conflict_algo | WithDefault | Composite of composite
    [@@deriving show{with_path=false}, ord]

  let make_composite_primary cols = Composite (CompositePrimary (StringSet.of_list cols))
  let make_composite_unique cols = Composite (CompositeUnique (StringSet.of_list cols))
end

module Constraints = struct
  include Set.Make(Constraint)
  let show s = [%derive.show: Constraint.t list] (elements s)
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Meta = struct

  module StringMap = Map.Make(String)
  
  type t = string StringMap.t
  
  let of_list list = List.fold_left (fun map (k, v) -> StringMap.add k v map) StringMap.empty list
  
  let empty () = StringMap.empty
  
  let find_opt k map = StringMap.find_opt map k
  
  let mem k map = StringMap.mem map k
  let pp fmt t =
    if StringMap.is_empty t then
      Format.fprintf fmt "{}"
    else begin
      Format.fprintf fmt "{";
      let first_key = fst (StringMap.min_binding t) in
      StringMap.iter (fun k v ->
        if k = first_key then
          Format.fprintf fmt "%s = %s" k v
        else
          Format.fprintf fmt "; %s = %s" k v
      ) t;
      Format.fprintf fmt "}"
    end

  let equal = StringMap.equal String.equal

  let merge_right t1 t2 =
    StringMap.merge (fun _ v1 v2 ->
      match v1, v2 with
      | Some v, None -> Some v
      | Some _, Some v2 -> Some v2
      | None, Some v -> Some v
      | None, None -> None
    ) t1 t2

  let get_is_non_nullifiable meta = Option.default "false" (find_opt meta "non_nullifiable") = "true" 
end

type attr = {name : string; domain : Type.t; extra : Constraints.t; meta: Meta.t; }
  [@@deriving show {with_path=false}]

let make_attribute name kind extra ~meta =
  if Constraints.mem Null extra && Constraints.mem NotNull extra then fail "Column %s can be either NULL or NOT NULL, but not both" name;
  let domain = Type.{ t = Option.default Int kind; nullability = if List.exists (fun cstrt -> Constraints.mem cstrt extra) [NotNull; PrimaryKey]
    then Strict else Nullable } in
  {name;domain;extra;meta=Meta.of_list meta;}

let unnamed_attribute ?(meta = Meta.empty()) domain = {name="";domain;extra=Constraints.empty;meta;}

let make_attribute' ?(extra = Constraints.empty) ?(meta = []) name domain = { name; domain; extra; meta = Meta.of_list meta; }

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
    type typ = Left | Right | Full | Inner | Straight [@@deriving show]

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
      | Inner | Straight -> action a b
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
  schema |> List.iter begin fun {name;domain;extra;_} ->
    IO.printf out "%10s %s %s\n" (Type.show domain) name (Constraints.show extra)
  end;
  IO.write_line out ""

(** optional name and start/end position in string *)
type param_id = string option located [@@deriving show]
type shared_query_ref_id = string located [@@deriving show]

module Source_type = struct
  type kind = Infer of Type.kind | UInt32 [@@deriving show, eq]

  type t = { t : kind; nullability : Type.nullability; } [@@deriving eq, show{with_path=false}, make]

  let nullability nullability t = { t = Infer t; nullability }
  let strict = nullability Type.Strict
  let depends = nullability Type.Depends
  let nullable = nullability Type.Nullable

  let to_infer_type { t; nullability; } =
    let t = match t with
      | Infer ty -> ty
      | UInt32 -> Type.Int 
    in
    { Type.t; nullability }

  let show { t; nullability; } = 
    let kind_str = match t with
      | Infer ty -> Type.show_kind ty
      | UInt32 -> "UInt32"
    in
    kind_str ^ (match nullability with Type.Nullable -> "?" | Type.Depends -> "??" | Type.Strict -> "")
end

type 't param = { id : param_id; typ : 't; } [@@deriving show, make]
type option_actions_kind = BoolChoices | SetDefault [@@deriving show]
type params = Type.t param list [@@deriving show]
type in_or_not_in = [`In | `NotIn] [@@deriving show]
type ctor =
| Simple of param_id * var list option
| Verbatim of string * string
and var =
| Single of Type.t param * Meta.t
| SingleIn of Type.t param * Meta.t
| ChoiceIn of { param: param_id; kind : in_or_not_in; vars: var list }
| Choice of param_id * ctor list
| DynamicSelect of param_id * ctor list
| TupleList of param_id * tuple_list_kind
(* It differs from Choice that in this case we should generate sql "TRUE", it doesn't seem reusable *)
| OptionActionChoice of param_id * var list * (pos * pos) * option_actions_kind
| SharedVarsGroup of vars * shared_query_ref_id
and tuple_list_kind = 
  | Insertion of schema 
  | Where_in of ((Type.t * Meta.t) list * in_or_not_in) located 
  | ValueRows of { types: Type.t list; values_start_pos: int; }
[@@deriving show]
and vars = var list [@@deriving show]

type alter_pos = [ `After of string | `Default | `First ] [@@deriving show {with_path=false}]

type direction = [ `Fixed | `Param of param_id ] [@@deriving show]

type cte_supported_compound_op = [ `Union | `Union_all ] [@@deriving show]

type compound_op = [ cte_supported_compound_op | `Except | `Intersect ] [@@deriving show]

type int_or_param = [`Const of int | `Limit of Source_type.t param]
type limit_t = [ `Limit | `Offset ]
type col_name = {
  cname : string; (** column name *)
  tname : table_name option;
} [@@deriving show]
type logical_op = And | Or | Xor [@@deriving show]
type comparison_op = Comp_equal | Comp_num_cmp | Comp_num_eq | Not_distinct_op | Is_null | Is_not_null [@@deriving eq, show]
type null_handling_fn_kind = Coalesce of Type.tyvar * Type.tyvar | Null_if | If_null [@@deriving show]
type source_alias = { table_name : table_name; column_aliases : schema option } [@@deriving show]
type select_row_locking_kind = For_update | For_share [@@deriving show]
and limit = Source_type.t param list * bool
and nested = source * (source * Schema.Join.typ located * join_condition) located list [@@deriving show]
and source_kind = [ `Select of select_full | `Table of table_name | `Nested of nested | `ValueRows of row_values ]
and source = (source_kind * source_alias option) (* alias, position *)
and join_condition = expr Schema.Join.condition
and select = {
  columns : column list;
  from : nested option;
  where : expr option;
  group : expr list;
  having : expr option;
}
and cte_item = { cte_name: string; cols: schema option; stmt: cte_stmt; } [@@deriving show]
and cte_stmt = CteInline of select_complete | CteSharedQuery of shared_query_ref_id [@@deriving show]
and cte = { cte_items: cte_item list; is_recursive: bool; } [@@deriving show]
and select_complete = {
  select : select * (compound_op * select) list;
  order : order;
  limit : limit option;
  select_row_locking: select_row_locking_kind located option;
}
and select_full = { select_complete: select_complete; cte: cte option; }
and row_constructor_list = RowExprList of expr list list | RowParam of { id : param_id; types : Source_type.t list; values_start_pos: int; } 
and row_values = {
  row_constructor_list: row_constructor_list;
  row_order: order;
  row_limit: limit option;
}
and order = (expr * direction option) list
and agg_with_order_kind = 
    | Group_concat
    | Json_arrayagg
and agg_fun = Self (* self means that it returns the same type what aggregated columns have. ie: max, min *) 
    | Count (* count it's count function which never returns null  *) 
    | Avg (* avg it's avg function that returns float *)
    | With_order of {
        with_order_kind: agg_with_order_kind;
        order: order; 
      }
and 't func =
  | Agg of agg_fun (* 'a -> 'a | 'a -> t *)
  | Null_handling of null_handling_fn_kind
  | Comparison of comparison_op
  | Logical of logical_op
  | Negation
  | Ret of 't (* _ -> t *) (* TODO eliminate *)
  | F of Type.tyvar * Type.tyvar list
  | Col_assign of { ret_t: Type.tyvar; col_t: Type.tyvar; arg_t: Type.tyvar; }
  | Multi of { 
    ret: Type.tyvar; 
    fixed_args: Type.tyvar list; 
    repeating_pattern: Type.tyvar list 
  }
  (* repeating_pattern is needed for functions with fixed initial args + optional repeating pattern
     Example: JSON_ARRAY_APPEND(json_doc, path, val[, path, val] ...)
     - return_type: what function returns
     - fixed_args: required initial arguments [json_doc, path, val] 
     - repeating_pattern: list of types that repeat [path_type, val_type]
     Valid calls: f(a,b,c) or f(a,b,c,d,e) or f(a,b,c,d,e,f,g) etc. *)
  [@@deriving show]
and 'expr choices = (param_id * 'expr option) list
and 't fun_ = { fn_name: string; kind: 't func; parameters: expr list; is_over_clause: bool; } [@@deriving show]
and case_branch = { when_: expr; then_: expr }
and case = {  
  case: expr option;
  branches: case_branch list;
  else_: expr option;
} [@@deriving show]
and in_tuple_list = { exprs: expr list; param_id: param_id; kind_in_tuple_list: in_or_not_in; } [@@deriving show]
and expr =
  | Value of Type.t collated (** literal value *)
  | Param of Source_type.t param * Meta.t
  | Inparam of Source_type.t param * Meta.t
  | Choices of param_id * expr choices
  | InChoice of param_id * in_or_not_in * expr
  | Fun of Source_type.t fun_
  | SelectExpr of select_full * [ `AsValue | `Exists ]
  | Column of col_name collated
  | InTupleList of in_tuple_list located
   (* pos - full syntax pos from {, to }?, pos is only sql, that inside {}?
      to use it during the substitution and to not depend on the magic numbers there.
   *)
  | OptionActions of { choice: expr; pos: (pos * pos); kind: option_actions_kind }
  | Case of case
  | Of_values of string (** VALUES(col_name) *)
and column =
  | All
  | AllOf of table_name
  | Expr of expr * string option (** name *)
  [@@deriving show {with_path=false}]

type columns = column list [@@deriving show]

let source_fun_kind_to_infer = function
  | Ret t -> Ret (Source_type.to_infer_type t)
  | Agg (Self | Count | Avg | With_order _) 
  | Null_handling _ | Comparison _
  | Logical _ | Negation | F _ 
  | Col_assign _ | Multi _ as fn -> fn

let expr_to_string = show_expr

let make_partition_by = List.iter (function
  | Value _ -> fail "ORDER BY or PARTITION BY uses legacy position indication which is not supported, use expression."
  | _ -> ())

type assignment_expr = 
  | RegularExpr of expr 
  | AssignDefault
  | WithDefaultParam of expr * (pos * pos)
  [@@deriving show {with_path=false}]

type assignments = (col_name * assignment_expr) list [@@deriving show]

type on_conflict = Do_update of assignments | Do_nothing [@@deriving show]

type conflict_clause = 
  | On_duplicate of { assignments: assignments; }
  | On_conflict of { action: on_conflict; attrs: col_name list; }
  [@@deriving show]

type insert_action_kind = Insert_into | Replace_into of pos [@@deriving show]

type insert_action =
{
  insert_action_kind: insert_action_kind;
  target : table_name;
  action : [ `Set of assignments option
           | `Values of (string list option * assignment_expr list list option) (* column names * list of value tuples *)
           | `Param of (string list option * param_id)
           | `Select of (string list option * select_full) ];
  on_conflict_clause : conflict_clause located option;
} [@@deriving show {with_path=false}]

type table_constraints = [ `Ignore | `Primary of string list | `Unique of string list ] [@@deriving show {with_path=false}]

type index_kind  = 
  | Regular_idx
  | Fulltext
  | Spatial
  [@@deriving show {with_path=false}]

module Alter_action_attr = struct

  type constraint_ = Syntax_constraint of Constraint.t | Default of expr located
    [@@deriving show {with_path=false}]

  type t = {  
    name : string; 
    kind : Source_type.kind collated located option;
    extra : constraint_ located list;
    meta: (string * string) list; 
  }
  [@@deriving show {with_path=false}]

  let constraint_to_syntax_constraint = function
    | Syntax_constraint c -> c
    | Default _ -> WithDefault

  let kind_to_type_kind = function
    | Source_type.Infer k -> k
    | Source_type.UInt32 -> Type.Int

  let to_attr (x: t): attr = make_attribute x.name 
    (Option.map_default (fun k -> Some (kind_to_type_kind k.value.collated)) None x.kind)
    (Constraints.of_list (List.map (fun c -> constraint_to_syntax_constraint c.value) x.extra))
    ~meta:x.meta

  (* All attributes were already checked for dialect and default expression when writing to Tables,
     we deliberately make the fields dummy to reconstruct
   *)
  let from_attr (attr: attr): t =
    let extra = attr.extra |> Constraints.elements |> List.map (fun c -> 
      let c = match c with
      | Constraint.WithDefault -> Default (make_located ~pos:(0,0) ~value:(Value 
        (make_collated ~collated:(Type.depends Any) ()))) 
      | x -> Syntax_constraint x
      in
      make_located ~pos:(0,0) ~value:c
    ) in
    let kind = Some (make_located ~pos:(0,0) ~value:(make_collated ~collated:(Source_type.Infer attr.domain.Type.t) ())) in
    let meta = Meta.StringMap.to_seq attr.meta |> List.of_seq in
    { name = attr.name; kind; extra; meta }
end

type create_target_schema = { 
  schema: Alter_action_attr.t list; 
  constraints: table_constraints list; 
  indexes: index_kind located list; 
}
[@@deriving show]

type create_target = 
  | Schema of create_target_schema
  | Select of select_full located
[@@deriving show {with_path=false}]

type alter_action = [
    | `Add of Alter_action_attr.t * alter_pos
    | `RenameTable of table_name
    | `RenameColumn of string * string
    | `RenameIndex of string * string
    | `Drop of string
    | `Change of string * Alter_action_attr.t * alter_pos
    | `Default_or_convert_to of string located option
    | `None ] [@@deriving show {with_path=false}]

type stmt =
  | Create of table_name * create_target
  | Drop of table_name
  | Alter of table_name * alter_action list
  | Rename of (table_name * table_name) list
  | CreateIndex of string * table_name * string collated list (* index name, table name, columns *)
  | Insert of insert_action
  | Delete of table_name * expr option
  | DeleteMulti of table_name list * nested * expr option
  | Set of (string * expr) list * stmt option
  | Update of table_name * assignments * expr option * order * Source_type.t param list (* where, order, limit *)
  | UpdateMulti of nested list * assignments * expr option * order * Source_type.t param list (* where, order, limit *)
  | Select of select_full
  | CreateRoutine of table_name * Source_type.kind collated located option * (string * Source_type.kind collated located * expr option) list (* table_name represents possibly namespaced function name *)
  [@@deriving show {with_path=false}]

(*
open Schema

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)

type schema_column_with_sources =
  | AttrWithSources of table_name Schema.Source.Attr.t
  | DynamicWithSources of param_id * (param_id * table_name Schema.Source.Attr.t) list
  [@@deriving show]

type schema_column =
  | Attr of attr
  | Dynamic of param_id * (param_id * attr) list
  [@@deriving show]

let drop_sources : schema_column_with_sources -> schema_column = function
  | AttrWithSources { attr; _ } -> Attr attr
  | DynamicWithSources (p, l) -> Dynamic (p, List.map (fun (p, { Schema.Source.Attr.attr; _ }) -> p, attr) l)

let monomorphic ret args = F (Typ ret, List.map (fun t -> Type.Typ t) args)
let fixed ret args = monomorphic (Type.depends ret) (List.map Type.depends args)

let fun_identity = F (Var 0, [Var 0])

let pp_func pp f =
  let open Format in
  let rec aux = function
  | Agg Self -> fprintf pp "|'a| -> 'a"
  | Agg Avg -> fprintf pp "|'a| -> float"
  | Agg Count -> fprintf pp "|'a| -> int"
  | Agg (With_order { with_order_kind = Group_concat; _ }) -> fprintf pp "|'a| -> text"
  | Agg (With_order { with_order_kind = Json_arrayagg; _ }) -> fprintf pp "|'a| -> json"
  | Ret ret -> fprintf pp "_ -> %s" (Type.show ret)
  | F (ret, args) -> fprintf pp "%s -> %s" (String.concat " -> " @@ List.map Type.string_of_tyvar args) (Type.string_of_tyvar ret)
  | Col_assign { ret_t=ret; col_t; arg_t } -> aux (F (ret, [col_t; arg_t]))
  | Null_handling (Coalesce (ret, each_arg)) -> fprintf pp "{ %s }+ -> %s" (Type.string_of_tyvar each_arg) (Type.string_of_tyvar ret)
  | Null_handling _ -> fprintf pp "'a -> 'a -> 'a"
  | Comparison _ -> fprintf pp "'a -> 'a -> %s" (Type.show_kind Bool)
  | Logical _ -> fprintf pp "'a -> 'a -> %s" (Type.show_kind Bool)
  | Negation -> fprintf pp "'a -> %s" (Type.show_kind Bool)
  | Multi { ret; fixed_args; repeating_pattern } ->
      let fixed_str = match fixed_args with
        | [] -> ""
        | args -> String.concat " -> " (List.map Type.string_of_tyvar args) ^ " -> "
      in
      let repeating_str = String.concat ", " (List.map Type.string_of_tyvar repeating_pattern) in
      fprintf pp "%s[%s]* -> %s" fixed_str repeating_str (Type.string_of_tyvar ret)
  in
  aux f

let string_of_func = Format.asprintf "%a" pp_func

let is_grouping = function
  | Agg _ -> true
  | Col_assign _ | Ret _ | F _ | Multi _ | Null_handling _  | Comparison _ | Negation | Logical _ -> false

module Function : sig

val lookup : string -> int -> Source_type.t func
val lookup_agg : string -> int -> Source_type.t func

val add : int -> Source_type.t func -> string -> unit
val exclude : int -> string -> unit
val monomorphic : Type.t -> Type.t list -> string -> unit
val multi : ret:Type.tyvar -> Type.tyvar -> string -> unit
val multi_polymorphic : string -> unit
val add_multi: Source_type.t func -> string -> unit
val sponge : Source_type.t func
val add_fixed_then_pairs : ret:Type.tyvar -> fixed_args:Type.tyvar list -> repeating_pattern:Type.tyvar list -> string -> unit

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

let sponge = 
  let open Type in 
  let any = depends Any in 
  Multi { ret = Typ any; fixed_args = []; repeating_pattern = [Typ any] }

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

let lookup_agg name narg = match lookup name narg with 
  | Agg _ as a -> a
  | _ -> fail "Function %s is not an aggregate function" name

let monomorphic ret args name = add (List.length args) (monomorphic ret args) name
let multi_polymorphic name = 
  add_multi (Multi { ret = Var 0; fixed_args = []; repeating_pattern = [Var 0] }) name

let multi ~ret args name = 
  add_multi (Multi { ret; fixed_args = []; repeating_pattern = [args] }) name

let add_fixed_then_pairs ~ret ~fixed_args ~repeating_pattern name = 
  add_multi (Multi { ret; fixed_args; repeating_pattern }) name

end

let () =
  let open Type in
  let open Function in
  let (||>) x f = List.iter f x in
  let int = strict Int in
  let float = strict Float in
  let text = strict Text in
  let json = strict Json in
  let json_path = strict Json_path in
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
  "nullif" |> add 2 (Null_handling Null_if);
  "ifnull" |> add 2 (Null_handling If_null);
  ["least";"greatest";] ||> multi_polymorphic;
  "strftime" |> exclude 1; (* requires at least 2 arguments *)
  ["concat";"concat_ws";"strftime"] ||> multi ~ret:(Typ (depends Text)) (Typ (depends Text));
  "date" |> monomorphic datetime [datetime];
  "time" |> monomorphic text [datetime];
  "julianday" |> multi ~ret:(Typ float) (Typ text);
  "from_unixtime" |> monomorphic datetime [int];
  "from_unixtime" |> monomorphic text [int;text];
  ["pow"; "power"] ||> monomorphic float [float;int];
  "unix_timestamp" |> monomorphic int [];
  "unix_timestamp" |> monomorphic int [datetime];
  ["extract"; "dayofmonth";"dayofweek";"dayofyear";] ||> monomorphic int [datetime];
  "last_day" |> monomorphic datetime [datetime];
  ["microsecond"; "second"; "minute"; "hour"; "day"; "week"; "month"; "quarter"; "year" ] ||> monomorphic int [datetime];
  ["current_date";"current_timestamp";"current_time";"localtime";"localtimestamp";"now" ] ||> monomorphic datetime [];
  "getdate" |> monomorphic datetime [];
  ["timestampdiff";"timestampadd"] ||> monomorphic int [strict @@ Datetime;datetime;datetime];
  ["date_add";"date_sub"] ||> monomorphic datetime [datetime; strict @@ Datetime];
  ["date_format";"time_format"] ||> monomorphic text [datetime; text];
  "str_to_date" |> monomorphic datetime [text;text];
  "any_value" |> add 1 (F (Var 0,[Var 0])); (* 'a -> 'a but not aggregate *)
  ["substring"; "sha2"] ||> monomorphic text [text; int];
  "substring" |> monomorphic text [text; int; int];
  "substring_index" |> monomorphic text [text; text; int];
  "replace" |> monomorphic text [text; text; text];
  "last_insert_id" |> monomorphic int [];
  "last_insert_id" |> monomorphic int [int];
  add_multi Type.(Null_handling (Coalesce (Var 0, Var 0))) "coalesce";
  "uuid" |> monomorphic text [];
  "uuid_short" |> monomorphic int [];
  "is_uuid" |> monomorphic bool [text];
  "makedate" |> monomorphic datetime [int; int];
  (* 
     Any is used instead of Var because MySQL JSON functions have unique semantics:
   
   1. ACCEPT ANY DATA TYPE: MySQL JSON functions accept values of any type
      and automatically serialize them to JSON according to built-in rules
   
   2. PRESERVE TYPES IN JSON: each type is serialized differently:
      - Numbers → JSON numbers (123 → 123)
      - Strings → JSON strings ("text" → "text")  
      - Booleans → JSON booleans (true → true)
      - NULL → JSON null
      - JSON-like strings remain STRINGS: '{"a":1}' → "{\"a\":1}" (not parsed!)
   
   3. ONLY RESULTS OF JSON FUNCTIONS become JSON objects:
      JSON_SET(data, '$.obj', JSON_OBJECT('key', 'value'))  -- JSON object
      JSON_SET(data, '$.str', '{"key": "value"}')           -- string!
   
   4. CRITICAL: different values in a single call can have DIFFERENT types
      
      Example of valid MySQL query:
      JSON_SET(
        data, 
        '$.user.name',    'Alice',              -- Text
        '$.user.age',     25,                   -- Int
        '$.user.active',  true,                 -- Bool
        '$.user.score',   99.5,                 -- Float
        '$.user.meta',    JSON_OBJECT('x', 1)   -- Json
      )
   
   WHY NOT Var 0:
   If we used ~repeating_pattern:[Typ json_path; Var 0], then:
   - First value 'Alice' (Text) → Var 0 becomes Text
   - Second value 25 (Int) → requires Text, but gets Int → TYPE ERROR
   - Valid MySQL query would be rejected!
   
   WHY NOT fresh Var for each cycle:
   Consider this example:
   JSON_ARRAY_APPEND(
     data, 
     '$[0].items',     123,           -- Int
     '$[1].props',     "hello",       -- Text  
     '$[2].flags',     true,          -- Bool
     '$[3].meta',      null,          -- Null
     '$[4].nested',    JSON_OBJECT('x', 'y')  -- Json
   )
   
   With fresh Var this would be:
   json -> json_path -> 'a -> json_path -> 'b -> json_path -> 'c -> json_path -> 'd -> json_path -> 'e -> json
   
  This is essentially an existential type: json -> (json_path -> ∃a. a)* -> json
   
   But this complicates implementation for the same effect as Any:
   - Fresh Var can be any type = Any
   - In our type system: | Any, t | t, Any -> `Order (t, t)
   - Any already correctly handles unification with any types
   
   Applied to: JSON_SET, JSON_ARRAY_APPEND, JSON_OBJECT, JSON_ARRAY, etc.
  *)
  "json_array_append" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[Typ (depends Json); Typ json_path; Typ (depends Any)]
    ~repeating_pattern:[Typ json_path; Typ (depends Any)];
  "json_search" |> monomorphic ((nullable Json)) [json; strict One_or_all; text];
  "json_search" |> add_fixed_then_pairs
    ~ret:(Typ (nullable Json))
    ~fixed_args:[Typ json; Typ (strict One_or_all); Typ text; Typ text]
    ~repeating_pattern:[Typ json_path];
  "json_remove" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[Typ (depends Json); Typ (depends Json_path)]
    ~repeating_pattern:[Typ (depends Json_path)];   
  "json_set" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[(Typ (depends Json)); Typ (depends Json_path); Typ (depends Any)]
    ~repeating_pattern:[Typ (depends Json_path); Typ (depends Any)];
  "json_array" |> multi ~ret:(Typ json) (Typ (depends Any));
  "json_object" |> add 0 (F (Typ json, []));
  "json_object" |> add_fixed_then_pairs
    ~ret:(Typ json)
    ~fixed_args:[Typ text; Typ (depends Any)]
    ~repeating_pattern:[Typ text; Typ (depends Any)]; 
  "json_contains" |> add 2 (F (Typ (nullable Bool), [Typ json; Typ json]));
  "json_contains" |> add 3 (F (Typ (nullable Bool), [Typ json; Typ json; Typ json_path]));
  "json_unquote" |> monomorphic (depends Text) [depends (Json)];
  "json_extract" |> add_fixed_then_pairs
    ~ret:(Typ (nullable Json))
    ~fixed_args:[Typ json; Typ json_path]
    ~repeating_pattern:[Typ json_path];
  "json_array_insert" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[Typ json; Typ json_path; Typ (strict Any)]
    ~repeating_pattern:[Typ json_path; Typ (strict Any)];
  "json_contains_path" |> add_fixed_then_pairs
    ~ret:(Typ (depends Bool))
    ~fixed_args:[Typ (depends Json); Typ (depends One_or_all); Typ (depends Json_path)]
    ~repeating_pattern:[Typ (depends Json_path)];
  "json_depth" |> add 1 (F (Typ (depends Int), [Typ (strict Json)]));
  "json_insert" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[Typ json; Typ json_path; Typ (strict Any)]
    ~repeating_pattern:[Typ json_path; Typ (strict Any)];
  "json_keys" |> add 1 (F (Typ (depends Json), [Typ json]));
  "json_keys" |> add 2 (F (Typ (depends Json), [Typ json; Typ json_path]));
  "json_length" |> add 1 (F (Typ (strict Int), [Typ json]));
  "json_length" |> add 2 (F (Typ (strict Int), [Typ json; Typ json_path]));
  "json_merge" |> multi ~ret:(Typ (depends Json)) (Typ json);
  "json_merge_patch" |> multi ~ret:(Typ (depends Json)) (Typ json);
  "json_merge_preserve" |> multi ~ret:(Typ (depends Json)) (Typ json);
  "json_pretty" |> monomorphic (depends Text) [json];
  "json_quote" |> monomorphic (depends Text) [text];
  "json_replace" |> add_fixed_then_pairs
    ~ret:(Typ (depends Json))
    ~fixed_args:[Typ json; Typ json_path; Typ (strict Any)]
    ~repeating_pattern:[Typ json_path; Typ (strict Any)];
  "json_storage_size" |> add 1 (F (Typ (depends Int), [Typ json]));
  "json_type" |> add 1 (F (Typ (depends Text), [Typ json]));
  "json_valid" |> add 1 (F (Typ (depends Bool), [Typ text]));
  ()
