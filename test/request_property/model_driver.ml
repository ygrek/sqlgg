
type cell =
  | Null
  | Bool of bool
  | Int of int64
  | UInt64 of Unsigned.UInt64.t
  | Float of float
  | Decimal of float
  | String of string
  | Json of Sqlgg_trait_types.json
  | Json_path of Sqlgg_trait_types.json_path
  | One_or_all of Sqlgg_trait_types.one_or_all
  | Enum of string

type store = {
  mutable rows : cell array list;
  mutable query : Sqlgg_query.t option;
  mutable bound : cell list;
  mutable reads : int list;
}

let connect () = { rows = []; query = None; bound = []; reads = [] }

let reads store = List.rev store.reads

let describe = function
  | Null -> "NULL"
  | Bool value -> Printf.sprintf "Bool %b" value
  | Int value -> Printf.sprintf "Int %Ld" value
  | UInt64 value -> Printf.sprintf "UInt64 %s" (Unsigned.UInt64.to_string value)
  | Float value -> Printf.sprintf "Float %h" value
  | Decimal value -> Printf.sprintf "Decimal %h" value
  | String value -> Printf.sprintf "String %S" value
  | Json _ -> "Json"
  | Json_path _ -> "Json_path"
  | One_or_all value -> Printf.sprintf "One_or_all %s" (match value with `One -> "one" | `All -> "all")
  | Enum value -> Printf.sprintf "Enum %S" value

let family = function
  | Null -> "NULL"
  | Bool _ -> "Bool"
  | Int _ -> "Int"
  | UInt64 _ -> "UInt64"
  | Float _ -> "Float"
  | Decimal _ -> "Decimal"
  | String _ -> "String"
  | Json _ -> "Json"
  | Json_path _ -> "Json_path"
  | One_or_all _ -> "One_or_all"
  | Enum _ -> "Enum"

exception Oops of string

let unsupported name = raise (Oops ("model driver: " ^ name ^ " is not part of the canonical layer"))

module Types = struct
  module Bool = struct
    type t = bool
    let to_literal = string_of_bool
    let bool_to_literal value = to_literal value
  end
  module Int = struct
    type t = int64
    let to_literal = Int64.to_string
    let int64_to_literal value = to_literal value
  end
  module UInt64 = struct
    type t = Unsigned.UInt64.t
    let to_literal = Unsigned.UInt64.to_string
    let uint64_to_literal value = to_literal value
  end
  module Float = struct
    type t = float
    let to_literal = Printf.sprintf "%h"
    let float_to_literal value = to_literal value
  end
  module Text = struct
    type t = string
    let to_literal = Printf.sprintf "%S"
    let string_to_literal value = to_literal value
  end
  module Blob = struct
    type t = string
    let to_literal = Printf.sprintf "X%S"
    let string_to_literal value = to_literal value
  end
  module Decimal = struct
    type t = float
    let to_literal = Printf.sprintf "%h"
    let float_to_literal value = to_literal value
  end
  module Datetime = struct
    type t = string
    let to_literal = Printf.sprintf "%S"
    let float_to_literal = Printf.sprintf "%h"
  end
  module Json = struct
    type t = Sqlgg_trait_types.json
    let to_literal _ = "JSON"
    let json_to_literal value = to_literal value
  end
  module Json_path = struct
    type t = Sqlgg_trait_types.json_path
    let to_literal _ = "JSON_PATH"
    let json_path_to_literal value = to_literal value
  end
  module One_or_all = struct
    type t = Sqlgg_trait_types.one_or_all
    let to_literal = function `One -> "'one'" | `All -> "'all'"
  end
  module Any = struct
    type t = string
    let to_literal = Printf.sprintf "%S"
  end
end

type statement = string
type -'a connection = store
type params = { declared : int; mutable cells : cell list }
type row = { values : cell array; owner : store }
type result = cell list
type execute_response = { affected_rows : int64; insert_id : int64 option }

type num = Types.Int.t
type text = Types.Text.t
type any = Types.Any.t
type datetime = Types.Datetime.t

module IO = struct
  type 'a future = 'a
  let return value = value
  let ( >>= ) value next = next value
  let bracket value cleanup body =
    match body value with
    | result -> cleanup value; result
    | exception error -> cleanup value; raise error
end

let column row index =
  if index < 0 || index >= Array.length row.values then
    raise (Oops (Printf.sprintf "column %d is out of %d" index (Array.length row.values)));
  row.owner.reads <- index :: row.owner.reads;
  row.values.(index)

let mismatch ~wanted ~index cell =
  raise (Oops (Printf.sprintf "column %d: wanted %s, found %s" index wanted (describe cell)))

let required ~wanted pick row index =
  let cell = column row index in
  match pick cell with
  | Some value -> value
  | None -> mismatch ~wanted ~index cell

let optional ~wanted pick row index =
  match column row index with
  | Null -> None
  | cell ->
    (match pick cell with
     | Some value -> Some value
     | None -> mismatch ~wanted ~index cell)

let pick_bool = function Bool value -> Some value | _ -> None
let pick_int = function Int value -> Some value | _ -> None
let pick_uint64 = function UInt64 value -> Some value | _ -> None
let pick_float = function Float value -> Some value | _ -> None
let pick_decimal = function Decimal value -> Some value | _ -> None
let pick_string = function String value -> Some value | _ -> None
let pick_json = function Json value -> Some value | _ -> None
let pick_json_path = function Json_path value -> Some value | _ -> None
let pick_one_or_all = function One_or_all value -> Some value | _ -> None
let pick_enum = function Enum value -> Some value | _ -> None

let get_column_Bool row index = required ~wanted:"Bool" pick_bool row index
let get_column_Bool_nullable row index = optional ~wanted:"Bool" pick_bool row index
let get_column_Int row index = required ~wanted:"Int" pick_int row index
let get_column_Int_nullable row index = optional ~wanted:"Int" pick_int row index
let get_column_UInt64 row index = required ~wanted:"UInt64" pick_uint64 row index
let get_column_UInt64_nullable row index = optional ~wanted:"UInt64" pick_uint64 row index
let get_column_Text row index = required ~wanted:"String" pick_string row index
let get_column_Text_nullable row index = optional ~wanted:"String" pick_string row index
let get_column_Any row index = required ~wanted:"String" pick_string row index
let get_column_Any_nullable row index = optional ~wanted:"String" pick_string row index
let get_column_Float row index = required ~wanted:"Float" pick_float row index
let get_column_Float_nullable row index = optional ~wanted:"Float" pick_float row index
let get_column_Decimal row index = required ~wanted:"Decimal" pick_decimal row index
let get_column_Decimal_nullable row index = optional ~wanted:"Decimal" pick_decimal row index
let get_column_Datetime row index = required ~wanted:"String" pick_string row index
let get_column_Datetime_nullable row index = optional ~wanted:"String" pick_string row index
let get_column_Json row index = required ~wanted:"Json" pick_json row index
let get_column_Json_nullable row index = optional ~wanted:"Json" pick_json row index
let get_column_Json_path row index = required ~wanted:"Json_path" pick_json_path row index
let get_column_Json_path_nullable row index = optional ~wanted:"Json_path" pick_json_path row index
let get_column_One_or_all _ _ = unsupported "get_column_One_or_all"
let get_column_One_or_all_nullable _ _ = unsupported "get_column_One_or_all_nullable"

let get_column_string row index = required ~wanted:"String" pick_string row index
let get_column_string_nullable row index = optional ~wanted:"String" pick_string row index
let get_column_one_or_all row index = required ~wanted:"One_or_all" pick_one_or_all row index
let get_column_one_or_all_nullable row index = optional ~wanted:"One_or_all" pick_one_or_all row index
let get_column_bool _ _ = unsupported "get_column_bool"
let get_column_bool_nullable _ _ = unsupported "get_column_bool_nullable"
let get_column_int64 _ _ = unsupported "get_column_int64"
let get_column_int64_nullable _ _ = unsupported "get_column_int64_nullable"
let get_column_uint64 _ _ = unsupported "get_column_uint64"
let get_column_uint64_nullable _ _ = unsupported "get_column_uint64_nullable"
let get_column_float _ _ = unsupported "get_column_float"
let get_column_float_nullable _ _ = unsupported "get_column_float_nullable"
let get_column_decimal _ _ = unsupported "get_column_decimal"
let get_column_decimal_nullable _ _ = unsupported "get_column_decimal_nullable"
let get_column_datetime _ _ = unsupported "get_column_datetime"
let get_column_datetime_nullable _ _ = unsupported "get_column_datetime_nullable"
let get_column_json _ _ = unsupported "get_column_json"
let get_column_json_nullable _ _ = unsupported "get_column_json_nullable"
let get_column_json_path _ _ = unsupported "get_column_json_path"
let get_column_json_path_nullable _ _ = unsupported "get_column_json_path_nullable"

let start_params _statement declared = { declared; cells = [] }

let bind params cell = params.cells <- cell :: params.cells

let finish_params params =
  let bound = List.length params.cells in
  if bound <> params.declared then
    raise (Oops (Printf.sprintf "declared %d parameters, bound %d" params.declared bound));
  List.rev params.cells

let set_param_null params = bind params Null
let set_param_Bool params value = bind params (Bool value)
let set_param_Int params value = bind params (Int value)
let set_param_UInt64 params value = bind params (UInt64 value)
let set_param_Float params value = bind params (Float value)
let set_param_Decimal params value = bind params (Decimal value)
let set_param_Text params value = bind params (String value)
let set_param_Datetime params value = bind params (String value)
let set_param_Json params value = bind params (Json value)
let set_param_Json_path params value = bind params (Json_path value)
let set_param_Any params value = bind params (String value)
let set_param_One_or_all _ _ = unsupported "set_param_One_or_all"

let set_param_string params value = bind params (String value)
let set_param_one_or_all params value = bind params (One_or_all value)
let set_param_bool _ _ = unsupported "set_param_bool"
let set_param_int64 _ _ = unsupported "set_param_int64"
let set_param_uint64 _ _ = unsupported "set_param_uint64"
let set_param_float _ _ = unsupported "set_param_float"
let set_param_decimal _ _ = unsupported "set_param_decimal"
let set_param_datetime _ _ = unsupported "set_param_datetime"
let set_param_json _ _ = unsupported "set_param_json"
let set_param_json_path _ _ = unsupported "set_param_json_path"

module Make_enum (E : Sqlgg_traits.Enum) = struct
  let get_column row index = E.inj (required ~wanted:"Enum" pick_enum row index)
  let get_column_nullable row index = Option.map E.inj (optional ~wanted:"Enum" pick_enum row index)
  let set_param params value = bind params (Enum (E.proj value))
  let to_literal value = "'" ^ E.proj value ^ "'"
end

let no_params _statement = []

let run store (query : Sqlgg_query.t) set_params =
  store.query <- Some query;
  store.reads <- [];
  store.bound <- set_params query.sql

let select store query set_params callback =
  run store query set_params;
  List.iter (fun cells -> callback { values = cells; owner = store }) store.rows

let select_one_maybe store query set_params convert =
  run store query set_params;
  match store.rows with
  | [] -> None
  | cells :: _ -> Some (convert { values = cells; owner = store })

let select_one store query set_params convert =
  match select_one_maybe store query set_params convert with
  | Some value -> value
  | None -> raise (Oops "select_one: no rows")

let execute store query set_params =
  run store query set_params;
  store.rows <- store.rows @ [ Array.of_list store.bound ];
  { affected_rows = 1L; insert_id = None }

let execute_unprepared store query = execute store query no_params
