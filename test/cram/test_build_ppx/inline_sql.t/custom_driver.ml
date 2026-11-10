
let unsupported name = failwith ("custom driver: " ^ name ^ " not implemented")

module Types = struct
  module Bool = struct
    type t = bool
    let to_literal = string_of_bool
    let bool_to_literal = to_literal
  end
  module Int = struct
    type t = int
    let to_literal = string_of_int
    let int64_to_literal = Int64.to_string
  end
  module UInt64 = struct
    type t = Unsigned.UInt64.t
    let to_literal = Unsigned.UInt64.to_string
    let uint64_to_literal = to_literal
  end
  module Float = struct
    type t = float
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Text = struct
    type t = Bytes.t
    let quote s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"
    let to_literal bytes = quote (Bytes.to_string bytes)
    let string_to_literal = quote
  end
  module Blob = struct
    type t = Bytes.t
    let to_literal bytes = "X'" ^ Bytes.to_string bytes ^ "'"
    let string_to_literal s = "X'" ^ s ^ "'"
  end
  module Decimal = struct
    type t = float
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Datetime = struct
    type t = float
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Json = struct
    type t = Sqlgg_trait_types.json
    let to_literal _ = unsupported "Json.to_literal"
    let json_to_literal = to_literal
  end
  module Json_path = struct
    type t = Sqlgg_trait_types.json_path
    let to_literal _ = unsupported "Json_path.to_literal"
    let json_path_to_literal = to_literal
  end
  module One_or_all = struct
    type t = Sqlgg_trait_types.one_or_all
    let to_literal = function `One -> "'one'" | `All -> "'all'"
  end
  module Any = struct
    type t = string
    let to_literal s = "'" ^ s ^ "'"
  end
end

type statement = string
type -'a connection = string array list ref
type params = { sql : statement; declared : int; literals : string list ref }
type row = string array
type result = string
type execute_response = { affected_rows : int64; insert_id : int64 option }

type num = Types.Int.t
type text = Types.Text.t
type any = Types.Any.t
type datetime = Types.Datetime.t

exception Oops of string

module IO = struct
  type 'a future = ('a, string) Result.t
  let return x = Ok x
  let ( >>= ) x f = Result.bind x f
  let bracket x cleanup f =
    match x with
    | Error _ as error -> error
    | Ok value ->
      let result = f value in
      ignore (cleanup value);
      result
end

let column row index = if index < Array.length row then row.(index) else ""
let nullable convert row index =
  match column row index with "" -> None | value -> Some (convert value)
let strict convert row index = convert (column row index)

let get_column_Bool = strict bool_of_string
let get_column_Bool_nullable = nullable bool_of_string
let get_column_Int = strict int_of_string
let get_column_Int_nullable = nullable int_of_string
let get_column_UInt64 = strict Unsigned.UInt64.of_string
let get_column_UInt64_nullable = nullable Unsigned.UInt64.of_string
let get_column_Text = strict Bytes.of_string
let get_column_Text_nullable = nullable Bytes.of_string
let get_column_Any = strict Fun.id
let get_column_Any_nullable = nullable Fun.id
let get_column_Float = strict float_of_string
let get_column_Float_nullable = nullable float_of_string
let get_column_Decimal = strict float_of_string
let get_column_Decimal_nullable = nullable float_of_string
let get_column_Datetime = strict float_of_string
let get_column_Datetime_nullable = nullable float_of_string
let get_column_Json _ = unsupported "get_column_Json"
let get_column_Json_nullable _ = unsupported "get_column_Json_nullable"
let get_column_Json_path _ = unsupported "get_column_Json_path"
let get_column_Json_path_nullable _ = unsupported "get_column_Json_path_nullable"
let get_column_One_or_all _ = unsupported "get_column_One_or_all"
let get_column_One_or_all_nullable _ = unsupported "get_column_One_or_all_nullable"

let get_column_bool = strict bool_of_string
let get_column_bool_nullable = nullable bool_of_string
let get_column_int64 = strict Int64.of_string
let get_column_int64_nullable = nullable Int64.of_string
let get_column_uint64 = strict Unsigned.UInt64.of_string
let get_column_uint64_nullable = nullable Unsigned.UInt64.of_string
let get_column_float = strict float_of_string
let get_column_float_nullable = nullable float_of_string
let get_column_decimal = strict float_of_string
let get_column_decimal_nullable = nullable float_of_string
let get_column_datetime = strict Fun.id
let get_column_datetime_nullable = nullable Fun.id
let get_column_string = strict Fun.id
let get_column_string_nullable = nullable Fun.id
let get_column_json _ = unsupported "get_column_json"
let get_column_json_nullable _ = unsupported "get_column_json_nullable"
let get_column_json_path _ = unsupported "get_column_json_path"
let get_column_json_path_nullable _ = unsupported "get_column_json_path_nullable"
let get_column_one_or_all _ = unsupported "get_column_one_or_all"
let get_column_one_or_all_nullable _ = unsupported "get_column_one_or_all_nullable"

let start_params sql count = { sql; declared = count; literals = ref [] }
let bind literal params = params.literals := literal :: !(params.literals)

let set_param_null params = bind "NULL" params
let set_param_Text params v = bind (Types.Text.to_literal v) params
let set_param_Any params v = bind (Types.Any.to_literal v) params
let set_param_Bool params v = bind (Types.Bool.to_literal v) params
let set_param_Int params v = bind (Types.Int.to_literal v) params
let set_param_UInt64 params v = bind (Types.UInt64.to_literal v) params
let set_param_Float params v = bind (Types.Float.to_literal v) params
let set_param_Decimal params v = bind (Types.Decimal.to_literal v) params
let set_param_Datetime params v = bind (Types.Datetime.to_literal v) params
let set_param_Json params v = bind (Types.Json.to_literal v) params
let set_param_Json_path params v = bind (Types.Json_path.to_literal v) params
let set_param_One_or_all params v = bind (Types.One_or_all.to_literal v) params

let set_param_bool params v = bind (string_of_bool v) params
let set_param_int64 params v = bind (Int64.to_string v) params
let set_param_uint64 params v = bind (Unsigned.UInt64.to_string v) params
let set_param_float params v = bind (string_of_float v) params
let set_param_decimal params v = bind (string_of_float v) params
let set_param_string params v = bind (Types.Text.string_to_literal v) params
let set_param_datetime params v = bind (string_of_float v) params
let set_param_json _ _ = unsupported "set_param_json"
let set_param_json_path _ _ = unsupported "set_param_json_path"
let set_param_one_or_all params v = bind (Types.One_or_all.to_literal v) params

module Make_enum (E : Sqlgg_traits.Enum) = struct
  let get_column row index = E.inj (column row index)
  let get_column_nullable = nullable E.inj
  let to_literal v = Types.Text.quote (E.proj v)
  let set_param params v = bind (to_literal v) params
end

let finish_params params =
  let bound = List.length !(params.literals) in
  if bound <> params.declared then
    failwith
      (Printf.sprintf "custom driver: declared %d parameters, bound %d" params.declared bound);
  let pending = ref (List.rev !(params.literals)) in
  params.sql
  |> String.to_seq
  |> Seq.map (fun c ->
       match c, !pending with
       | '?', literal :: rest -> pending := rest; literal
       | c, _ -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""
  |> IO.return

let no_params sql = IO.return sql

let send (query : Sqlgg_traits.Query.t) set_params =
  Result.map (fun rendered -> print_endline ("[CUSTOM] " ^ rendered)) (set_params query.sql)

let select db query set_params callback =
  Result.map (fun () -> List.iter callback !db) (send query set_params)

let select_one_maybe db query set_params convert =
  Result.map (fun () -> Option.map convert (List.nth_opt !db 0)) (send query set_params)

let select_one db query set_params convert =
  Result.bind (select_one_maybe db query set_params convert) (function
    | Some value -> Ok value
    | None -> Error "no rows")

let execute db query set_params =
  Result.map
    (fun () -> { affected_rows = Int64.of_int (List.length !db); insert_id = None })
    (send query set_params)

let execute_unprepared db query = execute db query no_params
