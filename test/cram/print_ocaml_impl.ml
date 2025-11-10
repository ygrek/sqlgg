open Printf

module M = struct

type json = [
  | `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Assoc of (string * json) list
  | `List of json list
  | `Int of int
  | `Intlit of string
]

type json_path = Sqlgg_json_path.Ast.t
type one_or_all = [ `One | `All ]

let rec convert_json = function
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as x -> x
  | `Assoc assoc_list ->
    let convert_pair (key, value) = (key, convert_json value) in
    `Assoc (List.map convert_pair assoc_list)
  | `List json_list | `Tuple json_list ->
    `List (List.map convert_json json_list)
  | `Variant _ -> failwith "Variant type is not supported"

type mock_value = [ `Bool of bool | `Int of int64 | `Text of string | `Float of float | `Json of json | `Null ]

type mock_row_data = {
  values: (int * mock_value) array;
}

type mock_response = 
  | MockSelect of mock_row_data list
  | MockSelectOne of mock_row_data option
  | MockExecute of { affected_rows: int64; insert_id: int64 option }

let response_queue : mock_response list ref = ref []
let current_row : mock_row_data option ref = ref None

let setup_select_response (rows : mock_row_data list) : unit =
  response_queue := MockSelect rows :: !response_queue

let setup_select_one_response (row_opt : mock_row_data option) : unit =
  response_queue := MockSelectOne row_opt :: !response_queue

let setup_execute_response ~(affected_rows : int64) ?(insert_id : int64 option = None) () : unit =
  response_queue := MockExecute { affected_rows; insert_id } :: !response_queue

let clear_mock_responses () : unit =
  response_queue := [];
  current_row := None

let get_next_response () : mock_response =
  match !response_queue with
  | [] -> failwith "No more mock responses available"
  | response :: rest ->
    response_queue := rest;
    response

let make_mock_row (values : mock_value list) : mock_row_data =
  { values = Array.of_list (List.mapi (fun i v -> (i, v)) values) }

let mock_bool (v : bool) : mock_value = `Bool v
let mock_int (v : int64) : mock_value = `Int v  
let mock_text (v : string) : mock_value = `Text v
let mock_float (v : float) : mock_value = `Float v
let mock_json (v : json) : mock_value = `Json v
let mock_null : mock_value = `Null

(* Helper function to convert json to Yojson.Basic.t *)
let rec json_to_yojson : json -> Yojson.Basic.t = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Intlit s -> `Int (int_of_string s)  (* Convert Intlit to Int *)
  | `Float f -> `Float f
  | `String s -> `String s
  | `List lst -> `List (List.map json_to_yojson lst)
  | `Assoc assoc -> `Assoc (List.map (fun (k, v) -> (k, json_to_yojson v)) assoc)

(* Helper function to convert from Yojson.Basic.t to our json *)
let rec yojson_to_json : Yojson.Basic.t -> json = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `List lst -> `List (List.map yojson_to_json lst)
  | `Assoc assoc -> `Assoc (List.map (fun (k, v) -> (k, yojson_to_json v)) assoc)

module Types = struct
  module Bool = struct 
    type t = bool 
    let to_literal = string_of_bool
    let bool_to_literal = to_literal
  end
  module Int = struct 
    type t = int64
    let to_literal = Int64.to_string
    let int64_to_literal = to_literal
  end
  module UInt64 = struct
    type t = Unsigned.UInt64.t
    let to_literal = Unsigned.UInt64.to_string
    let uint64_to_literal = to_literal
  end
  module Text = struct
    type t = string
    let to_literal s = sprintf "'%s'" (String.escaped s)
    let string_to_literal = to_literal
  end
  module Blob = struct
    type t = string
    let to_literal s = 
      let hex = String.to_seq s 
               |> Seq.map (fun c -> sprintf "%02X" (Char.code c))
               |> List.of_seq 
               |> String.concat "" in
      sprintf "X'%s'" hex
  end
  module Float = struct 
    type t = float 
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Decimal = struct
    type t = float
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Datetime = struct
    type t = float
    let to_literal t = sprintf "'%s'" (string_of_float t)
    let float_to_literal = to_literal
  end
  
  module Json = struct
    type t = Yojson.Basic.t
    let to_literal j = Text.to_literal (Yojson.Basic.to_string j)
    let json_to_literal j = Text.to_literal (Yojson.Basic.to_string (json_to_yojson j))
  end

  module Json_path = struct
    type t = json_path
    let to_literal j = Text.to_literal (Sqlgg_json_path.Json_path.string_of_json_path j)
    let json_path_to_literal = to_literal
  end

  module One_or_all = struct
    type t = one_or_all
    let to_literal = function
      | `One -> "one"
      | `All -> "all"
  end

  module Any = struct
    type t = string
    let to_literal s = sprintf "'%s'" (String.escaped s)
  end
end

module type Enum = sig 
  type t
  val inj: string -> t
  val proj: t -> string
end

type statement = string * int
type -'a connection = unit
type params = statement * string list ref * int ref * int
type row = mock_row_data
type result = unit
type execute_response = { affected_rows: int64; insert_id: int64 option }

type num = Types.Int.t
type text = Types.Text.t
type any = Types.Any.t
type datetime = Types.Datetime.t

exception Oops of string

let get_mock_value (row : mock_row_data) (index : int) : mock_value =
  if index < Array.length row.values then
    let (_, value) = row.values.(index) in
    value
  else
    `Null

let get_column_Bool (row : mock_row_data) (index : int) : Types.Bool.t = 
  match get_mock_value row index with
  | `Bool v -> printf "[MOCK] get_column_Bool[%d] = %b\n" index v; v
  | _ -> printf "[MOCK] get_column_Bool[%d] = false (default)\n" index; false

let get_column_Int (row : mock_row_data) (index : int) : Types.Int.t = 
  match get_mock_value row index with
  | `Int v -> printf "[MOCK] get_column_Int[%d] = %Ld\n" index v; v
  | _ -> printf "[MOCK] get_column_Int[%d] = 0L (default)\n" index; 0L

let get_column_UInt64 (row : mock_row_data) (index : int) : Unsigned.UInt64.t =
  let _ = row, index in
  printf "[MOCK] get_column_UInt64[%d] = 0 (default)\n" index; Unsigned.UInt64.zero

let get_column_Text (row : mock_row_data) (index : int) : Types.Text.t = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Text[%d] = \"%s\"\n" index v; v
  | _ -> printf "[MOCK] get_column_Text[%d] = \"mock_text\" (default)\n" index; "mock_text"

let get_column_Any (row : mock_row_data) (index : int) : Types.Any.t = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Any[%d] = \"%s\"\n" index v; v
  | _ -> printf "[MOCK] get_column_Any[%d] = \"mock_any\" (default)\n" index; "mock_any"

let get_column_Float (row : mock_row_data) (index : int) : Types.Float.t = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Float[%d] = %f\n" index v; v
  | _ -> printf "[MOCK] get_column_Float[%d] = 0.0 (default)\n" index; 0.0

let get_column_Decimal (row : mock_row_data) (index : int) : Types.Decimal.t = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Decimal[%d] = %f\n" index v; v
  | _ -> printf "[MOCK] get_column_Decimal[%d] = 0.0 (default)\n" index; 0.0

let get_column_Datetime (row : mock_row_data) (index : int) : Types.Datetime.t = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Datetime[%d] = %f\n" index v; v
  | _ -> printf "[MOCK] get_column_Datetime[%d] = 0.0 (default)\n" index; 0.0

let get_column_Json (row : mock_row_data) (index : int) : Types.Json.t = 
  match get_mock_value row index with
  | `Json v -> 
    let yojson_val = json_to_yojson v in
    printf "[MOCK] get_column_Json[%d] = %s\n" index (Yojson.Basic.to_string yojson_val); 
    yojson_val
  | _ -> printf "[MOCK] get_column_Json[%d] = null (default)\n" index; `Null

let mock_json_path = 
  try 
    Sqlgg_json_path.Json_path.parse_json_path "$.mock"
  with 
  | _ -> Sqlgg_json_path.Json_path.parse_json_path "$"

let get_column_Json_path (row : mock_row_data) (index : int) : Types.Json_path.t = 
  printf "[MOCK] get_column_Json_path[%d] = $.mock\n" index;
  mock_json_path

let get_column_One_or_all (row : mock_row_data) (index : int) : Types.One_or_all.t = 
  printf "[MOCK] get_column_One_or_all[%d] = `One\n" index;
  `One

let get_column_Bool_nullable (row : mock_row_data) (index : int) : Types.Bool.t option = 
  match get_mock_value row index with
  | `Bool v -> printf "[MOCK] get_column_Bool_nullable[%d] = Some %b\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Bool_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Bool_nullable[%d] = Some false (default)\n" index; Some false

let get_column_Int_nullable (row : mock_row_data) (index : int) : Types.Int.t option = 
  match get_mock_value row index with
  | `Int v -> printf "[MOCK] get_column_Int_nullable[%d] = Some %Ld\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Int_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Int_nullable[%d] = Some 0L (default)\n" index; Some 0L

let get_column_UInt64_nullable (row : mock_row_data) (index : int) : Unsigned.UInt64.t option =
  match get_mock_value row index with
  | `Null -> printf "[MOCK] get_column_UInt64_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_UInt64_nullable[%d] = Some 0 (default)\n" index; Some Unsigned.UInt64.zero

let get_column_Text_nullable (row : mock_row_data) (index : int) : Types.Text.t option = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Text_nullable[%d] = Some \"%s\"\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Text_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Text_nullable[%d] = Some \"mock_text\" (default)\n" index; Some "mock_text"

let get_column_Any_nullable (row : mock_row_data) (index : int) : Types.Any.t option = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Any_nullable[%d] = Some \"%s\"\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Any_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Any_nullable[%d] = Some \"mock_any\" (default)\n" index; Some "mock_any"

let get_column_Float_nullable (row : mock_row_data) (index : int) : Types.Float.t option = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Float_nullable[%d] = Some %f\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Float_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Float_nullable[%d] = Some 0.0 (default)\n" index; Some 0.0

let get_column_Decimal_nullable (row : mock_row_data) (index : int) : Types.Decimal.t option = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Decimal_nullable[%d] = Some %f\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Decimal_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Decimal_nullable[%d] = Some 0.0 (default)\n" index; Some 0.0

let get_column_Datetime_nullable (row : mock_row_data) (index : int) : Types.Datetime.t option = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Datetime_nullable[%d] = Some %f\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Datetime_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Datetime_nullable[%d] = Some 0.0 (default)\n" index; Some 0.0

let get_column_Json_nullable (row : mock_row_data) (index : int) : Types.Json.t option = 
  match get_mock_value row index with
  | `Json v -> 
    let yojson_val = json_to_yojson v in
    printf "[MOCK] get_column_Json_nullable[%d] = Some %s\n" index (Yojson.Basic.to_string yojson_val); 
    Some yojson_val
  | `Null -> printf "[MOCK] get_column_Json_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Json_nullable[%d] = Some null (default)\n" index; Some `Null

let get_column_Json_path_nullable (row : mock_row_data) (index : int) : Types.Json_path.t option = 
  printf "[MOCK] get_column_Json_path_nullable[%d] = Some $.mock\n" index;
  Some mock_json_path

let get_column_One_or_all_nullable (row : mock_row_data) (index : int) : Types.One_or_all.t option = 
  printf "[MOCK] get_column_One_or_all_nullable[%d] = Some `One\n" index;
  Some `One

let get_column_bool = get_column_Bool
let get_column_bool_nullable = get_column_Bool_nullable
let get_column_int64 = get_column_Int
let get_column_int64_nullable = get_column_Int_nullable
let get_column_uint64 = get_column_UInt64
let get_column_uint64_nullable = get_column_UInt64_nullable
let get_column_float = get_column_Float
let get_column_float_nullable = get_column_Float_nullable
let get_column_decimal = get_column_Decimal
let get_column_decimal_nullable = get_column_Decimal_nullable
let get_column_string = get_column_Text
let get_column_string_nullable = get_column_Text_nullable
let get_column_datetime (row : mock_row_data) (index : int) : string = 
  string_of_float (get_column_Float row index)
let get_column_datetime_nullable (row : mock_row_data) (index : int) : string option = 
  match get_column_Float_nullable row index with
  | Some f -> Some (string_of_float f)
  | None -> None

(* Functions that return Sqlgg_trait_types.json instead of Yojson.Basic.t *)
let get_column_json (row : mock_row_data) (index : int) : json = 
  match get_mock_value row index with
  | `Json v -> printf "[MOCK] get_column_json[%d] = %s\n" index (Yojson.Basic.to_string (json_to_yojson v)); v
  | _ -> printf "[MOCK] get_column_json[%d] = null (default)\n" index; `Null

let get_column_json_nullable (row : mock_row_data) (index : int) : json option = 
  match get_mock_value row index with
  | `Json v -> printf "[MOCK] get_column_json_nullable[%d] = Some %s\n" index (Yojson.Basic.to_string (json_to_yojson v)); Some v
  | `Null -> printf "[MOCK] get_column_json_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_json_nullable[%d] = Some null (default)\n" index; Some `Null

let get_column_json_path = get_column_Json_path
let get_column_json_path_nullable = get_column_Json_path_nullable
let get_column_one_or_all = get_column_One_or_all
let get_column_one_or_all_nullable = get_column_One_or_all_nullable

let substitute_params sql params =
  let param_array = Array.of_list params in
  let param_index = ref 0 in
  String.to_seq sql
  |> Seq.map (function
    | '?' when !param_index < Array.length param_array ->
        let param = param_array.(!param_index) in
        incr param_index;
        param
    | c -> String.make 1 c)
  |> Seq.fold_left (^) ""

let bind_param value_str (params : params) =
  let (stmt, param_list, index, total) = params in
  param_list := value_str :: !param_list;
  incr index

let start_params (stmt : statement) (n : int) : params = 
  let _ = stmt in
  (stmt, ref [], ref 0, n)

let finish_params (params : params) : result = 
  let (stmt, param_list, index, total) = params in
  let (sql, _) = stmt in
  let final_sql = substitute_params sql (List.rev !param_list) in
  printf "[SQL] %s\n" final_sql;
  flush stdout;
  ()

let set_param_null (params : params) = bind_param "NULL" params
let set_param_Text (params : params) (v : Types.Text.t) = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Any (params : params) (v : Types.Any.t) = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Bool (params : params) (v : Types.Bool.t) = bind_param (if v then "TRUE" else "FALSE") params
let set_param_Int (params : params) (v : Types.Int.t) = bind_param (Int64.to_string v) params
let set_param_UInt64 (params : params) (v : Unsigned.UInt64.t) = bind_param (Unsigned.UInt64.to_string v) params
let set_param_Float (params : params) (v : Types.Float.t) = bind_param (Float.to_string v) params
let set_param_Decimal (params : params) (v : Types.Decimal.t) = bind_param (Float.to_string v) params
let set_param_Datetime (params : params) (v : Types.Datetime.t) = bind_param (Float.to_string v) params
let set_param_Json (params : params) (v : Types.Json.t) = bind_param (sprintf "'%s'" (String.escaped (Yojson.Basic.to_string v))) params
let set_param_Json_path (params : params) (v : Types.Json_path.t) = bind_param (sprintf "'%s'" (String.escaped (Sqlgg_json_path.Json_path.string_of_json_path v))) params
let set_param_One_or_all (params : params) (v : Types.One_or_all.t) = bind_param (match v with `One -> "'one'" | `All -> "'all'") params

let set_param_bool (params : params) v = bind_param (if v then "TRUE" else "FALSE") params
let set_param_int64 (params : params) v = bind_param (Int64.to_string v) params
let set_param_uint64 (params : params) v = bind_param (Unsigned.UInt64.to_string v) params
let set_param_float (params : params) v = bind_param (Float.to_string v) params
let set_param_decimal (params : params) v = bind_param (Float.to_string v) params
let set_param_string (params : params) v = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_datetime (params : params) v = bind_param (Float.to_string v) params
let set_param_json (params : params) (v : json) = bind_param (sprintf "'%s'" (String.escaped (Yojson.Basic.to_string (json_to_yojson v)))) params
let set_param_json_path (params : params) v = bind_param (sprintf "'%s'" (String.escaped (Sqlgg_json_path.Json_path.string_of_json_path v))) params
let set_param_one_or_all (params : params) v = bind_param (match v with `One -> "'one'" | `All -> "'all'") params

let no_params (stmt : statement) : result = 
  let (sql, _) = stmt in
  printf "[SQL] %s\n" sql;
  flush stdout;
  ()

module Make_enum (E: Enum) = struct 
  let get_column (row : mock_row_data) (index : int) : E.t = 
    match get_mock_value row index with
    | `Text v -> printf "[MOCK] get_column_Enum[%d] = \"%s\"\n" index v; E.inj v
    | _ -> printf "[MOCK] get_column_Enum[%d] = \"mock_enum\" (default)\n" index; E.inj "mock_enum"
  let get_column_nullable (row : mock_row_data) (index : int) : E.t option = 
    match get_mock_value row index with
    | `Null -> printf "[MOCK] get_column_Enum_nullable[%d] = None\n" index; None
    | `Text v -> printf "[MOCK] get_column_Enum_nullable[%d] = Some \"%s\"\n" index v; Some (E.inj v)
    | _ -> printf "[MOCK] get_column_Enum_nullable[%d] = Some \"mock_enum\" (default)\n" index; Some (E.inj "mock_enum")
  let set_param (params : params) v = bind_param (sprintf "'%s'" (E.proj v)) params
  let to_literal v = Types.Text.to_literal (E.proj v)
end

let select (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (callback : row -> unit) : unit =
  printf "[MOCK SELECT] Connection type: [> `RO ]\n";
  let stmt = (sql, 0) in
  let _ = set_params stmt in
  match get_next_response () with
  | MockSelect rows ->
    printf "[MOCK] Returning %d rows\n" (List.length rows);
    List.iteri (fun i row ->
      printf "  Row %d: " i;
      Array.iteri (fun j (_, value) ->
        let val_str = match value with
          | `Bool b -> string_of_bool b
          | `Int i -> Int64.to_string i
          | `Text s -> s
          | `Float f -> string_of_float f
          | `Json j -> Yojson.Basic.to_string (json_to_yojson j)
          | `Null -> "NULL"
        in
        printf "col%d=%s " j val_str
      ) row.values;
      printf "\n";
      callback row
    ) rows;
    flush stdout
  | _ -> failwith "Expected MockSelect response"

let execute (db : [> `WR ] connection) (sql : string) (set_params : statement -> result) : execute_response =
  printf "[MOCK EXECUTE] Connection type: [> `WR ]\n";
  let stmt = (sql, 0) in
  let _ = set_params stmt in
  match get_next_response () with
  | MockExecute { affected_rows; insert_id } ->
    printf "[MOCK] Execute result: affected_rows=%Ld, insert_id=%s\n" 
      affected_rows
      (match insert_id with Some id -> Int64.to_string id | None -> "None");
    flush stdout;
    { affected_rows; insert_id }
  | _ -> failwith "Expected MockExecute response"

let select_one_maybe (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (convert : row -> 'a) : 'a option =
  printf "[MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]\n";
  let stmt = (sql, 0) in
  let _ = set_params stmt in
  match get_next_response () with
  | MockSelectOne (Some row) ->
    printf "[MOCK] Returning one row\n";
    flush stdout;
    Some (convert row)
  | MockSelectOne None ->
    printf "[MOCK] Returning no rows\n";
    flush stdout;
    None
  | _ -> failwith "Expected MockSelectOne response"

let select_one (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (convert : row -> 'a) : 'a =
  printf "[MOCK SELECT_ONE] Connection type: [> `RO ]\n";
  let stmt = (sql, 0) in
  let _ = set_params stmt in
  match get_next_response () with
  | MockSelectOne (Some row) ->
    printf "[MOCK] Returning one row\n";
    flush stdout;
    convert row
  | MockSelectOne None ->
    failwith "Expected one row but got none"
  | _ -> failwith "Expected MockSelectOne response"

end

let () =
  let module Test = (M : Sqlgg_traits.M) in 
  ignore (Test.Oops "Testing implementation ready")

include M
