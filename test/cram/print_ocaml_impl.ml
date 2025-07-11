open Printf

module M = struct

type json = [ `Null
  | `String of string
  | `Float of float
  | `Int of int
  | `Bool of bool
  | `List of json list
  | `Assoc of (string * json) list 
]

type json_path = Sqlgg_json_path.Ast.t
type one_or_all = [ `One | `All ]

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

module Types = struct
  module Bool = struct 
    type t = bool 
    let to_literal = string_of_bool
    let bool_to_literal = to_literal
  end
  module Int = struct 
    include Int64 
    let to_literal = to_string
    let int64_to_literal = to_literal
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
  module Decimal = Float
  module Datetime = struct
    type t = float
    let to_literal t = sprintf "'%s'" (string_of_float t)
    let float_to_literal = to_literal
  end
  
  module Json = struct
    type t = Yojson.Basic.t
    let to_literal j = Text.to_literal (Yojson.Basic.to_string j)
    let json_to_literal = to_literal
  end

  module Json_path = struct
    open Sqlgg_json_path
    type t = json_path
    let to_literal j = Text.to_literal (Json_path.string_of_json_path j)
    let json_path_to_literal = to_literal
  end

  module One_or_all = struct
    type t = one_or_all
    let to_literal = function
      | `One -> "one"
      | `All -> "all"
    let one_or_all_to_literal = to_literal
  end

  module Any = Text
end

module type Enum = sig 
  type t
  val inj: string -> t
  val proj: t -> string
end

type statement = string * int
type 'a connection = string
type params = string * string list ref * int ref * int
type row = mock_row_data
type result = unit
type execute_response = { affected_rows: int64; insert_id: int64 option }

type num = int64
type text = string
type any = string
type datetime = float

exception Oops of string

let get_mock_value (row : mock_row_data) (index : int) : mock_value =
  if index < Array.length row.values then
    let (_, value) = row.values.(index) in
    value
  else
    `Null

let get_column_Bool (row : mock_row_data) (index : int) : bool = 
  match get_mock_value row index with
  | `Bool v -> printf "[MOCK] get_column_Bool[%d] = %b\n" index v; v
  | _ -> printf "[MOCK] get_column_Bool[%d] = false (default)\n" index; false

let get_column_Int (row : mock_row_data) (index : int) : int64 = 
  match get_mock_value row index with
  | `Int v -> printf "[MOCK] get_column_Int[%d] = %Ld\n" index v; v
  | _ -> printf "[MOCK] get_column_Int[%d] = 0L (default)\n" index; 0L

let get_column_Text (row : mock_row_data) (index : int) : string = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Text[%d] = \"%s\"\n" index v; v
  | _ -> printf "[MOCK] get_column_Text[%d] = \"mock_text\" (default)\n" index; "mock_text"

let get_column_Float (row : mock_row_data) (index : int) : float = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Float[%d] = %f\n" index v; v
  | _ -> printf "[MOCK] get_column_Float[%d] = 0.0 (default)\n" index; 0.0

let get_column_Json (row : mock_row_data) (index : int) : json = 
  match get_mock_value row index with
  | `Json v -> printf "[MOCK] get_column_Json[%d] = %s\n" index (Yojson.Basic.to_string v); v
  | _ -> printf "[MOCK] get_column_Json[%d] = null (default)\n" index; `Null

let get_column_Any = get_column_Text
let get_column_Decimal = get_column_Float
let get_column_Datetime = get_column_Float

let mock_json_path = 
  try 
    Sqlgg_json_path.Json_path.parse_json_path "$.mock"
  with 
  | _ -> Sqlgg_json_path.Json_path.parse_json_path "$"

let get_column_Json_path (row : mock_row_data) (index : int) : json_path = 
  printf "[MOCK] get_column_Json_path[%d] = $.mock\n" index;
  mock_json_path

let get_column_One_or_all (row : mock_row_data) (index : int) : one_or_all = 
  printf "[MOCK] get_column_One_or_all[%d] = `One\n" index;
  `One

let get_column_Bool_nullable (row : mock_row_data) (index : int) : bool option = 
  match get_mock_value row index with
  | `Bool v -> printf "[MOCK] get_column_Bool_nullable[%d] = Some %b\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Bool_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Bool_nullable[%d] = Some false (default)\n" index; Some false

let get_column_Int_nullable (row : mock_row_data) (index : int) : int64 option = 
  match get_mock_value row index with
  | `Int v -> printf "[MOCK] get_column_Int_nullable[%d] = Some %Ld\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Int_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Int_nullable[%d] = Some 0L (default)\n" index; Some 0L

let get_column_Text_nullable (row : mock_row_data) (index : int) : string option = 
  match get_mock_value row index with
  | `Text v -> printf "[MOCK] get_column_Text_nullable[%d] = Some \"%s\"\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Text_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Text_nullable[%d] = Some \"mock_text\" (default)\n" index; Some "mock_text"

let get_column_Float_nullable (row : mock_row_data) (index : int) : float option = 
  match get_mock_value row index with
  | `Float v -> printf "[MOCK] get_column_Float_nullable[%d] = Some %f\n" index v; Some v
  | `Null -> printf "[MOCK] get_column_Float_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Float_nullable[%d] = Some 0.0 (default)\n" index; Some 0.0

let get_column_Json_nullable (row : mock_row_data) (index : int) : json option = 
  match get_mock_value row index with
  | `Json v -> printf "[MOCK] get_column_Json_nullable[%d] = Some %s\n" index (Yojson.Basic.to_string v); Some v
  | `Null -> printf "[MOCK] get_column_Json_nullable[%d] = None\n" index; None
  | _ -> printf "[MOCK] get_column_Json_nullable[%d] = Some null (default)\n" index; Some `Null

let get_column_Any_nullable = get_column_Text_nullable
let get_column_Decimal_nullable = get_column_Float_nullable
let get_column_Datetime_nullable = get_column_Float_nullable

let get_column_Json_path_nullable (row : mock_row_data) (index : int) : json_path option = 
  printf "[MOCK] get_column_Json_path_nullable[%d] = Some $.mock\n" index;
  Some mock_json_path

let get_column_One_or_all_nullable (row : mock_row_data) (index : int) : one_or_all option = 
  printf "[MOCK] get_column_One_or_all_nullable[%d] = Some `One\n" index;
  Some `One

let get_column_bool = get_column_Bool
let get_column_bool_nullable = get_column_Bool_nullable
let get_column_int64 = get_column_Int
let get_column_int64_nullable = get_column_Int_nullable
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
let get_column_json = get_column_Json
let get_column_json_nullable = get_column_Json_nullable
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

let bind_param value_str (sql, params, index, total) =
  params := value_str :: !params;
  incr index

let start_params (sql, param_count) n = 
  (sql, ref [], ref 0, n)

let finish_params (sql, params, index, total) = 
  let final_sql = substitute_params sql (List.rev !params) in
  printf "[SQL] %s\n" final_sql;
  flush stdout;
  ()

let set_param_null params = bind_param "NULL" params
let set_param_Text params v = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Any = set_param_Text
let set_param_Bool params v = bind_param (if v then "TRUE" else "FALSE") params
let set_param_Int params v = bind_param (Int64.to_string v) params
let set_param_Float params v = bind_param (Float.to_string v) params
let set_param_Decimal = set_param_Float
let set_param_Datetime = set_param_Float
let set_param_Json params v = bind_param (sprintf "'%s'" (String.escaped (Yojson.Basic.to_string v))) params
let set_param_Json_path params v = bind_param (sprintf "'%s'" (String.escaped (Sqlgg_json_path.Json_path.string_of_json_path v))) params
let set_param_One_or_all params v = bind_param (match v with `One -> "'one'" | `All -> "'all'") params

let set_param_bool = set_param_Bool
let set_param_int64 = set_param_Int
let set_param_float = set_param_Float
let set_param_decimal = set_param_Float
let set_param_string = set_param_Text
let set_param_datetime = set_param_Float
let set_param_json = set_param_Json
let set_param_json_path = set_param_Json_path
let set_param_one_or_all = set_param_One_or_all

let no_params (sql, _) = 
  printf "[SQL] %s\n" sql;
  flush stdout;
  ()

module Make_enum (E: Enum) = struct 
  include E

  let get_column (row : mock_row_data) (index : int) : E.t = E.inj "mock_enum"
  let get_column_nullable (row : mock_row_data) (index : int) : E.t option = None
  let set_param params v = bind_param (sprintf "'%s'" (E.proj v)) params
  let to_literal = E.proj
end

let select (db : string) (sql : string) (set_params : statement -> unit) (callback : mock_row_data -> unit) : unit =
  printf "[MOCK SELECT] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
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
          | `Json j -> Yojson.Basic.to_string j
          | `Null -> "NULL"
        in
        printf "col%d=%s " j val_str
      ) row.values;
      printf "\n";
      callback row
    ) rows;
    flush stdout
  | _ -> failwith "Expected MockSelect response"

let execute (db : string) (sql : string) (set_params : statement -> unit) : execute_response =
  printf "[MOCK EXECUTE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
  match get_next_response () with
  | MockExecute { affected_rows; insert_id } ->
    printf "[MOCK] Execute result: affected_rows=%Ld, insert_id=%s\n" 
      affected_rows
      (match insert_id with Some id -> Int64.to_string id | None -> "None");
    flush stdout;
    { affected_rows; insert_id }
  | _ -> failwith "Expected MockExecute response"

let select_one_maybe (db : string) (sql : string) (set_params : statement -> unit) (convert : mock_row_data -> 'a) : 'a option =
  printf "[MOCK SELECT_ONE_MAYBE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
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

let select_one (db : string) (sql : string) (set_params : statement -> unit) (convert : mock_row_data -> 'a) : 'a =
  printf "[MOCK SELECT_ONE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
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
