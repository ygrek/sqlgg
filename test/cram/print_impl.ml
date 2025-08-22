open Printf

module M = struct

type mock_value = [ `Bool of bool | `Int of int64 | `Text of string | `Float of float | `Null ]

type mock_row_data = {
  values: (int * mock_value) array;
}

type mock_response = 
  | MockSelect of mock_row_data list
  | MockSelectOne of mock_row_data option
  | MockExecute of { affected_rows: int64; insert_id: int64 option }
  | MockError of string

let response_queue : mock_response list ref = ref []

let setup_select_response (rows : mock_row_data list) : unit =
  response_queue := MockSelect rows :: !response_queue

let setup_select_one_response (row_opt : mock_row_data option) : unit =
  response_queue := MockSelectOne row_opt :: !response_queue

let setup_execute_response ~(affected_rows : int64) ?(insert_id : int64 option = None) () : unit =
  response_queue := MockExecute { affected_rows; insert_id } :: !response_queue

let setup_error_response (error_msg : string) : unit =
  response_queue := MockError error_msg :: !response_queue

let clear_mock_responses () : unit =
  response_queue := []

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
let mock_null : mock_value = `Null

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
    type t = Sqlgg_trait_types.json

    let escape_sql_string (s : string) : string =
      let b = Buffer.create (String.length s) in
      String.iter (fun c -> if c = '\'' then (Buffer.add_char b '\''; Buffer.add_char b '\'') else Buffer.add_char b c) s;
      Buffer.contents b

    let escape_json_string (s : string) : string =
      let b = Buffer.create (String.length s) in
      String.iter (fun c ->
        match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\b' -> Buffer.add_string b "\\b"
        | '\012' -> Buffer.add_string b "\\f"
        | '\n' -> Buffer.add_string b "\\n"
        | '\r' -> Buffer.add_string b "\\r"
        | '\t' -> Buffer.add_string b "\\t"
        | c when Char.code c < 32 -> Buffer.add_string b (Printf.sprintf "\\u%04X" (Char.code c))
        | c -> Buffer.add_char b c
      ) s;
      Buffer.contents b

    let rec json_to_text (j : t) : string =
      match j with
      | `Null -> "null"
      | `Bool b -> if b then "true" else "false"
      | `Float f -> string_of_float f
      | `Int i -> string_of_int i
      | `Intlit s -> s
      | `String s -> sprintf "\"%s\"" (escape_json_string s)
      | `Assoc props ->
        let parts = List.map (fun (k, v) -> sprintf "\"%s\":%s" (escape_json_string k) (json_to_text v)) props in
        sprintf "{%s}" (String.concat "," parts)
      | `List xs ->
        let parts = List.map json_to_text xs in
        sprintf "[%s]" (String.concat "," parts)

    let to_literal (j : t) : string =
      sprintf "'%s'" (escape_sql_string (json_to_text j))

    let json_to_literal = to_literal
  end
  module Json_path = struct
    type t = Sqlgg_trait_types.json_path
    let to_literal (_ : t) = "'$'"
    let json_path_to_literal = to_literal
  end
  module One_or_all = struct
    type t = Sqlgg_trait_types.one_or_all
    let to_literal = function `One -> "'one'" | `All -> "'all'"
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

type mock_statement = {
  id : int;
  sql : string;
  mutable is_closed : bool;
}

type statement = mock_statement
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

let stmt_counter = ref 0
let prepared_statements = ref []
let closed_statements = ref []
let operation_log = ref []

let reset_mock_stats () =
  stmt_counter := 0;
  prepared_statements := [];
  closed_statements := [];
  operation_log := []

let log_operation op =
  operation_log := op :: !operation_log;
  printf "[MOCK] %s\n%!" op

let get_mock_stats () =
  let total_prepared = List.length !prepared_statements in
  let total_closed = List.length !closed_statements in
  let still_open = total_prepared - total_closed in
  let operations = List.rev !operation_log in
  printf "\n--- MOCK STATS ---\n%!";
  printf "Prepared: %d, Closed: %d, Open: %d\n%!" total_prepared total_closed still_open;
  printf "Operations:\n%!";
  List.iteri (fun i op -> printf "  %d. %s\n%!" (i+1) op) operations;
  printf "---\n%!";
  (total_prepared, total_closed, still_open, operations)

let assert_mock_stats ~expected_prepared ~expected_closed ~expected_open () =
  let (prepared, closed, open_stmts, _) = get_mock_stats () in
  if prepared <> expected_prepared then
    failwith (sprintf "Expected %d prepared, got %d" expected_prepared prepared);
  if closed <> expected_closed then
    failwith (sprintf "Expected %d closed, got %d" expected_closed closed);
  if open_stmts <> expected_open then
    failwith (sprintf "Expected %d open, got %d" expected_open open_stmts)

let snapshot_stats () = (!stmt_counter, !prepared_statements, !closed_statements, !operation_log)
let restore_stats (sc, ps, cs, ol) =
  stmt_counter := sc;
  prepared_statements := ps;
  closed_statements := cs;
  operation_log := ol

module Clock = struct
  let current_time = ref 0.0
  let now_s () = !current_time
  let travel dt = current_time := !current_time +. dt
  let set t = current_time := t
end

module IO = struct
  type 'a future = 'a
  let return x = x
  let (>>=) x f = f x
  let bracket resource cleanup f = 
    let result = f resource in
    cleanup resource;
    result
  let catch f handler =
    try f ()
    with exn -> handler exn

    let try_bind f succ_handler exc_handler =
    try
      let x = f () in
      succ_handler x
    with exn -> exc_handler exn

  let async f = ignore (f ())

  module List = struct
    let rec iter_s f = function
      | [] -> return ()
      | x :: xs ->
          f x >>= fun () ->
          iter_s f xs 
  end
end

let prepare _conn sql =
  incr stmt_counter;
  let stmt = { id = !stmt_counter; sql; is_closed = false } in
  prepared_statements := stmt :: !prepared_statements;
  log_operation (sprintf "PREPARE[%d]: %s" stmt.id sql);
  match List.find_opt (function MockError _ -> true | _ -> false) !response_queue with
  | Some (MockError msg) -> raise (Oops msg)
  | _ -> stmt

let close_stmt stmt =
  stmt.is_closed <- true;
  closed_statements := stmt :: !closed_statements;
  log_operation (sprintf "CLOSE[%d]: %s" stmt.id stmt.sql)

let select_with_stmt stmt _set_params callback =
  log_operation (sprintf "SELECT_WITH_STMT[%d]" stmt.id);
  match get_next_response () with
  | MockSelect rows ->
    printf "[MOCK] Processing %d rows in select_with_stmt\n" (List.length rows);
    List.iteri (fun i row ->
      printf "  Row %d: " i;
      Array.iteri (fun j (_, value) ->
        let val_str = match value with
          | `Bool b -> string_of_bool b
          | `Int i -> Int64.to_string i
          | `Text s -> s
          | `Float f -> string_of_float f
          | `Null -> "NULL"
        in
        printf "col%d=%s " j val_str
      ) row.values;
      printf "\n";
      callback row
    ) rows;
    flush stdout
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelect response for select_with_stmt"

let select_one_with_stmt stmt _set_params convert =
  log_operation (sprintf "SELECT_ONE_WITH_STMT[%d]" stmt.id);
  match get_next_response () with
  | MockSelectOne (Some row) ->
    printf "[MOCK] select_one_with_stmt returning one row\n";
    convert row
  | MockSelectOne None ->
    failwith "Expected one row but got none in select_one_with_stmt"
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelectOne response for select_one_with_stmt"

let select_one_maybe_with_stmt stmt _set_params convert =
  log_operation (sprintf "SELECT_ONE_MAYBE_WITH_STMT[%d]" stmt.id);
  match get_next_response () with
  | MockSelectOne (Some row) ->
    printf "[MOCK] select_one_maybe_with_stmt returning Some row\n";
    Some (convert row)
  | MockSelectOne None ->
    printf "[MOCK] select_one_maybe_with_stmt returning None\n";
    None
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelectOne response for select_one_maybe_with_stmt"

let execute_with_stmt stmt _set_params = 
  log_operation (sprintf "EXECUTE_WITH_STMT[%d]" stmt.id);
  match get_next_response () with
  | MockExecute { affected_rows; insert_id } ->
    printf "[MOCK] execute_with_stmt result: affected_rows=%Ld, insert_id=%s\n" 
      affected_rows
      (match insert_id with Some id -> Int64.to_string id | None -> "None");
    { affected_rows; insert_id }
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockExecute response for execute_with_stmt"

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

let get_column_Json (_row : mock_row_data) (index : int) : Types.Json.t =
  printf "[MOCK] get_column_Json[%d] = {} (default)\n" index; (`Assoc [])

let get_column_Json_path (_row : mock_row_data) (index : int) : Types.Json_path.t =
  printf "[MOCK] get_column_Json_path[%d] not mocked, using default '$' and failing on read\n" index;
  failwith "get_column_Json_path not mocked"

let get_column_One_or_all (_row : mock_row_data) (index : int) : Types.One_or_all.t =
  printf "[MOCK] get_column_One_or_all[%d] = `One (default)\n" index; `One

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

let get_column_Json_nullable (_row : mock_row_data) (index : int) : Types.Json.t option =
  printf "[MOCK] get_column_Json_nullable[%d] = Some {} (default)\n" index; Some (`Assoc [])

let get_column_Json_path_nullable (_row : mock_row_data) (index : int) : Types.Json_path.t option =
  printf "[MOCK] get_column_Json_path_nullable[%d] = None (default)\n" index; None

let get_column_One_or_all_nullable (_row : mock_row_data) (index : int) : Types.One_or_all.t option =
  printf "[MOCK] get_column_One_or_all_nullable[%d] = Some `One (default)\n" index; Some `One

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

let get_column_json (row : mock_row_data) (index : int) : Sqlgg_trait_types.json =
  (get_column_Json row index :> Sqlgg_trait_types.json)

let get_column_json_nullable (row : mock_row_data) (index : int) : Sqlgg_trait_types.json option =
  (get_column_Json_nullable row index :> Sqlgg_trait_types.json option)

let get_column_json_path (_row : mock_row_data) (_index : int) : Sqlgg_trait_types.json_path =
  failwith "get_column_json_path not mocked"

let get_column_json_path_nullable (_row : mock_row_data) (index : int) : Sqlgg_trait_types.json_path option =
  printf "[MOCK] get_column_json_path_nullable[%d] = None (default)\n" index; None

let get_column_one_or_all (_row : mock_row_data) (index : int) : Sqlgg_trait_types.one_or_all =
  printf "[MOCK] get_column_one_or_all[%d] = `One (default)\n" index; `One

let get_column_one_or_all_nullable (_row : mock_row_data) (index : int) : Sqlgg_trait_types.one_or_all option =
  printf "[MOCK] get_column_one_or_all_nullable[%d] = Some `One (default)\n" index; Some `One

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
  let (_, param_list, index, _) = params in
  param_list := value_str :: !param_list;
  incr index

let start_params (stmt : statement) (n : int) : params = 
  (stmt, ref [], ref 0, n)

let finish_params (params : params) : result = 
  let (stmt, param_list, _, _) = params in
  let final_sql = substitute_params stmt.sql (List.rev !param_list) in
  printf "[SQL] %s\n" final_sql;
  flush stdout;
  ()

let set_param_null (params : params) = bind_param "NULL" params
let set_param_Text (params : params) (v : Types.Text.t) = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Any (params : params) (v : Types.Any.t) = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Bool (params : params) (v : Types.Bool.t) = bind_param (if v then "TRUE" else "FALSE") params
let set_param_Int (params : params) (v : Types.Int.t) = bind_param (Int64.to_string v) params
let set_param_Float (params : params) (v : Types.Float.t) = bind_param (Float.to_string v) params
let set_param_Decimal (params : params) (v : Types.Decimal.t) = bind_param (Float.to_string v) params
let set_param_Datetime (params : params) (v : Types.Datetime.t) = bind_param (Float.to_string v) params

let set_param_Json (params : params) (v : Types.Json.t) = bind_param (Types.Json.to_literal v) params
let set_param_Json_path (params : params) (v : Types.Json_path.t) = bind_param (Types.Json_path.to_literal v) params
let set_param_One_or_all (params : params) (v : Types.One_or_all.t) = bind_param (Types.One_or_all.to_literal v) params

let set_param_bool (params : params) v = bind_param (if v then "TRUE" else "FALSE") params
let set_param_int64 (params : params) v = bind_param (Int64.to_string v) params
let set_param_float (params : params) v = bind_param (Float.to_string v) params
let set_param_decimal (params : params) v = bind_param (Float.to_string v) params
let set_param_string (params : params) v = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_datetime (params : params) v = bind_param (Float.to_string v) params

let set_param_json (params : params) (v : Sqlgg_trait_types.json) = set_param_Json params v
let set_param_json_path (params : params) (_v : Sqlgg_trait_types.json_path) = bind_param "'$'" params
let set_param_one_or_all (params : params) (v : Sqlgg_trait_types.one_or_all) = set_param_One_or_all params v

let no_params (stmt : statement) : result = 
  printf "[SQL] %s\n" stmt.sql;
  flush stdout;
  ()

module Make_enum (E: Enum) = struct 
  let get_column (_ : mock_row_data) (_ : int) : E.t = E.inj "mock_enum"
  let get_column_nullable (_ : mock_row_data) (_ : int) : E.t option = Some (E.inj "mock_enum")
  let set_param (params : params) v = bind_param (sprintf "'%s'" (E.proj v)) params
  let to_literal = E.proj
end

let select (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (callback : row -> unit) : unit =
  printf "[MOCK SELECT] Connection type: [> `RO ]\n";
  let stmt = prepare db sql in
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
          | `Null -> "NULL"
        in
        printf "col%d=%s " j val_str
      ) row.values;
      printf "\n";
      callback row
    ) rows;
    flush stdout
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelect response"

let execute (db : [> `WR ] connection) (sql : string) (set_params : statement -> result) : execute_response =
  printf "[MOCK EXECUTE] Connection type: [> `WR ]\n";
  let stmt = prepare db sql in
  let _ = set_params stmt in
  match get_next_response () with
  | MockExecute { affected_rows; insert_id } ->
    printf "[MOCK] Execute result: affected_rows=%Ld, insert_id=%s\n" 
      affected_rows
      (match insert_id with Some id -> Int64.to_string id | None -> "None");
    flush stdout;
    { affected_rows; insert_id }
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockExecute response"

let select_one_maybe (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (convert : row -> 'a) : 'a option =
  printf "[MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]\n";
  let stmt = prepare db sql in
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
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelectOne response"

let select_one (db : [> `RO ] connection) (sql : string) (set_params : statement -> result) (convert : row -> 'a) : 'a =
  printf "[MOCK SELECT_ONE] Connection type: [> `RO ]\n";
  let stmt = prepare db sql in
  let _ = set_params stmt in
  match get_next_response () with
  | MockSelectOne (Some row) ->
    printf "[MOCK] Returning one row\n";
    flush stdout;
    convert row
  | MockSelectOne None ->
    failwith "Expected one row but got none"
  | MockError msg -> raise (Oops msg)
  | _ -> failwith "Expected MockSelectOne response"

end

let () =
  let module Test = (M : Sqlgg_traits.M_io) in 
  ignore (Test.Oops "Simple mock ready")

include M
