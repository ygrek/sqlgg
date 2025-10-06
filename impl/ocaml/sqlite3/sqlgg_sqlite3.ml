(**
  Sqlite3 OCaml traits for sqlgg
  by ygrek
  2015-07-09

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*)

open Printf
open Sqlgg_trait_types
module M = struct

module S = Sqlite3

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
  module UInt64 = struct
    type t = Unsigned.UInt64.t
    let to_literal _ = failwith "uint64 unsupported by sqlite"
    let uint64_to_literal = to_literal
  end
  module Text = struct
    type t = string

    (* cf. https://sqlite.org/lang_expr.html "Literal Values"
        "A string constant is formed by enclosing the string in single quotes
        ('). A single quote within the string can be encoded by putting two
        single quotes in a row - as in Pascal. C-style escapes using the
        backslash character are not supported because they are not standard
        SQL." *)
    let to_literal s =
      let b = Buffer.create (String.length s + String.length s / 4) in
      Buffer.add_string b "'";
      for i = 0 to String.length s - 1 do
        match String.unsafe_get s i with
        | '\'' -> Buffer.add_string b "''"
        | c -> Buffer.add_char b c
      done;
      Buffer.add_string b "'";
      Buffer.contents b
    
    let string_to_literal = to_literal
  end
  module Blob = struct
    (* "BLOB literals are string literals containing hexadecimal data and preceded
       by a single "x" or "X" character. Example: X'53514C697465'" *)
    type t = string

    let to_hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

    let to_literal s =
      let b = Buffer.create (3 + String.length s * 2) in
      Buffer.add_string b "x'";
      for i = 0 to String.length s - 1 do
        let c = Char.code (String.unsafe_get s i) in
        Buffer.add_char b (Array.unsafe_get to_hex (c lsr 4));
        Buffer.add_char b (Array.unsafe_get to_hex (c land 0x0F));
      done;
      Buffer.add_string b "'";
      Buffer.contents b
  end
  module Float = struct 
    type t = float 
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Decimal = Float
  module Datetime = Float (* ? *)
  
  module Json = struct
    type t = json

    let to_literal (x: t) = match x with
     | `Bool x -> Bool.to_literal x
     | `Intlit x -> Int.to_literal (Int64.of_string x)
     | `Int x -> Int.to_literal (Int64.of_int x)
     | `Float x -> Float.to_literal x
     | #t -> Yojson.Safe.to_string (x :> Yojson.Safe.t)
    let json_to_literal = to_literal
  end

  module Json_path = struct
    open Sqlgg_json_path
    type t = json_path
    
    let to_literal j = 
      Text.to_literal (Json_path.string_of_json_path j)
    
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

type statement = S.stmt * string * S.db
type 'a connection = S.db
type params = statement * int * int ref
type row = statement
type result = unit
type execute_response = { affected_rows: int64; insert_id: int64 option }

type num = int64
type text = string
type any = string
type datetime = float

exception Oops of string

module Conv = struct
  open S.Data
  exception Type_mismatch of t
  let () = Printexc.register_printer (function Type_mismatch x -> Some (sprintf "Conv.Type_mismatch %s" (to_string_debug x)) | _ -> None)
  let bool = "Bool", function INT i -> i <> 0L | x -> raise (Type_mismatch x)
  let int = "Int", function INT i -> i | x -> raise (Type_mismatch x)
  let text = "Text", function TEXT s | BLOB s -> s | x -> raise (Type_mismatch x)
  let float = "Float", function FLOAT x -> x | x -> raise (Type_mismatch x)
  let numeric = "Numeric", function INT i -> Int64.to_float i | FLOAT x -> x | x -> raise (Type_mismatch x)
  let decimal = "Decimal", fun x -> snd numeric x

  let datetime = "Datetime", fun x -> Float.to_string @@ snd numeric x

  let json = "Json", function
    | TEXT s -> s |> Yojson.Safe.from_string |> convert_json
    | x -> raise (Type_mismatch x)

  let json_path = "Json_path", function
    | TEXT s -> Sqlgg_json_path.Json_path.parse_json_path s
    | x -> raise (Type_mismatch x)

  let one_or_all = "One_or_all", function
    | TEXT s -> 
      (match String.lowercase_ascii s with
       | "one" -> `One
       | "all" -> `All
       | _ -> raise (Type_mismatch (TEXT s)))
    | x -> raise (Type_mismatch x)
end

let get_column_ty (name,conv) =
  begin fun (stmt,sql,_) index ->
    try conv (S.column stmt index)
    with exn -> raise (Oops (sprintf "get_column_%s %u for %s : %s" name index sql (Printexc.to_string exn)))
  end,
  begin fun (stmt,sql,_) index ->
    try match S.column stmt index with S.Data.NULL -> None | x -> Some (conv x)
    with exn -> raise (Oops (sprintf "get_column_%s_nullable %u for %s : %s" name index sql (Printexc.to_string exn)))
  end

let get_column_Bool, get_column_Bool_nullable = get_column_ty Conv.bool
let get_column_Int, get_column_Int_nullable = get_column_ty Conv.int
let get_column_UInt64, get_column_UInt64_nullable =
  ((fun _ _ -> raise (Oops "get_column_UInt64: uint64 unsupported by sqlite")),
   (fun _ _ -> raise (Oops "get_column_UInt64_nullable: uint64 unsupported by sqlite")))
let get_column_Text, get_column_Text_nullable = get_column_ty Conv.text
let get_column_Any, get_column_Any_nullable = get_column_ty Conv.text
let get_column_Float, get_column_Float_nullable = get_column_ty Conv.float
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty Conv.decimal
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty Conv.numeric
let get_column_Json, get_column_Json_nullable = get_column_ty Conv.json
let get_column_Json_path, get_column_Json_path_nullable = get_column_ty Conv.json_path
let get_column_One_or_all, get_column_One_or_all_nullable = get_column_ty Conv.one_or_all

let get_column_bool, get_column_bool_nullable = (get_column_Bool, get_column_Bool_nullable)
let get_column_int64, get_column_int64_nullable = (get_column_Int, get_column_Int_nullable)
let get_column_uint64, get_column_uint64_nullable =
  ((fun _ _ -> raise (Oops "get_column_uint64: uint64 unsupported by sqlite")),
   (fun _ _ -> raise (Oops "get_column_uint64_nullable: uint64 unsupported by sqlite")))
let get_column_float, get_column_float_nullable = (get_column_Float, get_column_Float_nullable)
let get_column_decimal, get_column_decimal_nullable = (get_column_Decimal, get_column_Decimal_nullable)
let get_column_string, get_column_string_nullable = (get_column_Text, get_column_Text_nullable)
let get_column_datetime, get_column_datetime_nullable = get_column_ty Conv.datetime
let get_column_json, get_column_json_nullable = (get_column_Json, get_column_Json_nullable)
let get_column_json_path, get_column_json_path_nullable = (get_column_Json_path, get_column_Json_path_nullable)
let get_column_one_or_all, get_column_one_or_all_nullable = (get_column_One_or_all, get_column_One_or_all_nullable)


module Make_enum (E: Enum) = struct 
  include E
  let get_column, get_column_nullable = failwith "sqlite does not support enums"
  let set_param = failwith "sqlite does not support enums"
  let to_literal = failwith "sqlite does not support enums"
end

let test_ok sql rc =
  if rc <> S.Rc.OK then
    raise (Oops (sprintf "test_ok %s for %s" (S.Rc.to_string rc) sql))

let bind_param d ((stmt,sql,_),nr_params,index) =
  assert (!index < nr_params);
  let rc = S.bind stmt (!index+1) d in
  incr index;
  test_ok sql rc

let start_params stmt n = (stmt, n, ref 0)
let finish_params (_,n,index) = assert (n = !index); ()

let set_param_null = bind_param S.Data.NULL
let set_param_Text stmt v = bind_param (S.Data.TEXT v) stmt
let set_param_Any = set_param_Text
let set_param_Bool stmt v = bind_param (S.Data.INT (if v then 1L else 0L)) stmt
let set_param_Int stmt v = bind_param (S.Data.INT v) stmt
let set_param_UInt64 _ _ = failwith "set_param_UInt64: uint64 unsupported by sqlite"
let set_param_Float stmt v = bind_param (S.Data.FLOAT v) stmt
let set_param_Decimal = set_param_Float
let set_param_Datetime = set_param_Float
let set_param_Json stmt (v: json) = match v with
  | `Bool x -> set_param_Bool stmt x
  | `Int x -> set_param_Int stmt (Int64.of_int x)
  | `Intlit x -> set_param_Int stmt (Int64.of_string x)
  | `Float x -> set_param_Float stmt x
  | #json -> set_param_Text stmt @@ Yojson.Safe.to_string (v :> Yojson.Safe.t)
let set_param_Json_path stmt v = bind_param (S.Data.TEXT (Sqlgg_json_path.Json_path.string_of_json_path v)) stmt
let set_param_One_or_all stmt v = bind_param (S.Data.TEXT (match v with `One -> "one" | `All -> "all")) stmt

let set_param_bool = set_param_Bool
let set_param_int64 = set_param_Int
let set_param_uint64 _ _ = failwith "set_param_uint64: uint64 unsupported by sqlite"
let set_param_float = set_param_Float
let set_param_decimal = set_param_Float
let set_param_string = set_param_Text
let set_param_datetime = set_param_Float
let set_param_json = set_param_Json
let set_param_json_path = set_param_Json_path
let set_param_one_or_all = set_param_One_or_all

let no_params _ = ()

let try_finally final f x =
  let r =
    try f x with exn -> final (); raise exn
  in
    final ();
    r

let close_stmt (stmt, sql, _) = test_ok sql (S.finalize stmt)

let prepare db (sql: string) = 
  let stmt = S.prepare db sql in
  (stmt, sql, db)

let with_stmt db sql f =
  let full_stmt = prepare db sql in
  try_finally
    (fun () -> close_stmt full_stmt)
    f full_stmt

(* Fixed function signatures to match Cached_m interface *)
let select_with_stmt full_stmt set_params callback =
  let (stmt, _, _) = full_stmt in
  set_params full_stmt;
  while S.Rc.ROW = S.step stmt do
    callback full_stmt
  done

let execute_with_stmt full_stmt set_params =
  let (stmt, sql, db) = full_stmt in
  set_params full_stmt;
  let rc = S.step stmt in
  if rc <> S.Rc.DONE then raise (Oops (sprintf "execute : %s" sql));
  let insert_id =
    match S.last_insert_rowid db with
    | 0L -> None
    | x -> Some x
  in
  { affected_rows = Int64.of_int (S.changes db); insert_id }

let select_one_maybe_with_stmt full_stmt set_params convert =
  let (stmt, _, _) = full_stmt in
  set_params full_stmt;
  if S.Rc.ROW = S.step stmt then
    Some (convert full_stmt)
  else
    None

let select_one_with_stmt full_stmt set_params convert =
  let (stmt, sql, _) = full_stmt in
  set_params full_stmt;
  if S.Rc.ROW = S.step stmt then
    convert full_stmt
  else
    raise (Oops (sprintf "no row but one expected : %s" sql))

let select db sql set_params callback =
  with_stmt db sql (fun stmt ->
    select_with_stmt stmt set_params callback)

let execute db sql set_params =
  with_stmt db sql (fun stmt ->
    execute_with_stmt stmt set_params)

let select_one_maybe db sql set_params convert =
  with_stmt db sql (fun stmt ->
    select_one_maybe_with_stmt stmt set_params convert)

let select_one db sql set_params convert =
  with_stmt db sql (fun stmt ->
    select_one_with_stmt stmt set_params convert)

end

let () =
  (* checking signature match *)
  let module S = (M:Sqlgg_traits.M) in
  let module Cache_config = struct 
    let max_cache_size = 500
    let ttl_seconds = None
  end in
  let module C = Sqlgg_stmt_cache.Make (Cache_config) (struct
    include M
    module IO = Sqlgg_io.Blocking
  end) in
  ignore (S.Oops "ok");
  ignore (C.Oops "ok")

include M
