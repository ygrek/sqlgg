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

module M = struct

module S = Sqlite3

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
    type t = Yojson.Basic.t

    let to_literal j = 
      Text.to_literal (Yojson.Basic.to_string j)

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

type statement = S.stmt * string
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
  let decimal = "Decimal", function INT i -> Int64.to_float i | FLOAT x -> x | x -> raise (Type_mismatch x)

  let datetime = "Datetime", fun x -> Float.to_string @@ snd float x

  let json = "Json", function
    | TEXT s -> Yojson.Basic.from_string s
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
  begin fun (stmt,sql) index ->
    try conv (S.column stmt index)
    with exn -> raise (Oops (sprintf "get_column_%s %u for %s : %s" name index sql (Printexc.to_string exn)))
  end,
  begin fun (stmt,sql) index ->
    try match S.column stmt index with S.Data.NULL -> None | x -> Some (conv x)
    with exn -> raise (Oops (sprintf "get_column_%s_nullable %u for %s : %s" name index sql (Printexc.to_string exn)))
  end

let get_column_Bool, get_column_Bool_nullable = get_column_ty Conv.bool
let get_column_Int, get_column_Int_nullable = get_column_ty Conv.int
let get_column_Text, get_column_Text_nullable = get_column_ty Conv.text
let get_column_Any, get_column_Any_nullable = get_column_ty Conv.text
let get_column_Float, get_column_Float_nullable = get_column_ty Conv.float
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty Conv.decimal
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty Conv.float
let get_column_Json, get_column_Json_nullable = get_column_ty Conv.json
let get_column_Json_path, get_column_Json_path_nullable = get_column_ty Conv.json_path
let get_column_One_or_all, get_column_One_or_all_nullable = get_column_ty Conv.one_or_all

let get_column_bool, get_column_bool_nullable = (get_column_Bool, get_column_Bool_nullable)
let get_column_int64, get_column_int64_nullable = (get_column_Int, get_column_Int_nullable)
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

let bind_param d ((stmt,sql),nr_params,index) =
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
let set_param_Float stmt v = bind_param (S.Data.FLOAT v) stmt
let set_param_Decimal = set_param_Float
let set_param_Datetime = set_param_Float
let set_param_Json stmt v = bind_param (S.Data.TEXT (Yojson.Basic.to_string v)) stmt
let set_param_Json_path stmt v = bind_param (S.Data.TEXT (Sqlgg_json_path.Json_path.string_of_json_path v)) stmt
let set_param_One_or_all stmt v = bind_param (S.Data.TEXT (match v with `One -> "one" | `All -> "all")) stmt

let set_param_bool = set_param_Bool
let set_param_int64 = set_param_Int
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

let with_sql db sql f =
  let stmt = S.prepare db sql in
  try_finally
    (fun () -> test_ok sql (S.finalize stmt))
    f (stmt,sql)

let select db sql set_params callback =
  with_sql db sql (fun stmt ->
    set_params stmt;
    while S.Rc.ROW = S.step (fst stmt) do
      callback stmt
    done)

let execute db sql set_params =
  with_sql db sql (fun stmt ->
    set_params stmt;
    let rc = S.step (fst stmt) in
    if rc <> S.Rc.DONE then raise (Oops (sprintf "execute : %s" sql));
    let insert_id =
      match S.last_insert_rowid db with
      | 0L -> None
      | x -> Some x
    in
    { affected_rows = Int64.of_int (S.changes db); insert_id; }
  )

let select_one_maybe db sql set_params convert =
  with_sql db sql (fun stmt ->
    set_params stmt;
    if S.Rc.ROW = S.step (fst stmt) then
      Some (convert stmt)
    else
      None)

let select_one db sql set_params convert =
  with_sql db sql (fun stmt ->
    set_params stmt;
    if S.Rc.ROW = S.step (fst stmt) then
      convert stmt
    else
      raise (Oops (sprintf "no row but one expected : %s" sql)))

end

let () =
  (* checking signature match *)
  let module S = (M:Sqlgg_traits.M) in ignore (S.Oops "ok")

include M
