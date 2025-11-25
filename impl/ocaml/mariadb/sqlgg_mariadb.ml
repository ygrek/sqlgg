(**
  Mariadb OCaml traits for sqlgg
  by Raman Varabets
  2018-12-19

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*)

open Printf
open Sqlgg_trait_types

module type Value = sig
  type t
  type field
  type value
  val of_field : field -> t
  val to_value : t -> value
  val to_literal : t -> string
end

module type Enum = sig 
  type t

  val inj: string -> t

  val proj: t -> string
end

module type Types = sig
  type field
  type value
  module type Value = Value with type field = field and type value = value
  module Bool : sig 
    include Value 
    val get_bool : field -> bool
    val set_bool: bool -> value
    val bool_to_literal : bool -> string
  end
  module Int: sig 
    include Value
    val get_int64 : field -> int64
    val set_int64: int64 -> value
    val int64_to_literal : int64 -> string
    val of_int : int -> int64
  end
  module UInt64 : sig 
    include Value
    val get_uint64 : field -> Unsigned.UInt64.t
    val set_uint64: Unsigned.UInt64.t -> value
    val uint64_to_literal : Unsigned.UInt64.t -> string
    val of_int : int -> Unsigned.UInt64.t
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Float : sig
    include Value
    val get_float : field -> float
    val set_float: float -> value
    val float_to_literal : float -> string
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Text : sig 
    include Value
    val get_string : field -> string
    val set_string: string -> value
    val string_to_literal : string -> string
  end
  module Blob : sig
    include Value
    val get_string : field -> string
  end
  module Datetime : sig 
    include Value
    val get_string : field -> string
    val set_float: float -> value
    val float_to_literal : float -> string
  end
  module Decimal : sig 
    include Value
    val get_float : field -> float
    val set_float: float -> value
    val float_to_literal : float -> string
  end
  module Json : sig
    include Value
    val get_json : field -> json
    val set_json: json -> value
    val json_to_literal : json -> string
  end
  module Json_path : sig
    include Value
    val get_json_path : field -> json_path
    val set_json_path: json_path -> value
    val json_path_to_literal : json_path -> string
  end
  module One_or_all : sig
    include Value
    val to_literal : one_or_all -> string
    val get_one_or_all : field -> one_or_all
    val set_one_or_all: one_or_all -> value
    val one_or_all_to_literal : one_or_all -> string
  end
  module Any : Value
  module Make_enum : functor (E : Enum) -> Value with type t = E.t
end

module Default_types(M : Mariadb.Nonblocking.S) : Types with
  type field = M.Field.t and
  type value = M.Field.value and
  type Bool.t = bool and
  type Int.t = int64 and
  type UInt64.t = Unsigned.UInt64.t and
  type Text.t = string and
  type Blob.t = string and
  type Float.t = float and
  type Decimal.t = float and
  type Datetime.t = M.Time.t and
  type Json.t = json and
  type Json_path.t = json_path and
  type One_or_all.t = one_or_all and
  type Any.t = M.Field.value =
struct
  type field = M.Field.t
  type value = M.Field.value
  module type Value = Value with type field = field and type value = value
  module Make(T : sig type t val of_field : field -> t val to_value : t -> value val to_literal : t -> string end) : Value with
    type t = T.t =
  struct
    type t = T.t
    type nonrec field = field
    type nonrec value = value
    let of_field = T.of_field
    let to_value = T.to_value
    let to_literal = T.to_literal
  end

  let convfail expected field value =
    let found =
      match value with
      | `Null -> "null"
      | `Int x -> sprintf "int %d" x
      | `Int64 x -> sprintf "int64 %Ld" x
      | `UInt64 x -> sprintf "uint64 %s" (Unsigned.UInt64.to_string x)
      | `Float x -> sprintf "float %f" x
      | `String x -> sprintf "string %S" x
      | `Bytes x -> sprintf "bytes %S" (Bytes.to_string x)
      | `Decimal x -> sprintf "decimal %S" x
      | `Json x -> sprintf "json %S" x
      | `Time x ->
      let open M.Time in
      sprintf "time %04d-%02d-%02d %02d:%02d:%02d.%03d" (year x) (month x) (day x) (hour x) (minute x) (second x) (microsecond x)
    in
    ksprintf failwith "expected %s %s, but found %s" expected (M.Field.name field) found

  module Int = struct
    include Make(struct
      type t = int64
      let of_field field =
        let s =
          match M.Field.value field with
          | `Int x -> Int.to_string x
          | `Int64 x -> Int64.to_string x
          | `String s -> s
          | `UInt64 x -> Unsigned.UInt64.to_string x (* Unsigned.UInt64.to_int64 simply changes sign in case of overflow *)
          | value -> convfail "int64" field value
        in
        Int64.of_string s
      let to_value x = `Int64 x
      let to_literal = Int64.to_string
    end)
    let get_int64 = of_field
    let set_int64 = to_value
    let int64_to_literal = to_literal
    let of_int = Int64.of_int
  end

  module UInt64 = struct
    include Make(struct
      type t = Unsigned.UInt64.t
      let of_field field =
        match M.Field.value field with
        | `UInt64 x -> x
        | `Int64 x -> Unsigned.UInt64.of_int64 x
        | `Int x -> Unsigned.UInt64.of_int x
        | `String x -> Unsigned.UInt64.of_string x
        | value -> convfail "uint64" field value
      let to_value x = `UInt64 x
      let to_literal x = Unsigned.UInt64.to_string x
    end) 
    let get_uint64 = of_field
    let set_uint64 = to_value
    let uint64_to_literal = to_literal
    let of_int = Unsigned.UInt64.of_int
  end

  module Bool = struct 
    include Make(struct
      type t = bool
      let of_field field = Int.of_field field <> 0L
      let to_value = function true -> `Int 1 | false -> `Int 0
      let to_literal = string_of_bool
    end)
    let get_bool = of_field
    let set_bool = to_value
    let bool_to_literal = to_literal
  end

  module Float = struct 
    include Make(struct
      type t = float
      let of_field field =
        match M.Field.value field with
        | `Int x -> float_of_int x
        | `Float x -> x
        | `String x -> float_of_string x
        | value -> convfail "float" field value
      let to_value x = `Float x
      let to_literal = string_of_float
    end)
    let get_float = of_field
    let set_float = to_value
    let float_to_literal = to_literal
  end

  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Decimal = Float

  module Text = struct 
    include Make(struct
      type t = string
      let of_field field =
        match M.Field.value field with
        | `String x -> x
        | `Bytes x -> Bytes.to_string x
        | value -> convfail "string" field value
      let to_value x = `String x

      (* cf. https://dev.mysql.com/doc/refman/5.7/en/string-literals.html *)
      let to_literal s =
        let b = Buffer.create (String.length s + String.length s / 4) in
        Buffer.add_string b "'";
        for i = 0 to String.length s - 1 do
          match String.unsafe_get s i with
          | '\\' -> Buffer.add_string b "\\\\"
          | '\000' -> Buffer.add_string b "\\0"
          | '\'' -> Buffer.add_string b "\\'"
          | c -> Buffer.add_char b c
        done;
        Buffer.add_string b "'";
        Buffer.contents b
    end) 
    let get_string = of_field
    let set_string = to_value
    let string_to_literal = to_literal
  end

  module Blob = struct 
    include Make(struct
      (* https://dev.mysql.com/doc/refman/5.7/en/hexadecimal-literals.html
        "Hexadecimal literal values are written using X'val' or 0xval notation,
        where val contains hexadecimal digits (0..9, A..F)."
        "By default, a hexadecimal literal is a binary string, where each pair of
        hexadecimal digits represents a character" *)
      type t = string

      let of_field = Text.of_field
      let to_value x = `Bytes (Bytes.of_string x)

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
    end)
    let get_string = of_field
  end

  module Datetime = struct 

    include Make(struct
      type t = M.Time.t
      let of_field = M.Field.time
      let to_value x = `Time x
      let to_literal t =
        sprintf
          "'%04d-%02d-%02d %02d:%02d:%02d.%06d'"
          (M.Time.year t)
          (M.Time.month t)
          (M.Time.day t)
          (M.Time.hour t)
          (M.Time.minute t)
          (M.Time.second t)
          (M.Time.microsecond t)
    end)

    let get_string f = f |> of_field |> to_literal
    let set_float t = `Time (M.Time.utc_timestamp t)
    let float_to_literal t = t |> M.Time.utc_timestamp |> to_literal
  end

  module Json = struct
    include Make (struct
      type t = Sqlgg_trait_types.json

      let handle_with_json v = function
        | `String x -> Yojson.Safe.from_string x
        | `Bytes x -> Yojson.Safe.from_string (Bytes.to_string x)
        | #M.Field.value as value -> convfail "json" v value

      let to_literal (x: t) = Text.to_literal (Yojson.Safe.to_string (x :> Yojson.Safe.t))

      let of_field field = convert_json @@ handle_with_json field (M.Field.value field )
      let to_value (x: json) = match x with
        | `Bool x -> Bool.to_value x
        | `Int x -> Int.to_value (Int64.of_int x)
        | `Intlit x -> Int.to_value (Int64.of_string x)
        | `Float x -> Float.to_value x
        | #json -> `String (Yojson.Safe.to_string (x :> Yojson.Safe.t))
    end)

    let get_json = of_field
    let set_json = to_value
    let json_to_literal = to_literal
  end

  module Json_path = struct

    open Sqlgg_json_path

    include Make(struct
      type t = json_path
      
      let of_field field =
        match M.Field.value field with
        | `String x -> Json_path.parse_json_path x
        | value -> convfail "json_path" field value
      
      let to_value x = `String (Json_path.string_of_json_path x)
      
      let to_literal x = Text.to_literal (Json_path.string_of_json_path x)
    end)

    let get_json_path = of_field
    let set_json_path = to_value
    let json_path_to_literal = to_literal
  end

  module One_or_all = struct
  include Make(struct
    type t = one_or_all
    let of_field field =
      match M.Field.value field with
      | `String s -> 
        (match String.lowercase_ascii s with
         | "one" -> `One
         | "all" -> `All
         | _ -> convfail "one_or_all" field (`String s))
      | value -> convfail "one_or_all" field value
    let to_value = function `One -> `String "one" | `All -> `String "all"
    let to_literal = function `One -> "one" | `All -> "all"
  end)

  let get_one_or_all = of_field
  let set_one_or_all = to_value
  let one_or_all_to_literal = to_literal
end

  module Any = Make(struct
    type t = M.Field.value
    let of_field = M.Field.value
    let to_value x = x
    let to_literal _ = failwith "to_literal Any"
  end)

  module Make_enum (E: Enum) = Make(struct
  
    include E
  
    let of_field field = 
      match M.Field.value field with
      | `String x -> inj x
      | value -> convfail "enum" field value
  
    let to_value v = `String (proj v)
  
    let to_literal v = Text.to_literal (proj v)
  end)
end

module Make
  (IO : Sqlgg_io.M)
  (M : Mariadb.Nonblocking.S with type 'a future = 'a IO.future)
  (Types : Types with type field = M.Field.t and type value = M.Field.value) =
struct

module IO = IO

type statement = M.Stmt.t
type 'a connection = M.t
type params = statement * M.Field.value array * int ref
type row = M.Field.t array
type result = M.Res.t
type execute_response = { affected_rows: int64; insert_id: int64 option }

module Types = Types

open Types

type num = Int.t
type text = Text.t
type any = Any.t
type datetime = Datetime.t

exception Oops of string

let oops fmt = ksprintf (fun s -> raise (Oops s)) fmt

let check = function Error (code, msg) -> oops "(%d) %s" code msg | Ok r -> IO.return r

let get_column_ty name conv =
  begin fun row index ->
    try
      conv (row.(index))
    with
      e -> oops "get_column_%s %i (%s)" name index (Printexc.to_string e)
  end,
  begin fun row index ->
    try
      let x = row.(index) in
      if M.Field.null_value x then None else Some (conv x)
    with
      e -> oops "get_column_%s_nullable %i (%s)" name index (Printexc.to_string e)
  end

let get_column_Bool, get_column_Bool_nullable = get_column_ty "Bool" Bool.of_field
let get_column_Int, get_column_Int_nullable = get_column_ty "Int" Int.of_field
let get_column_UInt64, get_column_UInt64_nullable = get_column_ty "UInt64" UInt64.of_field
let get_column_Text, get_column_Text_nullable = get_column_ty "Text" Text.of_field
let get_column_Float, get_column_Float_nullable = get_column_ty "Float" Float.of_field
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty "Decimal" Decimal.of_field
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty "Datetime" Datetime.of_field
let get_column_Any, get_column_Any_nullable = get_column_ty "Any" Any.of_field
let get_column_Json, get_column_Json_nullable = get_column_ty "Json" Json.of_field
let get_column_Json_path, get_column_Json_path_nullable = get_column_ty "Json_path" Json_path.of_field
let get_column_One_or_all, get_column_One_or_all_nullable = get_column_ty "One_or_all" One_or_all.of_field

let get_column_bool, get_column_bool_nullable = get_column_ty "bool" Bool.get_bool
let get_column_int64, get_column_int64_nullable = get_column_ty "int64" Int.get_int64
let get_column_uint64, get_column_uint64_nullable = get_column_ty "uint64" UInt64.get_uint64
let get_column_float, get_column_float_nullable = get_column_ty "float" Float.get_float
let get_column_decimal, get_column_decimal_nullable = get_column_ty "float" Decimal.get_float
let get_column_datetime, get_column_datetime_nullable = get_column_ty "string" Datetime.get_string
let get_column_string, get_column_string_nullable = get_column_ty "string" Text.get_string
let get_column_json, get_column_json_nullable = get_column_ty "json" Json.get_json
let get_column_json_path, get_column_json_path_nullable = get_column_ty "json_path" Json_path.get_json_path
let get_column_one_or_all, get_column_one_or_all_nullable = get_column_ty "one_or_all" One_or_all.get_one_or_all


let bind_param data (_, params, index) = assert (!index < Array.length params); params.(!index) <- data; incr index

let start_params stmt n = (stmt, Array.make n `Null, ref 0)
let finish_params (stmt, params, index) =
  assert (!index = Array.length params);
  let open IO in
  M.Stmt.execute stmt params >>=
  check

let set_param_ty f = fun (p: params) v -> bind_param (f v) p

let set_param_null stmt = bind_param `Null stmt
let set_param_Text = set_param_ty Text.to_value
let set_param_Any = set_param_ty Any.to_value
let set_param_Bool = set_param_ty Bool.to_value
let set_param_Int = set_param_ty Int.to_value
let set_param_UInt64 = set_param_ty UInt64.to_value
let set_param_Float = set_param_ty Float.to_value
let set_param_Decimal = set_param_ty Decimal.to_value
let set_param_Datetime = set_param_ty Datetime.to_value
let set_param_Json = set_param_ty Json.to_value
let set_param_Json_path = set_param_ty Json_path.to_value
let set_param_One_or_all = set_param_ty One_or_all.to_value


let set_param_bool = set_param_ty Bool.set_bool
let set_param_int64 = set_param_ty Int.set_int64
let set_param_uint64 = set_param_ty UInt64.set_uint64
let set_param_string = set_param_ty Text.set_string
let set_param_float = set_param_ty Float.set_float
let set_param_decimal = set_param_ty Decimal.set_float
let set_param_datetime = set_param_ty Datetime.set_float
let set_param_json = set_param_ty Json.set_json
let set_param_json_path = set_param_ty Json_path.set_json_path
let set_param_one_or_all = set_param_ty One_or_all.set_one_or_all

module Make_enum (E: Enum) = struct 

  module E = Make_enum(E)

  type t = E.t

  let get_column, get_column_nullable = get_column_ty "Enum" E.of_field

  let set_param = set_param_ty E.to_value

  let to_literal = E.to_literal
end

let no_params stmt =
  let open IO in
  M.Stmt.execute stmt [||] >>=
  check

let close_stmt stmt = let open IO in M.Stmt.close stmt >>= fun _ -> return ()

let with_stmt db sql f =
  let open IO in
  M.prepare db sql >>=
  check >>=
  fun stmt -> bracket (return stmt) close_stmt f

let row_array = (module M.Row.Array : M.Row.S with type t = M.Field.t array)

let select_with_stmt stmt set_params callback =
  let open IO in
  let rec loop r =
    M.Res.fetch row_array r >>=
    check >>=
    function
    | Some row -> callback row; loop r
    | None -> return ()
  in
  set_params stmt >>=
  loop

let select_one_maybe_with_stmt stmt set_params convert =
  let open IO in
  set_params stmt >>=
  M.Res.fetch row_array >>=
  check >>= function
  | Some row -> return (Some (convert row))
  | None -> return None

let select_one_with_stmt stmt set_params convert =
  let open IO in
  set_params stmt >>=
  M.Res.fetch row_array >>=
  check >>= function
  | Some row -> IO.return (convert row)
  | None -> oops "no row but one expected"

let execute_with_stmt stmt set_params =
  let open IO in
  set_params stmt >>=
  fun res -> 
    let insert_id =
      match M.Res.insert_id res with
      | 0 -> None
      | x -> Some (Int64.of_int x)
    in
    return { affected_rows = Int64.of_int (M.Res.affected_rows res); insert_id }

let select db sql set_params callback =
  with_stmt db sql (fun stmt -> select_with_stmt stmt set_params callback)

let select_one_maybe db sql set_params convert =
  with_stmt db sql (fun stmt -> select_one_maybe_with_stmt stmt set_params convert)

let select_one db sql set_params convert =
  with_stmt db sql (fun stmt -> select_one_with_stmt stmt set_params convert)

let execute db sql set_params =
  with_stmt db sql (fun stmt -> execute_with_stmt stmt set_params)

let prepare db sql =
  let open IO in
  M.prepare db sql >>=
  check
end

module Default(IO : Sqlgg_io.M)(M : Mariadb.Nonblocking.S with type 'a future = 'a IO.future) =
  Make(IO)(M)(Default_types(M))

module Make_control_io
  (IO' : Sqlgg_io.M_control)
  (M : Mariadb.Nonblocking.S with type 'a future = 'a IO'.future)
  (Types : Types with type field = M.Field.t and type value = M.Field.value) = struct
    include Make(IO')(M)(Types)
    module IO = IO'
  end

module Default_cached
  (Config : Sqlgg_stmt_cache.Cache_config)
  (IO' : Sqlgg_io.M_control)
  (M : Mariadb.Nonblocking.S with type 'a future = 'a IO'.future) =
  Sqlgg_stmt_cache.Make(Config)(Make_control_io(IO')(M)(Default_types(M)))

let () =
  (* checking signature match *)
  let module B = struct include Mariadb.Blocking type 'a future = 'a end in
  let module Default_blocking = Default(Sqlgg_io.Blocking)(B) in
  let module Cache_config = struct
    let max_cache_size = 500
    let ttl_seconds = None
  end in
  let module Default_blocking_cached = Default_cached(Cache_config)(Sqlgg_io.Blocking)(B) in
  ignore (Default_blocking.Oops "ok");
  ignore (Default_blocking_cached.Oops "ok")
