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
  module Bool : Value
  module Int : Value
  module Float : Value
  module Text : Value
  module Blob : Value
  module Datetime : Value
  module Decimal : Value
  module Any : Value
  module Make_enum : functor (E : Enum) -> Value with type t = E.t
end

module Default_types(M : Mariadb.Nonblocking.S) : Types with
  type field = M.Field.t and
  type value = M.Field.value and
  type Bool.t = bool and
  type Int.t = int64 and
  type Text.t = string and
  type Blob.t = string and
  type Float.t = float and
  type Decimal.t = float and
  type Datetime.t = M.Time.t and
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
      | `Float x -> sprintf "float %f" x
      | `String x -> sprintf "string %S" x
      | `Bytes x -> sprintf "bytes %S" (Bytes.to_string x)
      | `Decimal x -> sprintf "decimal %S" x
      | `Time x ->
      let open M.Time in
      sprintf "time %04d-%02d-%02d %02d:%02d:%02d.%03d" (year x) (month x) (day x) (hour x) (minute x) (second x) (microsecond x)
    in
    ksprintf failwith "expected %s %s, but found %s" expected (M.Field.name field) found

  module Int = Make(struct
    type t = int64
    let of_field field =
      match M.Field.value field with
      | `Int x -> Int64.of_int x
      | `String x -> Int64.of_string x
      | value -> convfail "int" field value
    let to_value x = `Int (Int64.to_int x)
    let to_literal = Int64.to_string
  end)

  module Bool = Make(struct
    type t = bool
    let of_field field = Int.of_field field <> 0L
    let to_value = function true -> `Int 1 | false -> `Int 0
    let to_literal = string_of_bool
  end)

  module Float = Make(struct
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

  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Decimal = Float

  module Text = Make(struct
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

  module Blob = Make(struct
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

  module Datetime = Make(struct
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
  
    let to_literal = proj
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
type execute_response = { affected_rows: int64; insert_id: int64 }
type maybe_insert_response = { affected_rows: int64; insert_id: int64 option }

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
let get_column_Text, get_column_Text_nullable = get_column_ty "Text" Text.of_field
let get_column_Float, get_column_Float_nullable = get_column_ty "Float" Float.of_field
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty "Decimal" Decimal.of_field
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty "Datetime" Datetime.of_field
let get_column_Any, get_column_Any_nullable = get_column_ty "Any" Any.of_field

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
let set_param_Float = set_param_ty Float.to_value
let set_param_Decimal = set_param_ty Decimal.to_value
let set_param_Datetime = set_param_ty Datetime.to_value

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

let with_stmt db sql f =
  let open IO in
  let close stmt = M.Stmt.close stmt >>= fun _ -> return () in
  M.prepare db sql >>=
  check >>=
  fun stmt -> bracket (return stmt) close f

let row_array = (module M.Row.Array : M.Row.S with type t = M.Field.t array)

let select db sql set_params callback =
  with_stmt db sql @@ fun stmt ->
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

let execute db sql set_params =
  with_stmt db sql @@ fun stmt ->
  let open IO in
  set_params stmt >>=
  fun res -> 
    let insert_id =
      match M.Res.insert_id res with
      | 0 -> None
      | x -> Some (Int64.of_int x)
    in
    return { affected_rows = Int64.of_int (M.Res.affected_rows res); insert_id }

let select_one_maybe db sql set_params convert =
  with_stmt db sql @@ fun stmt ->
  let open IO in
  set_params stmt >>=
  M.Res.fetch row_array >>=
  check >>= function
  | Some row -> return (Some (convert row))
  | None -> return None

let select_one db sql set_params convert =
  with_stmt db sql @@ fun stmt ->
  let open IO in
  set_params stmt >>=
  M.Res.fetch row_array >>=
  check >>= function
  | Some row -> IO.return (convert row)
  | None -> oops "no row but one expected : %s" sql

end

module Default(IO : Sqlgg_io.M)(M : Mariadb.Nonblocking.S with type 'a future = 'a IO.future) =
  Make(IO)(M)(Default_types(M))

let () =
  (* checking signature match *)
  let module Default_blocking : Sqlgg_traits.M = Default(Sqlgg_io.Blocking)(struct include Mariadb.Blocking type 'a future = 'a end) in
  ignore (Default_blocking.Oops "ok")
