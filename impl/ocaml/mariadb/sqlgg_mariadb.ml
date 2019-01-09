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
end

module type Types = sig
  type field
  type value
  module type Value = Value with type field = field and type value = value
  module Bool : Value
  module Int : Value
  module Float : Value
  module Text : Value
  module Datetime : Value
  module Any : Value
end

module Default_types(M : Mariadb.Nonblocking.S) : Types with
  type field = M.Field.t and
  type value = M.Field.value and
  type Bool.t = bool and
  type Int.t = int64 and
  type Text.t = string and
  type Float.t = float and
  type Datetime.t = M.Time.t and
  type Any.t = M.Field.value =
struct
  type field = M.Field.t
  type value = M.Field.value
  module type Value = Value with type field = field and type value = value
  module Make(T : sig type t val of_field : field -> t val to_value : t -> value end) : Value with
    type t = T.t =
  struct
    type t = T.t
    type nonrec field = field
    type nonrec value = value
    let of_field = T.of_field
    let to_value = T.to_value
  end

  let convfail expected field value =
    let found =
      match value with
      | `Null -> "null"
      | `Int x -> sprintf "int %d" x
      | `Float x -> sprintf "float %f" x
      | `String x -> sprintf "string %S" x
      | `Bytes x -> sprintf "bytes %S" (Bytes.to_string x)
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
  end)

  module Bool = Make(struct
    type t = bool
    let of_field field = Int.of_field field <> 0L
    let to_value = function true -> `Int 1 | false -> `Int 0
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
  end)

  module Text = Make(struct
    type t = string
    let of_field field =
      match M.Field.value field with
      | `String x -> x
      | `Bytes x -> Bytes.to_string x
      | value -> convfail "string" field value
    let to_value x = `String x
  end)

  module Datetime = Make(struct
    type t = M.Time.t
    let of_field = M.Field.time
    let to_value x = `Time x
  end)

  module Any = Make(struct
    type t = M.Field.value
    let of_field = M.Field.value
    let to_value x = x
  end)
end

module Make
  (IO : Sqlgg_io.M)
  (M : Mariadb.Nonblocking.S with type 'a future = 'a IO.future)
  (Types : Types with type field = M.Field.t and type value = M.Field.value) =
struct

module IO = IO

type statement = M.Stmt.t
type connection = M.t
type params = statement * M.Field.value array
type row = M.Field.t array
type result = M.Res.t

module Types = Types

open Types

type num = Int.t
type text = Text.t
type any = Any.t
type datetime = Datetime.t

exception Oops of string

let oops fmt = ksprintf (fun s -> raise (Oops s)) fmt

let check = function Error (code, msg) -> oops "(%d) %s" code msg | Ok r -> IO.return r

let get_column_ty name conv row index =
  try
    conv (row.(index))
  with
    e -> oops "get_column_%s %i (%s)" name index (Printexc.to_string e)

let get_column_Bool = get_column_ty "Bool" Bool.of_field
let get_column_Int = get_column_ty "Int" Int.of_field
let get_column_Text = get_column_ty "Text" Text.of_field
let get_column_Float = get_column_ty "Float" Float.of_field
let get_column_Datetime = get_column_ty "Datetime" Datetime.of_field
let get_column_Any = get_column_ty "Any" Any.of_field

let bind_param data (_, params) index = params.(index) <- data

let start_params stmt n = (stmt, Array.make n `Null)
let finish_params (stmt, params) =
  let open IO in
  M.Stmt.execute stmt params >>=
  check

let set_param_ty f = fun (p: params) index v -> bind_param (f v) p index

let set_param_null stmt index = bind_param `Null stmt index
let set_param_Text = set_param_ty Text.to_value
let set_param_Any = set_param_ty Any.to_value
let set_param_Bool = set_param_ty Bool.to_value
let set_param_Int = set_param_ty Int.to_value
let set_param_Float = set_param_ty Float.to_value
let set_param_Datetime = set_param_ty Datetime.to_value

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
  fun res -> return (Int64.of_int (M.Res.affected_rows res))

let select1 db sql set_params callback =
  with_stmt db sql @@ fun stmt ->
  let open IO in
  set_params stmt >>=
  M.Res.fetch row_array >>=
  check >>=
  function
  | Some row -> return (Some (callback row))
  | None -> return None

end

module Default(IO : Sqlgg_io.M)(M : Mariadb.Nonblocking.S with type 'a future = 'a IO.future) =
  Make(IO)(M)
