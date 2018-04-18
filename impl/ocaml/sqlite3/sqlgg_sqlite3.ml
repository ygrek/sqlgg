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

module Types = struct
  module Bool = struct type t = bool end
  module Int = Int64
  module Text = struct type t = string end
  module Float = struct type t = float end
  module Datetime = Float (* ? *)
  module Any = Text
end

module S = Sqlite3

type statement = S.stmt * string
type connection = S.db
type params = statement
type row = statement
type result = unit

type num = int64
type text = string
type any = string
type datetime = float

exception Oops of string

let get_column_Bool (stmt,sql) index =
  match S.column stmt index with
  | S.Data.INT i -> i <> 0L
  | _ -> raise (Oops (sprintf "get_column_Bool %u for %s" index sql))

let get_column_Int (stmt,sql) index =
  match S.column stmt index with
  | S.Data.INT i -> i
  | _ -> raise (Oops (sprintf "get_column_Int %u for %s" index sql))

let get_column_Text (stmt,_) index =
  let x = S.column stmt index in
  S.Data.to_string x

let get_column_Any = get_column_Text

let get_column_Float (stmt,sql) index =
  match S.column stmt index with
  | S.Data.FLOAT i -> i
  | _ -> raise (Oops (sprintf "get_column_Float %u for %s" index sql))

let get_column_Datetime = get_column_Float

let test_ok sql rc =
  if rc <> S.Rc.OK then
    raise (Oops (sprintf "test_ok %s for %s" (S.Rc.to_string rc) sql))

let bind_param d (stmt,sql) index =
  let rc = S.bind stmt (index+1) d in
  test_ok sql rc

let start_params stmt _ = stmt
let finish_params _ = ()

let set_param_null = bind_param S.Data.NULL
let set_param_Text stmt index v = bind_param (S.Data.TEXT v) stmt index
let set_param_Any = set_param_Text
let set_param_Bool stmt index v = bind_param (S.Data.INT (if v then 1L else 0L)) stmt index
let set_param_Int stmt index v = bind_param (S.Data.INT v) stmt index
let set_param_Float stmt index v = bind_param (S.Data.FLOAT v) stmt index
let set_param_Datetime = set_param_Float

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
    Int64.of_int (S.changes db)
  )

let select1 db sql set_params callback =
  with_sql db sql (fun stmt ->
    set_params stmt;
    if S.Rc.ROW = S.step (fst stmt) then
      Some (callback stmt)
    else
      None)
