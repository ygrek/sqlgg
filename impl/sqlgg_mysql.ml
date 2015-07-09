(**
  Mysql OCaml traits for sqlgg
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

module P = Mysql.Prepared

module Make(Number : sig type t val of_string : string -> t val to_string : t -> string end) = struct

type statement = P.stmt
type connection = Mysql.dbd
type params = statement * string array
type row = string option array
type result = P.stmt_result

type num = Number.t
type text = string
type any = string
type datetime = float

exception Oops of string
let oops fmt = ksprintf (fun s -> raise (Oops s)) fmt

let get_column_ty name conv row index =
  try
    conv (match row.(index) with None -> failwith "no value" | Some (x:string) -> x)
  with
    e -> oops "get_column_%s %i (%s)" name index (Printexc.to_string e)

let get_column_Bool = get_column_ty "Bool" (fun s -> s <> "0")
let get_column_Int = get_column_ty "Int" Number.of_string
let get_column_Text = get_column_ty "Text" (fun x -> x)
let get_column_Float = get_column_ty "Float" float_of_string
let get_column_Datetime = get_column_ty "Datetime" float_of_string
let get_column_Any = get_column_Text

let bind_param data (_,params) index =
  match data with
  | Some s -> params.(index) <- s
  | None -> oops "bind_param None -- not implemented"

let start_params stmt n = (stmt,Array.make n "")
let finish_params (stmt,params) = P.execute stmt params

let set_param_Text stmt index v = bind_param (Some v) stmt index
let set_param_null stmt index = bind_param None stmt index
let set_param_Any = set_param_Text
let set_param_Bool stmt index v = bind_param (Some (if v then "1" else "0")) stmt index
let set_param_Int stmt index v = bind_param (Some (Number.to_string v)) stmt index
let set_param_Float stmt index v = bind_param (Some (string_of_float v)) stmt index
let set_param_Datetime = set_param_Float

let no_params stmt = P.execute stmt [||]

let try_finally final f x =
  let r =
    try f x with exn -> final (); raise exn
  in
    final ();
    r

let bracket res dtor k = try_finally (fun () -> dtor res) k res
let with_stmt db sql = bracket (P.create db sql) P.close

let select db sql set_params callback =
  with_stmt db sql (fun stmt ->
    let r = set_params stmt in
    let rec loop () =
      match P.fetch r with
      | Some row -> callback row; loop ()
      | None -> ()
    in
    loop ())

let execute db sql set_params =
  with_stmt db sql (fun stmt ->
    let _ = set_params stmt in
    if 0 <> P.real_status stmt then oops "execute : %s" sql;
    P.affected stmt)

let select1 db sql set_params callback =
  with_stmt db sql (fun stmt ->
    match P.fetch (set_params stmt) with
    | Some row -> Some (callback row)
    | None -> None)

end
