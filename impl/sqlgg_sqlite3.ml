(** sqlgg + ocaml + sqlite3 *)

open Printf

module S = Sqlite3

type statement = S.stmt
type connection = S.db

type num = int64
type text = string
type any = string

exception Oops of string

let get_column_Int stmt index = 
  match S.column stmt index with
  | S.Data.INT i -> i
  | _ -> raise (Oops (sprintf "get_column_Int %u" index))

let get_column_Text stmt index =
  let x = S.column stmt index in
  S.Data.to_string x

let get_column_Any = get_column_Text

let test_ok rc = 
  if rc <> S.Rc.OK then
    raise (Oops (sprintf "test_ok %s" (S.Rc.to_string rc)))

let bind_param d stmt index = 
  let rc = S.bind stmt (index+1) d in
  test_ok rc

let set_param_null = bind_param S.Data.NULL
let set_param_Text stmt index v = bind_param (S.Data.TEXT v) stmt index
let set_param_Any = set_param_Text
let set_param_Int stmt index v = bind_param (S.Data.INT v) stmt index

let finally final f x =
  let r =
    try f x with exn -> final (); raise exn
  in
    final ();
    r

let select db sql set_params callback =
  let stmt = S.prepare db sql in
  finally (fun () -> test_ok (S.finalize stmt))
  (fun () ->
    set_params stmt;
    while S.Rc.ROW = S.step stmt do
      callback stmt
    done) ()

let execute db sql set_params = 
  let stmt = S.prepare db sql in
  finally (fun () -> test_ok (S.finalize stmt))
  (fun () ->
    set_params stmt;
    let rc = S.step stmt in
    rc = S.Rc.DONE
  ) ()

let select1 db sql set_params callback =
  let stmt = S.prepare db sql in
  finally (fun () -> test_ok (S.finalize stmt))
  (fun () ->
    set_params stmt;
    if S.Rc.ROW = S.step stmt then
      Some (callback stmt)
    else
      None) ()

