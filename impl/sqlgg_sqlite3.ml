(** sqlgg + ocaml + sqlite3 *)

module S = Sqlite3

type statement = S.stmt
type connection = S.db

type num = int64
type text = string
type any = string

let get_column_Int stmt index = 
  match S.column stmt index with
  | S.Data.INT i -> i
  | x -> try Int64.of_string (S.Data.to_string x) with _ -> 0L (* or better fail ? *)

let get_column_Text stmt index =
  let x = S.column stmt index in
  S.Data.to_string x

let test_ok rc = let ok = rc = S.Rc.OK in assert ok; ok

let bind_param d stmt index = 
  let rc = S.bind stmt (index+1) d in
  test_ok rc

let set_param_null = bind_param S.Data.NULL
let set_param_Text stmt index v = bind_param (S.Data.TEXT v) stmt index
let set_param_Any = set_param_Text
let set_param_Int stmt index v = bind_param (S.Data.INT v) stmt index

let select_exn db sql callback set_params =
  let stmt = S.prepare db sql in
  set_params stmt;

  while S.Rc.ROW = S.step stmt do
    callback stmt
  done;

  test_ok (S.finalize stmt)

let execute_exn db sql set_params = 
  let stmt = S.prepare db sql in
  set_params stmt;

  let rc = S.step stmt in
  assert (S.Rc.DONE = rc);

  test_ok (S.finalize stmt)

(** may @raise exceptions other than Error *)
let select db sql cb p =
  try
    select_exn db sql cb p
  with
  | S.Error _ -> false

let execute db sql p =
  try
    execute_exn db sql p
  with
  | S.Error _ -> false

