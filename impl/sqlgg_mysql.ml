(** sqlgg + ocaml + mysql *)

open Printf

module M = Mysql
module P = Mysql.P

type statement = P.stmt
type connection = M.dbd
type params = statement * string array
type row = string option array
type result = P.result

type num = int64
type text = string
type any = string

exception Oops of string
let oops fmt = ksprintf (fun s -> raise (Oops s)) fmt
let opt = function Some x -> x | None -> failwith "opt"

let get_column_Int row index =
  try
    Int64.of_string (opt (row.(index)))
  with
    _ -> oops "get_column_Int %i" index

let get_column_Text row index =
  try
    opt (row.(index))
  with
    _ -> oops "get_column_Text %i" index

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
let set_param_Int stmt index v = bind_param (Some (Int64.to_string v)) stmt index

let no_params stmt = P.execute stmt [||]

let finally final f x =
  let r =
    try f x with exn -> final (); raise exn
  in
    final ();
    r

let bracket res dtor k = finally (fun () -> dtor res) k res
let with_stmt db sql = bracket (P.prepare db sql) P.close

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
    M.StatusEmpty = M.status db)

let select1 db sql set_params callback =
  with_stmt db sql (fun stmt ->
    match P.fetch (set_params stmt) with
    | Some row -> Some (callback row)
    | None -> None)

