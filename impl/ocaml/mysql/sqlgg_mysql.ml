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

module type Value = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val to_literal : t -> string
end

module type Types = sig
  module Bool : Value
  module Int : Value
  module Float : Value
  module Text : Value
  module Blob : Value
  module Datetime : Value
  module Decimal : Value
  module Any : Value
end

module Default_types = struct
  module Bool = struct
    type t = bool
    let of_string s = s <> "0"
    let to_string x = if x then "1" else "0"
    let to_literal = string_of_bool
  end
  module Int = struct include Int64 let to_literal = to_string end
  module Text = struct
    type t = string
    let of_string s = s
    let to_string s = s

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
  end

  module Blob = struct
    (* https://dev.mysql.com/doc/refman/5.7/en/hexadecimal-literals.html
       "Hexadecimal literal values are written using X'val' or 0xval notation,
       where val contains hexadecimal digits (0..9, A..F)."
      "By default, a hexadecimal literal is a binary string, where each pair of
       hexadecimal digits represents a character" *)
    type t = string

    let to_hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

    let of_string s = s
    let to_string s = s

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
    let of_string = float_of_string
    let to_string = string_of_float
    let to_literal = string_of_float
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Decimal = Float
  module Datetime = Text
  module Any = Text
end

(*
example: Datetime as timestamp
see below how to create Types based on Default_types with overridden submodule

module Datetime
: sig (* optionally constrain with a signature for moar typing *)
  type t = private float
  val of_string : string -> t
  val to_string : t -> string
end
= struct
  type t = float
  let of_string = function "0000-00-00 00:00:00" -> 0. | s -> ExtUnix.Specific.(timegm (strptime "%Y-%m-%d %H:%M:%S" s))
  let to_string = function 0. -> "0000-00-00 00:00:00" | t -> ExtUnix.Specific.strftime "%Y-%m-%d %H:%M:%S" (Unix.gmtime t)
end
*)

module Make_(T : Types) = struct

type statement = P.stmt
type 'a connection = Mysql.dbd
type params = statement * string array * int ref
type row = string option array
type result = P.stmt_result

module Types = T
open Types

(* compatibility *)
type num = Int.t
type text = Text.t
type any = Any.t
type datetime = Datetime.t

exception Oops of string
let oops fmt = ksprintf (fun s -> raise (Oops s)) fmt

let get_column_ty name conv =
  begin fun row index ->
    try
      conv (match row.(index) with None -> failwith "no value" | Some (x:string) -> x)
    with
      e -> oops "get_column_%s %i (%s)" name index (Printexc.to_string e)
  end,
  begin fun row index ->
    try
      match row.(index) with None -> None | Some s -> Some (conv s)
    with
      e -> oops "get_column_%s_nullable %i (%s)" name index (Printexc.to_string e)
  end

let get_column_Bool, get_column_Bool_nullable = get_column_ty "Bool" Bool.of_string
let get_column_Int, get_column_Int_nullable = get_column_ty "Int" Int.of_string
let get_column_Text, get_column_Text_nullable = get_column_ty "Text" Text.of_string
let get_column_Float, get_column_Float_nullable = get_column_ty "Float" Float.of_string
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty "Decimal" Decimal.of_string
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty "Datetime" Datetime.of_string
let get_column_Any, get_column_Any_nullable = get_column_ty "Any" Any.of_string

let bind_param data (_,params,index) =
  match data with
  | Some s -> assert (!index < Array.length params); params.(!index) <- s; incr index
  | None -> oops "bind_param None -- not implemented"

let start_params stmt n = (stmt,Array.make n "",ref 0)
let finish_params (stmt,params,index) = assert (!index = Array.length params); P.execute stmt params

let set_param_ty f = fun (p:params) v -> bind_param (Some (f v)) p

let set_param_null stmt = bind_param None stmt
let set_param_Text = set_param_ty Text.to_string
let set_param_Any = set_param_ty Any.to_string
let set_param_Bool = set_param_ty Bool.to_string
let set_param_Int = set_param_ty Int.to_string
let set_param_Float = set_param_ty Float.to_string
let set_param_Decimal = set_param_ty Decimal.to_string
let set_param_Datetime = set_param_ty Datetime.to_string

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
    { Sqlgg_traits.affected_rows = P.affected stmt; insert_id = P.insert_id stmt})

let select_one_maybe db sql set_params convert =
  with_stmt db sql (fun stmt ->
    match P.fetch (set_params stmt) with
    | Some row -> Some (convert row)
    | None -> None)

let select_one db sql set_params convert =
  with_stmt db sql (fun stmt ->
    match P.fetch (set_params stmt) with
    | Some row -> convert row
    | None -> oops "no row but one expected : %s" sql)

end

module Default = Make_(Default_types)

let () =
  (* checking signature match *)
  let module M = (Default : Sqlgg_traits.M) in
  ignore (M.Oops "OK")

(* compatibility *)
module Make(Number : Value) = struct
  (* ref http://gallium.inria.fr/blog/overriding-submodules/ *)
  module Types_ = struct
    include (Default_types : module type of Default_types with module Int := Default_types.Int)
    module Int = Number
  end
  include Make_(Types_)
end
