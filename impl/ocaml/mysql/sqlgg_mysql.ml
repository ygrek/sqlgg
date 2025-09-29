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
open Sqlgg_trait_types

module P = Mysql.Prepared

module type Value = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val to_literal : t -> string
end

module type Int = sig 
  include Value
  val get_int64 : string -> int64
  val set_int64: int64 -> string

  val int64_to_literal : int64 -> string
end

module type Types = sig
  module Bool : sig 
    include Value 
    val get_bool : string -> bool
    val set_bool: bool -> string
    val bool_to_literal : bool -> string
  end
  module Int : Int
  module UInt64 : sig
    include Value
    val get_uint64 : string -> Unsigned.UInt64.t
    val set_uint64: Unsigned.UInt64.t -> string
    val uint64_to_literal : Unsigned.UInt64.t -> string
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Float : sig
    include Value
    val get_float : string -> float
    val set_float: float -> string
    val float_to_literal : float -> string
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Text : sig 
    include Value
    val get_string : string -> string
    val set_string: string -> string
    val string_to_literal : string -> string
  end
  module Blob : sig
    include Value
    val get_string : string -> string
  end
  module Datetime : sig 
    include Value
    val get_string : string -> string
    val set_float: float -> string
    val float_to_literal : float -> string
  end
  module Decimal : sig 
    include Value
    val get_float : string -> float
    val set_float: float -> string
    val float_to_literal : float -> string
  end

  module Json : sig
    include Value
    val get_string : string -> json
    val set_string : json -> string
    val json_to_literal : json -> string
  end

  module Json_path: sig
    include Value
    val get_json_path : string -> json_path

    val set_json_path : json_path -> string
    val json_path_to_literal : json_path -> string
  end

  module One_or_all : sig
    include Value
    val get_one_or_all : string -> one_or_all
    val set_one_or_all : one_or_all -> string
    val one_or_all_to_literal : one_or_all -> string
  end

  module Any : Value
end

module Default_types = struct
  module Bool = struct
    type t = bool
    let of_string s = s <> "0"
    let to_string x = if x then "1" else "0"
    let to_literal = string_of_bool
    let get_bool = of_string
    let set_bool = to_string
    let bool_to_literal = to_literal
  end
  module Int = struct 
    include Int64 
    let to_literal = to_string

    let get_int64 = of_string
    let set_int64 = to_string
    let int64_to_literal = to_literal
  end
  module UInt64 = struct
    type t = Unsigned.UInt64.t
    let of_string _s = failwith "mysql bindings: uint64 is not supported yet"
    let to_string _v = failwith "mysql bindings: uint64 is not supported yet"
    let to_literal (x:t) = Unsigned.UInt64.to_string x
    let get_uint64 = of_string
    let set_uint64 = to_string
    let uint64_to_literal = to_literal
  end
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
    let get_string = of_string
    let set_string = to_string
    let string_to_literal = to_literal
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

    let get_string = of_string
  end
  module Float = struct
    type t = float
    let of_string = float_of_string
    let to_string = string_of_float
    let to_literal = string_of_float
    let get_float = of_string
    let set_float = to_string
    let float_to_literal = to_literal
  end
  (* you probably want better type, e.g. (int*int) or Z.t *)
  module Decimal = Float
  module Datetime = struct 
    include Text
    let set_float x = Text.set_string (Float.to_string x)
    let float_to_literal x = Text.to_literal (Float.to_literal x)
  end

  
  module Json = struct
    type t = json

    let of_string s = convert_json @@ Yojson.Safe.from_string s

    let to_string (x: t) = match x with
     | `Bool x -> Bool.to_string x
     | `Int x -> Int.to_string (Int64.of_int x)
     | `Intlit x ->  Int.to_string (Int64.of_string x)
     | `Float x -> Float.to_string x
     | #t  -> Yojson.Safe.to_string (x :> Yojson.Safe.t)

    let to_literal (x: t) = match x with
     | `Bool x -> Bool.to_literal x
     | `Int x -> Int.to_literal (Int64.of_int x)
     | `Intlit x ->  Int.to_string (Int64.of_string x)
     | `Float x -> Float.to_literal x
     | #t -> Text.to_literal @@ Yojson.Safe.to_string (x :> Yojson.Safe.t)

    let get_string = of_string
    let set_string = to_string
    let json_to_literal = to_literal
  end

 module Json_path = struct
    open Sqlgg_json_path
    type t = json_path
    
    let of_string = Json_path.parse_json_path
    let to_string = Json_path.string_of_json_path
    let to_literal j = Text.to_literal (Json_path.string_of_json_path j)

    let get_json_path = Json_path.parse_json_path

    let set_json_path = Json_path.string_of_json_path

    let json_path_to_literal = to_literal
  end

  module One_or_all = struct
    type t = one_or_all

    let of_string s =
      match String.lowercase_ascii s with
      | "one" -> `One
      | "all" -> `All
      | _ -> failwith (sprintf "One_or_all.of_string: unknown value %s" s)

    let to_string = function
      | `One -> "one"
      | `All -> "all"

    let to_literal = function
      | `One -> "one"
      | `All -> "all"

    let get_one_or_all = of_string
    let set_one_or_all = to_string
    let one_or_all_to_literal = to_literal
  end

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
type execute_response = { affected_rows: int64; insert_id: int64 option }

module Types = T

module type Enum = sig 
  type t

  val inj: string -> t

  val proj: t -> string
end

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
let get_column_UInt64, get_column_UInt64_nullable =
  ((fun _ _ -> oops "get_column_UInt64: mysql bindings do not support uint64 yet"),
   (fun _ _ -> oops "get_column_UInt64_nullable: mysql bindings do not support uint64 yet"))
let get_column_Text, get_column_Text_nullable = get_column_ty "Text" Text.of_string
let get_column_Float, get_column_Float_nullable = get_column_ty "Float" Float.of_string
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty "Decimal" Decimal.of_string
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty "Datetime" Datetime.of_string
let get_column_Json, get_column_Json_nullable = get_column_ty "Json" Json.of_string
let get_column_Json_path, get_column_Json_path_nullable = get_column_ty "Json_path" Json_path.of_string
let get_column_One_or_all, get_column_One_or_all_nullable = get_column_ty "One_or_all" One_or_all.of_string
let get_column_Any, get_column_Any_nullable = get_column_ty "Any" Any.of_string

let get_column_bool, get_column_bool_nullable = get_column_ty "bool" Bool.get_bool
let get_column_int64, get_column_int64_nullable = get_column_ty "int64" Int.get_int64
let get_column_uint64, get_column_uint64_nullable =
  ((fun _ _ -> oops "get_column_uint64: mysql bindings do not support uint64 yet"),
   (fun _ _ -> oops "get_column_uint64_nullable: mysql bindings do not support uint64 yet"))
let get_column_float, get_column_float_nullable = get_column_ty "float" Float.get_float
let get_column_decimal, get_column_decimal_nullable = get_column_ty "float" Decimal.get_float
let get_column_datetime, get_column_datetime_nullable = get_column_ty "string" Datetime.get_string
let get_column_string, get_column_string_nullable = get_column_ty "string" Text.get_string
let get_column_json, get_column_json_nullable = get_column_ty "json" Json.get_string
let get_column_json_path, get_column_json_path_nullable = get_column_ty "json_path" Json_path.get_json_path
let get_column_one_or_all, get_column_one_or_all_nullable = get_column_ty "one_or_all" One_or_all.get_one_or_all

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
let set_param_UInt64 _ _ = oops "set_param_UInt64: mysql bindings do not support uint64 yet"
let set_param_Float = set_param_ty Float.to_string
let set_param_Decimal = set_param_ty Decimal.to_string
let set_param_Datetime = set_param_ty Datetime.to_string
let set_param_Json = set_param_ty Json.to_string
let set_param_Json_path = set_param_ty Json_path.to_string
let set_param_One_or_all = set_param_ty One_or_all.to_string

let set_param_string = set_param_ty Text.set_string
let set_param_bool = set_param_ty Bool.set_bool
let set_param_int64 = set_param_ty Int.set_int64
let set_param_uint64 _ _ = oops "set_param_uint64: mysql bindings do not support uint64 yet"
let set_param_float = set_param_ty Float.set_float
let set_param_decimal = set_param_ty Decimal.set_float
let set_param_datetime = set_param_ty Datetime.set_float
let set_param_json = set_param_ty Json.set_string
let set_param_json_path = set_param_ty Json_path.set_json_path
let set_param_one_or_all = set_param_ty One_or_all.set_one_or_all

module Make_enum (E: Enum) = struct 
  include E

  let get_column, get_column_nullable = get_column_ty "Enum" E.inj

  let set_param = set_param_ty E.proj

  let to_literal = E.proj
end

let no_params stmt = P.execute stmt [||]

let prepare db sql = P.create db sql
let close_stmt stmt = P.close stmt

let try_finally final f x =
  let r =
    try f x with exn -> final (); raise exn
  in
    final ();
    r

let bracket res dtor k = try_finally (fun () -> dtor res) k res

let select_with_stmt stmt set_params callback =
  let r = set_params stmt in
  let rec loop () =
    match P.fetch r with
    | Some row -> callback row; loop ()
    | None -> ()
  in
  loop ()

let execute_with_stmt stmt set_params =
  let _ = set_params stmt in
  if 0 <> P.real_status stmt then oops "execute with stmt failed";
  let insert_id =
    match P.insert_id stmt with
    | 0L -> None
    | x -> Some x
  in
  { affected_rows = P.affected stmt; insert_id }

let select_one_maybe_with_stmt stmt set_params convert =
  match P.fetch (set_params stmt) with
  | Some row -> Some (convert row)
  | None -> None

let select_one_with_stmt stmt set_params convert =
  match P.fetch (set_params stmt) with
  | Some row -> convert row
  | None -> oops "no row but one expected in select_one_with_stmt"

let with_stmt db sql f = 
  bracket (prepare db sql) close_stmt f

let select db sql set_params callback =
  with_stmt db sql (fun stmt ->
    select_with_stmt stmt set_params callback)

let execute db sql set_params =
  with_stmt db sql (fun stmt ->
    execute_with_stmt stmt set_params)

let select_one_maybe db sql set_params convert =
  with_stmt db sql (fun stmt ->
    select_one_maybe_with_stmt stmt set_params convert)

let select_one db sql set_params convert =
  with_stmt db sql (fun stmt ->
    select_one_with_stmt stmt set_params convert)

end

module Default = Make_(Default_types)

let () =
  (* checking signature match *)
  let module M = (Default : Sqlgg_traits.M) in
  let module Cache_config = struct 
    let max_cache_size = 500
    let ttl_seconds = None
  end in
  let module C = Sqlgg_stmt_cache.Make (Cache_config) (struct
    module IO = Sqlgg_io.Blocking
    include Default
  end) in
  ignore (M.Oops "OK");
  ignore (C.Oops "OK");

(* compatibility *)
module Make(Number : Int) = struct
  (* ref http://gallium.inria.fr/blog/overriding-submodules/ *)
  module Types_ = struct
    include (Default_types : module type of Default_types with module Int := Default_types.Int)
    module Int = Number
  end
  include Make_(Types_)
end
