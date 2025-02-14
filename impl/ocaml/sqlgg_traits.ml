(**
  OCaml traits signature for sqlgg
  by ygrek
  2014-06-08

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*)

module type Value = sig
  type t

  (** Return the literal representation of the value in the SQL dialect. *)
  val to_literal : t -> string
end

module type FNS = sig
  (* all types intended for subsitution *)
  type params
  type result
  type 'a io_future (* this type intended for different substitutions *)
  type 'a connection
  type statement
  type row
  type execute_response

  val finish_params : params -> result io_future

  val no_params : statement -> result io_future

  val select : [>`RO] connection -> string -> (statement -> result io_future) -> (row -> unit) -> unit io_future

  val select_one_maybe : [>`RO] connection -> string -> (statement -> result io_future) -> (row -> 'b) -> 'b option io_future

  val select_one : [>`RO] connection -> string -> (statement -> result io_future) -> (row -> 'b) -> 'b io_future

  val execute : [>`WR] connection -> string -> (statement -> result io_future) -> execute_response io_future
end

module type M = sig

  type statement
  type -'a connection
  type params
  type row
  type result
  type execute_response = { affected_rows: int64; insert_id: int64 option }

  (** datatypes *)
  module Types : sig
    module Bool : Value
    module Int : Value
    module Float : Value
    module Text : Value
    module Blob : Value
    module Decimal : Value
    module Datetime : Value
    module Any : Value
  end

  open Types

  type num = Int.t
  type text = Text.t
  type any = Any.t
  type datetime = Datetime.t

  exception Oops of string

  val get_column_Bool : row -> int -> Bool.t
  val get_column_Int : row -> int -> Int.t
  val get_column_Text : row -> int -> Text.t
  val get_column_Any : row -> int -> Any.t
  val get_column_Float : row -> int -> Float.t
  val get_column_Decimal : row -> int -> Decimal.t
  val get_column_Datetime : row -> int -> Datetime.t

  val get_column_Bool_nullable : row -> int -> Bool.t option
  val get_column_Int_nullable : row -> int -> Int.t option
  val get_column_Text_nullable : row -> int -> Text.t option
  val get_column_Any_nullable : row -> int -> Any.t option
  val get_column_Float_nullable : row -> int -> Float.t option
  val get_column_Decimal_nullable : row -> int -> Decimal.t option
  val get_column_Datetime_nullable : row -> int -> Datetime.t option

  val start_params : statement -> int -> params

  (** [set_param_* stmt index val]. [index] is 0-based,
    @raise Oops on error *)
  val set_param_null : params -> unit
  val set_param_Text : params -> Text.t -> unit
  val set_param_Any : params -> Any.t -> unit
  val set_param_Bool : params -> Bool.t -> unit
  val set_param_Int : params -> Int.t -> unit
  val set_param_Float : params -> Float.t -> unit
  val set_param_Decimal : params -> Decimal.t -> unit
  val set_param_Datetime : params -> Datetime.t -> unit

  include FNS
    with type params := params
    with type result := result
    with type 'a io_future := 'a
    with type 'a connection := 'a connection
    with type statement := statement
    with type row := row
    with type execute_response := execute_response

end

module type M_io = sig

  include M

  module IO : Sqlgg_io.M

  include FNS
    with type params := params
    with type result := result
    with type 'a io_future := 'a IO.future
    with type 'a connection := 'a connection
    with type statement := statement
    with type row := row
    with type execute_response := execute_response

end
