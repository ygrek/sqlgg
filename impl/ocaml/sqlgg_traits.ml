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

module type Value = sig type t end

module type M = sig

  type statement
  type connection
  type params
  type row
  type result

  (** datatypes *)
  module Types : sig
    module Bool : Value
    module Int : Value
    module Float : Value
    module Text : Value
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
  val get_column_Datetime : row -> int -> Datetime.t

  val start_params : statement -> int -> params
  val finish_params : params -> result

  (** [set_param_* stmt index val]. [index] is 0-based,
    @raise Oops on error *)
  val set_param_null : params -> int -> unit
  val set_param_Text : params -> int -> Text.t -> unit
  val set_param_Any : params -> int -> Any.t -> unit
  val set_param_Bool : params -> int -> Bool.t -> unit
  val set_param_Int : params -> int -> Int.t -> unit
  val set_param_Float : params -> int -> Float.t -> unit
  val set_param_Datetime : params -> int -> Datetime.t -> unit

  val no_params : statement -> result

  (**
    Perform query and return results via callback for each row
    @raise Oops on error
  *)
  val select : connection -> string -> (statement -> result) -> (row -> unit) -> unit

  (**
    Perform query and return first row if available
    @raise Oops on error
  *)
  val select1 : connection -> string -> (statement -> result) -> (row -> 'b) -> 'b option

  (** Execute non-query.
    @raise Oops on error
    @return number of affected rows
  *)
  val execute : connection -> string -> (statement -> result) -> int64

end

module type M_io = sig

  include M

  module IO : Sqlgg_io.M

  val finish_params : params -> result IO.future

  val no_params : statement -> result IO.future

  val select : connection -> string -> (statement -> result IO.future) -> (row -> unit) -> unit IO.future

  val select1 : connection -> string -> (statement -> result IO.future) -> (row -> 'b) -> 'b option IO.future

  val execute : connection -> string -> (statement -> result IO.future) -> int64 IO.future

end
