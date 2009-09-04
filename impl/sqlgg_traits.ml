(** *)

module type M = sig

  type statement
  type connection
  type params
  type row
  type result

  (** datatypes *)
  type num = int64
  type text = string
  type any = text

  exception Oops of string

  val get_column_Int : row -> int -> num
  val get_column_Text : row -> int -> text
  val get_column_Any : row -> int -> any

  val start_params : statement -> int -> params
  val finish_params : params -> result

  (** [set_param_* stmt index val]. [index] is 0-based,
    @raise Oops on error *)
  val set_param_null : params -> int -> unit
  val set_param_Text : params -> int -> text -> unit
  val set_param_Any : params -> int -> any -> unit
  val set_param_Int : params -> int -> num -> unit

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

