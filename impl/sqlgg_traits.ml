(** *)

module type M = sig

  type statement
  type connection

  (** datatypes *)
  type num = int64
  type text = string
  type any = text

  exception Oops of string

  val get_column_Int : statement -> int -> num
  val get_column_Text : statement -> int -> text

  (** [set_param_* stmt index val]. [index] is 0-based,
    @raise Oops on error *)
  val set_param_null : statement -> int -> unit
  val set_param_Text : statement -> int -> text -> unit
  val set_param_Any : statement -> int -> any -> unit
  val set_param_Int : statement -> int -> num -> unit

  (** 
    Perform query and return results via callback for each row
    @raise Oops on error
  *)
  val select : connection -> string -> (statement -> unit) -> (statement -> unit) -> unit

  (**
    Perform query and return first row if available
    @raise Oops on error
  *)
  val select1 : connection -> string -> (statement -> unit) -> (statement -> 'b) -> 'b option

  (** Execute non-query. 
    @raise Oops on error
    @return true if successful
  *)
  val execute : connection -> string -> (statement -> unit) -> bool

end

