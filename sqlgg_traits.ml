(** *)

module type M = sig

  type statement
  type connection
  type num = int64
  type text = string
  type any = string

  val get_column_Int : statement -> int -> num
  val get_column_Text : statement -> int -> text

  val set_param_null : statement -> int -> bool
  val set_param_Text : statement -> int -> text -> bool
  val set_param_Any : statement -> int -> any -> bool
  val set_param_Int : statement -> int -> num -> bool

  (*
  val select_exn : connection -> string -> (statement -> 'a) -> (statement -> 'b) -> bool
  val execute_exn : connection -> string -> (statement -> 'a) -> bool
  *)

  val select : connection -> string -> (statement -> 'a) -> (statement -> 'b) -> bool
  val execute : connection -> string -> (statement -> 'a) -> bool

end

