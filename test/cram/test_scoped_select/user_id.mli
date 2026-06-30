type t
val get_column : int64 -> t
val get_column_nullable : int64 option -> t option
val set_param : t -> int64
val to_int64 : t -> int64
