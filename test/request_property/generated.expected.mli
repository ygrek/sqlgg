val insert_t1 : c1:bool option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t1 : (bool option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t1 : rows:(bool option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t2 : c1:string -> c2:bool option -> c3:string option -> c4:string -> c5:float -> c6:float option -> c7:Sqlgg_trait_types.json -> c8:Unsigned.UInt64.t option -> c9:string option -> c10:string option -> c11:float -> c12:Unsigned.UInt64.t -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t2 : (string * bool option * string option * string * float * float option * Sqlgg_trait_types.json * Unsigned.UInt64.t option * string option * string option * float * Unsigned.UInt64.t, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t2 : rows:(string * bool option * string option * string * float * float option * Sqlgg_trait_types.json * Unsigned.UInt64.t option * string option * string option * float * Unsigned.UInt64.t) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t3 : c1:int64 -> c2:float -> c3:Unsigned.UInt64.t option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t3 : (int64 * float * Unsigned.UInt64.t option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t3 : rows:(int64 * float * Unsigned.UInt64.t option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t3 : c1:int64 -> (int64 * float * Unsigned.UInt64.t option, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t3 : ids:int64 list -> (int64 * float * Unsigned.UInt64.t option, Sqlgg_request.many) Sqlgg_request.t

val insert_t4 : c1:int64 -> c2:float -> c3:float -> c4:float option -> c5:string option -> c6:string -> c7:float option -> c8:string -> c9:string option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t4 : (int64 * float * float * float option * string option * string * float option * string * string option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t4 : rows:(int64 * float * float * float option * string option * string * float option * string * string option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t4 : c1:int64 -> (int64 * float * float * float option * string option * string * float option * string * string option, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t4 : ids:int64 list -> (int64 * float * float * float option * string option * string * float option * string * string option, Sqlgg_request.many) Sqlgg_request.t

val insert_t5 : c1:bool -> c2:Unsigned.UInt64.t option -> c3:Unsigned.UInt64.t option -> c4:string option -> c5:string -> c6:string -> c7:string -> c8:Sqlgg_trait_types.json -> c9:Unsigned.UInt64.t option -> c10:string option -> c11:bool -> c12:[ `Red | `Green | `Num_404 | `X ] -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t5 : (bool * Unsigned.UInt64.t option * Unsigned.UInt64.t option * string option * string * string * string * Sqlgg_trait_types.json * Unsigned.UInt64.t option * string option * bool * [ `Red | `Green | `Num_404 | `X ], Sqlgg_request.many) Sqlgg_request.t
val insert_many_t5 : rows:(bool * Unsigned.UInt64.t option * Unsigned.UInt64.t option * string option * string * string * string * Sqlgg_trait_types.json * Unsigned.UInt64.t option * string option * bool * [ `Red | `Green | `Num_404 | `X ]) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t6 : c1:float -> c2:float -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t6 : (float * float, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t6 : rows:(float * float) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t7 : c1:int64 -> c2:[ `Green | `In_progress | `Done ] -> c3:string option -> c4:int64 -> c5:bool option -> c6:string -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t7 : (int64 * [ `Green | `In_progress | `Done ] * string option * int64 * bool option * string, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t7 : rows:(int64 * [ `Green | `In_progress | `Done ] * string option * int64 * bool option * string) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t7 : c1:int64 -> (int64 * [ `Green | `In_progress | `Done ] * string option * int64 * bool option * string, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t7 : ids:int64 list -> (int64 * [ `Green | `In_progress | `Done ] * string option * int64 * bool option * string, Sqlgg_request.many) Sqlgg_request.t

val insert_t8 : c1:bool -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t8 : (bool, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t8 : rows:(bool) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t9 : c1:string option -> c2:Unsigned.UInt64.t option -> c3:float option -> c4:Unsigned.UInt64.t option -> c5:string -> c6:bool option -> c7:Sqlgg_trait_types.json option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t9 : (string option * Unsigned.UInt64.t option * float option * Unsigned.UInt64.t option * string * bool option * Sqlgg_trait_types.json option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t9 : rows:(string option * Unsigned.UInt64.t option * float option * Unsigned.UInt64.t option * string * bool option * Sqlgg_trait_types.json option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t10 : c1:int64 -> c2:string -> c3:bool -> c4:Unsigned.UInt64.t -> c5:int64 -> c6:string -> c7:[ `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option -> c8:int64 option -> c9:string option -> c10:int64 -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t10 : (int64 * string * bool * Unsigned.UInt64.t * int64 * string * [ `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * int64 option * string option * int64, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t10 : rows:(int64 * string * bool * Unsigned.UInt64.t * int64 * string * [ `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * int64 option * string option * int64) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t10 : c1:int64 -> (int64 * string * bool * Unsigned.UInt64.t * int64 * string * [ `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * int64 option * string option * int64, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t10 : ids:int64 list -> (int64 * string * bool * Unsigned.UInt64.t * int64 * string * [ `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * int64 option * string option * int64, Sqlgg_request.many) Sqlgg_request.t

val insert_t11 : c1:float -> c2:int64 option -> c3:[ `Red | `In_progress | `Num_404 | `Done ] -> c4:bool -> c5:Unsigned.UInt64.t option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t11 : (float * int64 option * [ `Red | `In_progress | `Num_404 | `Done ] * bool * Unsigned.UInt64.t option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t11 : rows:(float * int64 option * [ `Red | `In_progress | `Num_404 | `Done ] * bool * Unsigned.UInt64.t option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t12 : c1:[ `Red | `Green | `Num_404 | `Done | `X ] -> c2:string -> c3:string -> c4:Sqlgg_trait_types.json -> c5:float -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t12 : ([ `Red | `Green | `Num_404 | `Done | `X ] * string * string * Sqlgg_trait_types.json * float, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t12 : rows:([ `Red | `Green | `Num_404 | `Done | `X ] * string * string * Sqlgg_trait_types.json * float) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t13 : c1:int64 -> c2:Unsigned.UInt64.t option -> c3:int64 option -> c4:string -> c5:string option -> c6:Sqlgg_trait_types.json option -> c7:string -> c8:string -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t13 : (int64 * Unsigned.UInt64.t option * int64 option * string * string option * Sqlgg_trait_types.json option * string * string, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t13 : rows:(int64 * Unsigned.UInt64.t option * int64 option * string * string option * Sqlgg_trait_types.json option * string * string) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t13 : c1:int64 -> (int64 * Unsigned.UInt64.t option * int64 option * string * string option * Sqlgg_trait_types.json option * string * string, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t13 : ids:int64 list -> (int64 * Unsigned.UInt64.t option * int64 option * string * string option * Sqlgg_trait_types.json option * string * string, Sqlgg_request.many) Sqlgg_request.t

val insert_t14 : c1:int64 -> c2:string option -> c3:string -> c4:int64 -> c5:int64 -> c6:Unsigned.UInt64.t option -> c7:int64 option -> c8:Unsigned.UInt64.t option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t14 : (int64 * string option * string * int64 * int64 * Unsigned.UInt64.t option * int64 option * Unsigned.UInt64.t option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t14 : rows:(int64 * string option * string * int64 * int64 * Unsigned.UInt64.t option * int64 option * Unsigned.UInt64.t option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t14 : c1:int64 -> (int64 * string option * string * int64 * int64 * Unsigned.UInt64.t option * int64 option * Unsigned.UInt64.t option, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t14 : ids:int64 list -> (int64 * string option * string * int64 * int64 * Unsigned.UInt64.t option * int64 option * Unsigned.UInt64.t option, Sqlgg_request.many) Sqlgg_request.t

val insert_t15 : c1:string -> c2:Unsigned.UInt64.t option -> c3:float -> c4:int64 option -> c5:float -> c6:float option -> c7:float option -> c8:float -> c9:float -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t15 : (string * Unsigned.UInt64.t option * float * int64 option * float * float option * float option * float * float, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t15 : rows:(string * Unsigned.UInt64.t option * float * int64 option * float * float option * float option * float * float) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t

val insert_t16 : c1:int64 -> c2:Unsigned.UInt64.t option -> c3:Unsigned.UInt64.t -> c4:string option -> c5:float -> c6:Sqlgg_trait_types.json option -> c7:string -> c8:[ `Red | `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option -> c9:string -> c10:bool -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_t16 : (int64 * Unsigned.UInt64.t option * Unsigned.UInt64.t * string option * float * Sqlgg_trait_types.json option * string * [ `Red | `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * string * bool, Sqlgg_request.many) Sqlgg_request.t
val insert_many_t16 : rows:(int64 * Unsigned.UInt64.t option * Unsigned.UInt64.t * string option * float * Sqlgg_trait_types.json option * string * [ `Red | `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * string * bool) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val lookup_t16 : c1:int64 -> (int64 * Unsigned.UInt64.t option * Unsigned.UInt64.t * string option * float * Sqlgg_trait_types.json option * string * [ `Red | `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * string * bool, Sqlgg_request.zero_or_one) Sqlgg_request.t
val lookup_in_t16 : ids:int64 list -> (int64 * Unsigned.UInt64.t option * Unsigned.UInt64.t * string option * float * Sqlgg_trait_types.json option * string * [ `Red | `Green | `In_progress | `Num_404 | `Done | `X | `A_b ] option * string * bool, Sqlgg_request.many) Sqlgg_request.t

val insert_kitchen_sink : c1:int64 -> c2:int64 option -> c3:Unsigned.UInt64.t -> c4:Unsigned.UInt64.t option -> c5:float -> c6:float option -> c7:float -> c8:float option -> c9:string -> c10:string option -> c11:string -> c12:string option -> c13:bool -> c14:bool option -> c15:string -> c16:string option -> c17:Sqlgg_trait_types.json -> c18:Sqlgg_trait_types.json option -> c19:[ `Red | `Green | `Num_404 | `Done | `X ] -> c20:[ `Red | `Green | `Num_404 | `Done | `X ] option -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
val select_kitchen_sink : (int64 * int64 option * Unsigned.UInt64.t * Unsigned.UInt64.t option * float * float option * float * float option * string * string option * string * string option * bool * bool option * string * string option * Sqlgg_trait_types.json * Sqlgg_trait_types.json option * [ `Red | `Green | `Num_404 | `Done | `X ] * [ `Red | `Green | `Num_404 | `Done | `X ] option, Sqlgg_request.many) Sqlgg_request.t
val insert_many_kitchen_sink : rows:(int64 * int64 option * Unsigned.UInt64.t * Unsigned.UInt64.t option * float * float option * float * float option * string * string option * string * string option * bool * bool option * string * string option * Sqlgg_trait_types.json * Sqlgg_trait_types.json option * [ `Red | `Green | `Num_404 | `Done | `X ] * [ `Red | `Green | `Num_404 | `Done | `X ] option) list -> (unit, Sqlgg_request.affected_rows) Sqlgg_request.t
