type cardinality = Zero_one | One | Nat

type kind =
  | Select of cardinality
  | Insert of string
  | Create of string
  | CreateIndex of string
  | Update of string option
  | Delete of string list
  | Alter of string list
  | Drop of string
  | CreateRoutine of string
  | CreateType of string
  | DropType of string
  | Other

type t = private {
  sql : string;
  name : string;
  kind : kind;
  filename : string option;
}

val make : ?filename:string -> sql:string -> name:string -> kind:kind -> unit -> t

(** https://google.github.io/sqlcommenter/spec/ *)
module Sqlcommenter : sig

  val comment : (string * string) list -> string

  val annotate : (string * string) list -> t -> t

end
