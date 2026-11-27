(** Registry of reusable SELECT statements ([include: reuse]) in declaration order *)

type entry = private {
  name : string;
  sql : string;
  select : Sql.select_full;
  props : Props.t list;
}
[@@deriving json, jsonschema]

type t

val add : name:string -> sql:string -> select:Sql.select_full -> props:Props.t list -> unit

(** latest declaration with this name
  @raise Failure when no such reusable query exists *)
val get : string -> entry

(** visible entries in declaration order, a redeclared name keeps its latest position *)
val entries : unit -> entry list

val snapshot : unit -> t
val restore : t -> unit
