(** Relation slots: [FROM @source AS alias] chosen at runtime among compatible relations *)

(** A relation slot used where it cannot be supported, independent of the chosen relation *)
exception Unsupported of string

(** raises [Unsupported] at [at] *)
val unsupported : at:Sql.Pos.t -> ('a, unit, string, 'b) format4 -> 'a

module Name : sig
  type t = private string [@@deriving eq, ord, json, jsonschema]
  val of_param_id : Sql.param_id -> t option
end

(** A column of the relation chosen for a slot *)
type column = { slot : Name.t; column : string } [@@deriving eq, ord, json, jsonschema]

(** SQL kind used to keep two independently chosen relations compatible.
  Coarser than SQL compatibility on purpose: equal classes are always compatible. *)
type kind_class =
  | Int_kind
  | UInt64_kind
  | Text_kind
  | Blob_kind
  | Float_kind
  | Decimal_kind
  | Bool_kind
  | Datetime_kind
  | Json_kind
  | Enum_kind of string list
[@@deriving eq, json, jsonschema]

val kind_class : Sql.Type.t -> kind_class option

(** How a slot is seen during one evaluation of the statement *)
type binding =
  | Open (** columns are created on demand, expected types are recorded *)
  | Bound of Sql.schema (** columns of one candidate *)

(** the binding of a slot when an evaluation binds the slot of [bound] to its relation, the others open *)
val binding : (Name.t * Sql.schema) option -> Name.t -> binding

(** A column of an open slot used by the statement *)
type use = {
  column : column;
  pos : Sql.Pos.t;
  expected : kind_class list; (** the SQL kinds expected of it there, latest first *)
}

(** One use of a parameter of a reusable query: named and of a built-in type, bound by the builder of the relation *)
type own_param = { name : string; param : Sql.Type.t Sql.param; meta : Sql.Meta.t }
[@@deriving json, jsonschema]

(** A type parameter of the generated [Source.t] *)
type index =
  | Column of int (** type of a result column *)
  | Parameter of string (** type of a named query parameter *)
  | Coupling of column (** SQL kind of a column compared with a column of another slot *)

val equal_index : index -> index -> bool

(** What one relation puts in a type parameter of [Source.t] *)
type evidence =
  | Column_type of int * Sql.attr
  | Parameter_type of string * (Sql.Type.t * Sql.Meta.t)
  | Coupling_kind of column * kind_class
[@@deriving json, jsonschema]

val index : evidence -> index

(** One relation accepted for a slot *)
type instance = {
  candidate : Relation_candidate.t;
  own_params : own_param list; (** every use, in source order *)
  evidence : evidence list; (** the same indices in the same order for every instance of a slot *)
}
[@@deriving json, jsonschema]

type rejection = { rejected : Relation_candidate.t; reason : string }
[@@deriving json, jsonschema]

type t = {
  name : Name.t;
  instances : instance * instance list;
  rejections : rejection list; (** in declaration order of the relations *)
}
[@@deriving json, jsonschema]

val instances : t -> instance list

(** the type parameters of the generated [Source.t], in order *)
val indices : t -> index list
