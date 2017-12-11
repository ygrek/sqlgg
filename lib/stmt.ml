(** Statement *)

type insert_kind = Values | Assign [@@deriving show {with_path=false}]

(** inferred inserted values to complete sql statement *)
type inferred = (insert_kind * Sql.Schema.t) option [@@deriving show]

type cardinality = [`Zero_one | `One | `Nat] [@@deriving show]

let cardinality_to_string = show_cardinality

type kind = | Select of cardinality (** possible number of rows *)
            | Insert of inferred * string (** table *)
            | Create of string
            | CreateIndex of string
            | Update of string option (** name for single-table UPDATEs *)
            | Delete of string
            | Alter of string
            | Drop of string
            | Other
            [@@deriving show {with_path=false}]

type t = { schema : Sql.Schema.t; params : Sql.params; kind : kind; props : Props.t; }
