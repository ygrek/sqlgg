(** Statement *)

(** optional name and start/end position in string *)
type param_id = string option * (int * int) deriving (Show)
type param = param_id * Sql.Type.t deriving (Show)
type params = param list deriving (Show)

let params_to_string ps = Show.show<params>(ps)

type insert_kind = Values | Assign deriving(Show)

(** inferred inserted values to complete sql statement *)
type inferred = (insert_kind * RA.Schema.t) option deriving(Show)

type cardinality = [`Zero_one | `One | `Nat] deriving(Show)

let cardinality_to_string c = Show.show<cardinality>(c)

type kind = | Select of cardinality (** possible number of rows *)
            | Insert of inferred * string (** table *)
            | Create of string
            | CreateIndex of string
            | Update of string option (** name for single-table UPDATEs *)
            | Delete of string
            | Alter of string
            | Drop of string
            | Other
            deriving (Show)

type t = { schema : RA.Schema.t; params : params; kind : kind; props : Props.t; }
