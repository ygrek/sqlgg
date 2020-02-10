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
            | CreateRoutine of string
            | Other
            [@@deriving show {with_path=false}]

type category = DDL | DQL | DML | DCL | TCL | OTHER [@@deriving show {with_path=false}, enum]

let all_categories = List.init (max_category - min_category) (fun i -> Option.get @@ category_of_enum @@ min_category + i)

let category_of_stmt_kind = function
| Select _ -> DQL
| Insert _
| Update _
| Delete _ -> DML
| Create _
| CreateIndex _
| CreateRoutine _
| Alter _
| Drop _ -> DDL
| Other -> OTHER
