(** Statement *)

type insert_kind = Values | Assign [@@deriving show {with_path=false}]

(** inferred inserted values to complete sql statement *)
type inferred = (insert_kind * Sql.Schema.t) option [@@deriving show]

(** possible number of rows in query result *)
type cardinality = [`Zero_one | `One | `Nat] [@@deriving show]

let cardinality_to_string = show_cardinality

type kind = | Select of cardinality
            | Insert of inferred * Sql.table_name
            | Create of Sql.table_name
            | CreateIndex of string
            | Update of Sql.table_name option (** name for single-table UPDATEs *)
            | Delete of Sql.table_name
            | Alter of Sql.table_name list
            | Drop of Sql.table_name
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
