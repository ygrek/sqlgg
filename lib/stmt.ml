(** Statement *)

open ExtLib

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
            | Delete of Sql.table_name list
            | Alter of Sql.table_name list
            | Drop of Sql.table_name
            | CreateRoutine of string * Sql.Type.t option
            | Other
            [@@deriving show {with_path=false}]

let kind_to_operation_name = function
| Select _ -> Some "SELECT"
| Insert _ -> Some "INSERT"
| Create _ -> Some "CREATE TABLE"
| CreateIndex _ -> Some "CREATE INDEX"
| Update _ -> Some "UPDATE"
| Delete _ -> Some "DELETE"
| Alter _ -> Some "ALTER TABLE"
| Drop _ -> Some "DROP TABLE"
| CreateRoutine (_, Some _) -> Some "CREATE FUNCTION"
| CreateRoutine (_, None) -> Some "CREATE PROCEDURE"
| Other -> None

let kind_to_table_names = function
| Create t -> [t]
| CreateIndex _ -> [] (* FIXME *)
| Update (Some t) -> [t]
| Update None -> []
| Insert (_,t) -> [t]
| Delete ts -> ts
| Alter ts -> ts
| Drop t -> [t]
| Select _  ->  [] (* FIXME *)
| CreateRoutine (_s, _ret) -> []
| Other -> []

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
