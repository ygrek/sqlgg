(** Statement *)

open ExtLib
open Jsonkit.Primitives

type insert_kind = Values | Assign [@@deriving show {with_path=false}, eq, json, jsonschema] [@@compact_variants]

(** inferred inserted values to complete sql statement *)
type inferred = (insert_kind * Sql.schema) option [@@deriving show, eq, json, jsonschema]

(** possible number of rows in query result *)
type cardinality = [`Zero_one | `One | `Nat] [@@deriving show, eq, json, jsonschema] [@@compact_variants]

let cardinality_to_string = show_cardinality

type kind = | Select of cardinality
            | Insert of inferred * Sql.table_name
            | Create of Sql.table_name
            | CreateIndex of string
            | Update of Sql.table_name option (** name for single-table UPDATEs *)
            | Delete of Sql.table_name list
            | Alter of Sql.table_name list
            | Drop of Sql.table_name
            | CreateRoutine of Sql.table_name (** namespaced function name *)
            | CreateType of string
            | DropType of string
            | Other
            [@@deriving show {with_path=false}, eq, json, jsonschema] [@@compact_variants]

type category =
  | DDL [@as "ddl"]
  | DQL [@as "dql"]
  | DML [@as "dml"]
  | DCL [@as "dcl"]
  | TCL [@as "tcl"]
  | OTHER [@as "other"]
[@@deriving show {with_path=false}, enumerate, to_string, of_string]

let all_categories =
  List.filter
    (function DDL | DQL | DML | DCL | TCL -> true | OTHER -> false)
    all_of_category

let category_of_stmt_kind = function
| Select _ -> DQL
| Insert _
| Update _
| Delete _ -> DML
| Create _
| CreateIndex _
| CreateRoutine _
| Alter _
| Drop _
| CreateType _
| DropType _ -> DDL
| Other -> OTHER

let dml_tables = function
| Insert (_, table)
| Update (Some table) -> [ table ]
| Delete tables -> tables
| Select _ | Create _ | CreateIndex _ | Update None | Alter _ | Drop _
| CreateRoutine _ | CreateType _ | DropType _ | Other -> []
