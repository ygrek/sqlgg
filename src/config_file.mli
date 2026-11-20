open Sqlgg

type gen = Cxx | Caml | Caml_io | Java | Xml | Json | Csharp | Sql | No_output
val gen_of_string : string -> gen option

type notnull_null_write = Allow | Reject
type dynamic_select = Both | Off

type setting =
  | Dialect of Dialect.t
  | No_check of Dialect.feature list
  | Write_notnull_null of notnull_null_write
  | Header of [ `Full | `Without_timestamp | `Static ] option
  | Gen of gen
  | Name of string
  | Params of Gen.params_selection
  | Category of
      [ `All | `None | `Only of Stmt.category list | `Except of Stmt.category list ]
  | Dynamic_select of dynamic_select
  | Diff_mode
  | Migrate_mode
  | Base of string
  | Target of string
  | Initial of string
  | Migrations_file of string
  | Extends of string
  | Max_migration_id_length of int
  | New_tables of Schema_diff.new_tables
  | Alter_algorithm of Sql.alter_algorithm
  | Alter_lock of Sql.alter_lock
  | Open of string
  | Input of string
  | Stdin

module Migrations : sig
  type host_language = Ocaml | Cxx | Csharp | Java

  val all_of_host_language : host_language list
  val host_language_to_string : host_language -> string

  module Module_name : sig
    type t

    val of_string :
      string -> (t, [> `Invalid_ocaml_module_name of string ]) result

    val error_to_string : [ `Invalid_ocaml_module_name of string ] -> string
    val to_string : t -> string
  end

  type t

  val lang : t -> host_language option
  val schema : t -> string option
  val initial : t -> string option
  val baseline : t -> Sqlgg_migrations.Migration_id.t option
  val migrations : t -> string option
  val manual : t -> string option
  val module_name : t -> Module_name.t option
  val new_tables : t -> Schema_diff.new_tables option
  val output : t -> string option
end

type error

val error_to_string : error -> string

val settings : string -> (setting list, [> `Config_file of error ]) result

val migrations :
  string ->
  (Dialect.t option * Migrations.t option, [> `Config_file of error ]) result

val document_jsonschema : Jsonkit.Jsonschema.t
