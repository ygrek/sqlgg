type t =
  | MySQL
  | PostgreSQL
  | SQLite
  | TiDB
  | ClickHouse

val equal : t -> t -> bool
val hash : t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string
val all : t list
val to_string : t -> string
val of_string : string -> t option
val of_yojson : Yojson.Safe.t -> (t, string) result

(** A dialect together with the semantic settings it actually varies. *)
type configuration

(** The configuration of a row-store dialect; such dialects take no settings. *)
type row_store

val default_configuration : t -> configuration
val dialect_of_configuration : configuration -> t
val row_store_dialect : row_store -> t

(** MySQL, SQLite and TiDB outside strict mode replace a NULL written into a NOT NULL column
    with the column default; the other dialects are left unchanged. *)
val with_non_strict_writes : configuration -> configuration

type configuration_error =
  | Unknown_dialect of string
  | Settings_not_supported of row_store
  | Invalid_clickhouse_setting of Sql_semantics.ClickHouse.Setting.error

(** Parses [name] or [clickhouse,setting=value,...]. *)
val parse_configuration : string -> (configuration, configuration_error) result

type feature =
  | Collation
  | JoinOnSubquery
  | CreateTableAsSelect
  | OnDuplicateKey
  | OnConflict
  | StraightJoin
  | LockInShareMode
  | FulltextIndex
  | UnsignedTypes
  | AutoIncrement
  | ReplaceInto
  | RowLocking
  | DefaultExpr
  | Ttl
  | CachedTable
  | AlterColumn
  | AlterAlgorithm
  | AlterLock
  | UserDefinedType
  | Extension

val show_feature : feature -> string
val all_of_feature : feature list
val feature_to_string : feature -> string
val feature_of_string : string -> feature option

type support_state = {
  supported : t list;
  unsupported : t list;
  unknown : t list;
}

type dialect_support = {
  feature : feature;
  pos : Sql.Pos.t;
  state : support_state;
}

val support : dialect_support -> t -> [ `Supported | `Unsupported | `Unknown ]
val unknown_message : dialect_support -> t -> string
val unsupported_message : ?alternatives:bool -> dialect_support -> t -> string

val analyze : Sql.stmt -> dialect_support list

module Semantic : sig
  (** Every semantic dimension inference depends on, derived from one configuration. *)
  type t = private {
    outer_join_padding : Sql_semantics.Outer_join_padding.t;
    membership_null_semantics : Sql_semantics.Membership_null_semantics.t;
    nonnullable_write : Sql_semantics.Nonnullable_write.t;
    implicit_column_nullability : Sql_semantics.Implicit_column_nullability.t;
    nullable_scope : Sql_semantics.Nullable_scope.t;
    key_uniqueness : Sql_semantics.Key_uniqueness.t;
    omitted_insert_column : Sql_semantics.Omitted_insert_column.t;
    empty_aggregate : Sql_semantics.Empty_aggregate.t;
    where_alias_visibility : Sql_semantics.Where_alias_visibility.t;
    arrow_operator : Sql_semantics.Arrow_operator.t;
    column_type_syntax : Sql_semantics.Column_type_syntax.t;
    function_catalog : Sql_semantics.Function_catalog.t;
  }

  val of_configuration : configuration -> t

  type function_resolution = private
    | Standard_function
    | Dialect_function of Sql.Source_type.t Sql.func
    | Invalid_arity of { name : string; argument_count : int }

  val function_names : t -> string list
  val resolve_function : t -> string -> Sql.expr list -> function_resolution
end
