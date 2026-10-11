module Outer_join_padding : sig
  type t =
    | Null
    | Type_default
  [@@deriving enumerate]
end

module Null_membership : sig
  type t =
    | Null_never_matches
    | Null_matches_null
  [@@deriving show, enumerate]
end

module Membership_null_semantics : sig
  type t =
    | Three_valued
    | Two_valued of Null_membership.t
  [@@deriving show, enumerate]
end

module Nonnullable_write : sig
  type t =
    | Reject
    | Replace_with_default
end

module Implicit_column_nullability : sig
  type t =
    | Strict
    | Nullable
end

module Nullable_scope : sig
  type t =
    | Any_type
    | Scalar_types
  [@@deriving enumerate]
end

module Key_uniqueness : sig
  type t =
    | Enforced
    | Not_enforced
end

module Omitted_insert_column : sig
  type t =
    | Requires_declared_default
    | Filled_with_type_default
end

module Empty_aggregate : sig
  type t =
    | Null
    | Type_default
end

module Where_alias_visibility : sig
  type t =
    | Source_columns_only
    | Select_aliases_visible
end

module Arrow_operator : sig
  type t =
    | Json_extract
    | Lambda
end

module Column_type_syntax : sig
  type t =
    | Standard_types
    | ClickHouse_types
end

module Function_catalog : sig
  type t =
    | Standard_functions
    | ClickHouse_functions
end

module ClickHouse : sig
  type t = private {
    outer_join_padding : Outer_join_padding.t;
    null_membership : Null_membership.t;
    nonnullable_write : Nonnullable_write.t;
    implicit_column_nullability : Implicit_column_nullability.t;
    empty_aggregate : Empty_aggregate.t;
  }

  val default : unit -> t

  module Setting : sig
    type name =
      | Join_use_nulls
      | Transform_null_in
      | Insert_null_as_default
      | Data_type_default_nullable
      | Aggregate_functions_null_for_empty

    val name_to_string : name -> string

    type t

    type error =
      | Malformed_setting of string
      | Unknown_setting of string
      | Invalid_setting_value of { setting : name; value : string }

    val parse : string -> (t, error) result
  end

  val apply_setting : t -> Setting.t -> t
end
