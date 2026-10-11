module Outer_join_padding : sig
  type t =
    | Null
    | Type_default
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

module ClickHouse : sig
  type t = private {
    outer_join_padding : Outer_join_padding.t;
    null_membership : Null_membership.t;
    nonnullable_write : Nonnullable_write.t;
    implicit_column_nullability : Implicit_column_nullability.t;
  }

  val default : unit -> t

  module Setting : sig
    type name =
      | Join_use_nulls
      | Transform_null_in
      | Insert_null_as_default
      | Data_type_default_nullable

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
