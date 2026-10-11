module Outer_join_padding = struct
  type t =
    | Null
    | Type_default
  [@@deriving enumerate]
end

module Null_membership = struct
  type t =
    | Null_never_matches
    | Null_matches_null
  [@@deriving show { with_path = false }, enumerate]
end

module Membership_null_semantics = struct
  type t =
    | Three_valued
    | Two_valued of Null_membership.t
  [@@deriving show { with_path = false }, enumerate]
end

module Nonnullable_write = struct
  type t =
    | Reject
    | Replace_with_default
end

module Implicit_column_nullability = struct
  type t =
    | Strict
    | Nullable
end

module Nullable_scope = struct
  type t =
    | Any_type
    | Scalar_types
  [@@deriving enumerate]
end

module Key_uniqueness = struct
  type t =
    | Enforced
    | Not_enforced
end

module Omitted_insert_column = struct
  type t =
    | Requires_declared_default
    | Filled_with_type_default
end

module Empty_aggregate = struct
  type t =
    | Null
    | Type_default
end

module Where_alias_visibility = struct
  type t =
    | Source_columns_only
    | Select_aliases_visible
end

module Arrow_operator = struct
  type t =
    | Json_extract
    | Lambda
end

module Column_type_syntax = struct
  type t =
    | Standard_types
    | ClickHouse_types
end

module Function_catalog = struct
  type t =
    | Standard_functions
    | ClickHouse_functions
end

module ClickHouse = struct
  type t = {
    outer_join_padding : Outer_join_padding.t;
    null_membership : Null_membership.t;
    nonnullable_write : Nonnullable_write.t;
    implicit_column_nullability : Implicit_column_nullability.t;
    empty_aggregate : Empty_aggregate.t;
  }

  let default () = {
    outer_join_padding = Outer_join_padding.Type_default;
    null_membership = Null_membership.Null_never_matches;
    nonnullable_write = Nonnullable_write.Reject;
    implicit_column_nullability = Implicit_column_nullability.Strict;
    empty_aggregate = Empty_aggregate.Type_default;
  }

  module Setting = struct
    type name =
      | Join_use_nulls [@as "join_use_nulls"]
      | Transform_null_in [@as "transform_null_in"]
      | Insert_null_as_default [@as "insert_null_as_default"]
      | Data_type_default_nullable [@as "data_type_default_nullable"]
      | Aggregate_functions_null_for_empty [@as "aggregate_functions_null_for_empty"]
    [@@deriving of_string, to_string]

    type t =
      | Join_use_nulls of Outer_join_padding.t
      | Transform_null_in of Null_membership.t
      | Insert_null_as_default of Nonnullable_write.t
      | Data_type_default_nullable of Implicit_column_nullability.t
      | Aggregate_functions_null_for_empty of Empty_aggregate.t

    type error =
      | Malformed_setting of string
      | Unknown_setting of string
      | Invalid_setting_value of { setting : name; value : string }

    type binary =
      | Disabled [@as "0"]
      | Enabled [@as "1"]
    [@@deriving of_string]

    let parse raw_setting =
      match String.split_on_char '=' raw_setting with
      | [] | [_] | _ :: _ :: _ :: _ -> Error (Malformed_setting raw_setting)
      | [raw_name; value] ->
        match name_of_string raw_name with
        | None -> Error (Unknown_setting raw_name)
        | Some setting ->
          match setting, binary_of_string value with
          | (Join_use_nulls | Transform_null_in | Insert_null_as_default
            | Data_type_default_nullable | Aggregate_functions_null_for_empty), None ->
            Error (Invalid_setting_value { setting; value })
          | Join_use_nulls, Some Disabled -> Ok (Join_use_nulls Outer_join_padding.Type_default)
          | Join_use_nulls, Some Enabled -> Ok (Join_use_nulls Outer_join_padding.Null)
          | Transform_null_in, Some Disabled -> Ok (Transform_null_in Null_membership.Null_never_matches)
          | Transform_null_in, Some Enabled -> Ok (Transform_null_in Null_membership.Null_matches_null)
          | Insert_null_as_default, Some Disabled -> Ok (Insert_null_as_default Nonnullable_write.Reject)
          | Insert_null_as_default, Some Enabled ->
            Ok (Insert_null_as_default Nonnullable_write.Replace_with_default)
          | Data_type_default_nullable, Some Disabled ->
            Ok (Data_type_default_nullable Implicit_column_nullability.Strict)
          | Data_type_default_nullable, Some Enabled ->
            Ok (Data_type_default_nullable Implicit_column_nullability.Nullable)
          | Aggregate_functions_null_for_empty, Some Disabled ->
            Ok (Aggregate_functions_null_for_empty Empty_aggregate.Type_default)
          | Aggregate_functions_null_for_empty, Some Enabled ->
            Ok (Aggregate_functions_null_for_empty Empty_aggregate.Null)
  end

  let apply_setting configuration = function
    | Setting.Join_use_nulls outer_join_padding -> { configuration with outer_join_padding }
    | Setting.Transform_null_in null_membership -> { configuration with null_membership }
    | Setting.Insert_null_as_default nonnullable_write -> { configuration with nonnullable_write }
    | Setting.Data_type_default_nullable implicit_column_nullability ->
      { configuration with implicit_column_nullability }
    | Setting.Aggregate_functions_null_for_empty empty_aggregate ->
      { configuration with empty_aggregate }
end
