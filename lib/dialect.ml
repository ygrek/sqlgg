open Prelude

type t =
  | MySQL [@as "mysql"]
  | PostgreSQL [@as "postgresql"]
  | SQLite [@as "sqlite"]
  | TiDB [@as "tidb"]
  | ClickHouse [@as "clickhouse"]
[@@deriving eq, hash, show { with_path = false }, enumerate, to_string, of_string]

let of_yojson = function
  | `String name -> Stdlib.Option.to_result ~none:("unknown dialect " ^ name) (of_string name)
  | _ -> Error "dialect"

type row_store =
  | MySQL_configuration of Sql_semantics.Nonnullable_write.t
  | PostgreSQL_configuration
  | SQLite_configuration of Sql_semantics.Nonnullable_write.t
  | TiDB_configuration of Sql_semantics.Nonnullable_write.t

type configuration =
  | Row_store of row_store
  | ClickHouse_configuration of Sql_semantics.ClickHouse.t

let default_configuration = function
  | MySQL -> Row_store (MySQL_configuration Sql_semantics.Nonnullable_write.Reject)
  | PostgreSQL -> Row_store PostgreSQL_configuration
  | SQLite -> Row_store (SQLite_configuration Sql_semantics.Nonnullable_write.Reject)
  | TiDB -> Row_store (TiDB_configuration Sql_semantics.Nonnullable_write.Reject)
  | ClickHouse -> ClickHouse_configuration (Sql_semantics.ClickHouse.default ())

let row_store_dialect = function
  | MySQL_configuration _ -> MySQL
  | PostgreSQL_configuration -> PostgreSQL
  | SQLite_configuration _ -> SQLite
  | TiDB_configuration _ -> TiDB

let dialect_of_configuration = function
  | Row_store row_store -> row_store_dialect row_store
  | ClickHouse_configuration _ -> ClickHouse

let with_non_strict_writes = function
  | Row_store (MySQL_configuration _) ->
    Row_store (MySQL_configuration Sql_semantics.Nonnullable_write.Replace_with_default)
  | Row_store (SQLite_configuration _) ->
    Row_store (SQLite_configuration Sql_semantics.Nonnullable_write.Replace_with_default)
  | Row_store (TiDB_configuration _) ->
    Row_store (TiDB_configuration Sql_semantics.Nonnullable_write.Replace_with_default)
  | (Row_store PostgreSQL_configuration | ClickHouse_configuration _) as configuration -> configuration

type configuration_error =
  | Unknown_dialect of string
  | Settings_not_supported of row_store
  | Invalid_clickhouse_setting of Sql_semantics.ClickHouse.Setting.error

let parse_configuration specification =
  match String.split_on_char ',' specification with
  | [] -> Error (Unknown_dialect specification)
  | dialect_name :: raw_settings ->
    match of_string dialect_name with
    | None -> Error (Unknown_dialect dialect_name)
    | Some dialect ->
      match default_configuration dialect, raw_settings with
      | configuration, [] -> Ok configuration
      | Row_store row_store, _ :: _ -> Error (Settings_not_supported row_store)
      | ClickHouse_configuration configuration, raw_settings ->
        List.fold_left (fun configuration raw_setting ->
          Result.bind configuration (fun configuration ->
            match Sql_semantics.ClickHouse.Setting.parse raw_setting with
            | Ok setting -> Ok (Sql_semantics.ClickHouse.apply_setting configuration setting)
            | Error error -> Error (Invalid_clickhouse_setting error)))
          (Ok configuration) raw_settings
        |> Result.map (fun configuration -> ClickHouse_configuration configuration)

type feature =
  | Collation [@as "collation"]
  | JoinOnSubquery [@as "join_on_subquery"]
  | CreateTableAsSelect [@as "create_table_as_select"]
  | OnDuplicateKey [@as "on_duplicate_key"]
  | OnConflict [@as "on_conflict"]
  | StraightJoin [@as "straight_join"]
  | LockInShareMode [@as "lock_in_share_mode"]
  | FulltextIndex [@as "fulltext_index"]
  | UnsignedTypes [@as "unsigned_types"]
  | AutoIncrement [@as "autoincrement"]
  | ReplaceInto [@as "replace_into"]
  | RowLocking [@as "row_locking"]
  | DefaultExpr [@as "default_expr"]
  | Ttl [@as "ttl"]
  | CachedTable [@as "cached_table"]
  | AlterColumn [@as "alter_column"]
  | AlterAlgorithm [@as "alter_algorithm"]
  | AlterLock [@as "alter_lock"]
  | UserDefinedType [@as "user_defined_type"]
  | Extension [@as "extension"]
[@@deriving show { with_path = false }, enumerate, to_string, of_string]

let show_feature x = 
  match x with 
  | DefaultExpr -> "with this kind of default expressions"
  | x -> show_feature x

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

let support { state; _ } dialect =
  if List.mem dialect state.supported then `Supported
  else if List.mem dialect state.unsupported then `Unsupported
  else `Unknown

let unknown_message { feature; _ } dialect =
  Printf.sprintf "Feature %s has unknown support for dialect %s" (show_feature feature) (show dialect)

let unsupported_message ?(alternatives = true) { state; feature; _ } dialect =
  let suffix =
    match state.supported with
    | (_ :: _) as dialects when alternatives ->
      Printf.sprintf " (supported by: %s)" (String.concat ", " (List.map show dialects))
    | [] | _ :: _ -> ""
  in
  Printf.sprintf "Feature %s is not supported for dialect %s%s" (show_feature feature) (show dialect) suffix

let all_except excluded = List.filter (fun d -> not (List.mem d excluded)) all

let make_only_state supported = {
  supported;
  unsupported = all_except supported;
  unknown = []
}

let supported feature l pos = {
  feature; pos;
  state = { supported = l; unsupported = []; unknown = List.filter (fun x -> not (List.mem x l)) all }
}

let only feature l pos = {
  feature; pos;
  state = make_only_state l
}

let get_collation collation pos =
  let clean_collation = 
    String.trim collation |> fun s ->
      if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
        String.sub s 1 (String.length s - 2)
      else s
  in
  let lower = String.lowercase_ascii clean_collation in
  match lower with
  | "binary" -> supported Collation [SQLite; MySQL; TiDB] pos
  | s when List.exists (fun suffix -> String.ends_with s ~suffix) ["_ci"; "_cs"; "_bin"; "_as_cs"; "_as_cs_ks"] ->
      only Collation [MySQL; TiDB] pos
  | s when String.ends_with ~suffix:"-x-icu" s -> only Collation [PostgreSQL] pos
  | "c" | "c.utf8" | "c.utf-8" | "posix" | "pg_c_utf8" | "ucs_basic"
  | "default" | "unicode" ->
      only Collation [PostgreSQL] pos
  | "nocase" | "rtrim" -> only Collation [SQLite] pos
  | _ -> supported Collation [] pos

let get_join_source s pos =
  match s with
  | `Select _ -> {
      feature = JoinOnSubquery; pos;
      state = make_only_state (all_except [TiDB])
    }
  | #Sql.source_kind -> supported JoinOnSubquery all pos

let get_create_table_as_select pos = {
  feature = CreateTableAsSelect; pos;
  state = make_only_state (all_except [TiDB])
}

let get_on_duplicate_key pos = only OnDuplicateKey [MySQL; TiDB] pos

let get_on_conflict pos = only OnConflict [SQLite; PostgreSQL] pos

let get_straight_join pos = only StraightJoin [MySQL; TiDB] pos

let get_lock_in_share_mode pos = only LockInShareMode [MySQL] pos

let get_fulltext_index pos = only FulltextIndex [MySQL] pos

let get_unsigned_types pos = only UnsignedTypes [MySQL; TiDB] pos

let get_autoincrement pos = only AutoIncrement [SQLite; MySQL; TiDB] pos

let get_replace_into pos = only ReplaceInto [MySQL; TiDB] pos

let get_row_locking pos = only RowLocking [PostgreSQL; MySQL; TiDB] pos

let get_ttl pos = only Ttl [TiDB] pos

let get_cached_table pos = only CachedTable [TiDB] pos

let get_alter_column (change : Sql.Alter_column_pg.t) pos =
  match change with
  | Set_type _ | Set_not_null | Drop_not_null -> only AlterColumn [PostgreSQL] pos
  | Set_default | Drop_default -> only AlterColumn [MySQL; PostgreSQL; TiDB] pos

let get_alter_option ({ Sql.value; pos } : Sql.alter_option Sql.located) =
  match value with
  | Alter_algorithm _ -> only AlterAlgorithm [MySQL; TiDB] pos
  | Alter_lock _ -> only AlterLock [MySQL] pos

let get_user_defined_type pos = only UserDefinedType [PostgreSQL] pos

let get_extension pos = only Extension [PostgreSQL] pos

let get_default_expr ~kind ~expr pos =
  let open Sql in
  let tidb_only_functions =
    [ "NOW"; "CURRENT_TIMESTAMP"; "LOCALTIME"; "LOCALTIMESTAMP"
    ; "RAND"; "UUID"; "UUID_SHORT"; "UUID_TO_BIN"; "UPPER"; "REPLACE"
    ; "DATE_FORMAT"; "STR_TO_DATE"; "CURRENT_DATE"
    ; "JSON_OBJECT"; "JSON_ARRAY"; "JSON_QUOTE"
    ; "NEXTVAL"; "VEC_FROM_TEXT"
    ]
  in
  let rec analyze = function
    | Value _ -> (true, false, true)
    | Column _ -> (true, true, false)
    | Case _ as e -> fold_parts (sub_exprs e)
    | Fun { parameters; _ } -> fold_parts parameters
    | Param _ | Inparam _ | Choices _ | InChoice _
    | SelectExpr _ | InTupleList _ | OptionActions _ | Of_values _ | Lambda _ ->
        (false, false, false)
  and fold_parts parts =
    List.fold_left
      (fun (v_acc, c_acc, o_acc) e ->
        let v, c, o = analyze e in
        (v_acc && v, c_acc || c, o_acc && o))
      (true, false, true)
      parts
  in
  let valid, has_column, only_value = analyze expr in
  if not valid then only DefaultExpr [] pos
  else
    let base_dialects =
      match expr with
      | Case _ -> all_except [ TiDB ]
      | Fun { fn_name; _ } ->
          if List.mem (String.uppercase_ascii fn_name) tidb_only_functions then
            all
          else
            all_except [ TiDB ]
      (* 
        https://docs.pingcap.com/tidb/stable/data-type-default-values/
        TiDB supports assigning default values to BLOB, TEXT, and JSON data types. 
        However, you can only use expressions, not literals, to define default values for these data types.
       *)
      | Value _ when List.exists (fun x -> 
          Option.map_default (Sql.Source_type.equal_kind (Sql.Source_type.Infer x)) false kind) [Json; Text; Blob] -> all_except [ TiDB ]
      | _ -> all
    in
    let dialects =
      List.filter (fun d -> not (has_column && d = PostgreSQL) && (only_value || d <> SQLite)) base_dialects
    in
    only DefaultExpr dialects pos


module Semantic = struct
  module ClickHouse_function = struct
    type t =
      | Array_map of unit list [@as "arraymap"] [@no_args]
      | Array_filter [@as "arrayfilter"]
      | Array_sum [@as "arraysum"]
      | Has [@as "has"]
      | Length [@as "length"]
    [@@deriving of_string, to_string]

    type parse_error =
      | Unknown_name
      | Invalid_arity

    let names = List.map to_string [Array_map []; Array_filter; Array_sum; Has; Length]

    let parse raw_name argument_count =
      match raw_name, argument_count with
      | "arraymap", count when count >= 2 ->
        Ok (Array_map (List.init (count - 2) (fun _ -> ())))
      | "arraymap", _ -> Error Invalid_arity
      | name, count ->
        match of_string name, count with
        | Some Array_filter, 2 -> Ok Array_filter
        | Some Array_sum, 1 -> Ok Array_sum
        | Some Has, 2 -> Ok Has
        | Some Length, 1 -> Ok Length
        | Some (Array_map _ | Array_filter | Array_sum | Has | Length), _ ->
          Error Invalid_arity
        | None, _ -> Error Unknown_name

    let signature = function
      | Array_map additional_arrays ->
        let array_count = 1 + List.length additional_arrays in
        let element_templates = List.init array_count (fun index -> Sql.Type.Var index) in
        let result_template = Sql.Type.Var array_count in
        Sql.(F
          (Type.Array_of result_template,
           Type.Fun_of (element_templates, result_template)
           :: List.map (fun template -> Type.Array_of template) element_templates))
      | Array_filter ->
        Sql.(F
          (Type.Array_of (Type.Var 0),
           [Type.Fun_of ([Type.Var 0], Type.Typ (Type.strict Type.Bool));
            Type.Array_of (Type.Var 0)]))
      | Array_sum -> Sql.(F (Type.Var 0, [Type.Array_of (Type.Var 0)]))
      | Has ->
        Sql.(F
          (Type.Typ (Type.strict Type.Bool),
           [Type.Array_of (Type.Var 0); Type.Var 0]))
      | Length ->
        Sql.(F
          (Type.Typ (Type.strict Type.Int), [Type.Array_of (Type.Var 0)]))
  end

  type t = {
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

  type function_resolution =
    | Standard_function
    | Dialect_function of Sql.Source_type.t Sql.func
    | Invalid_arity of { name : string; argument_count : int }

  let of_configuration = function
    | Row_store row_store -> {
        outer_join_padding = Sql_semantics.Outer_join_padding.Null;
        membership_null_semantics = Sql_semantics.Membership_null_semantics.Three_valued;
        nonnullable_write =
          begin match row_store with
          | MySQL_configuration nonnullable_write
          | SQLite_configuration nonnullable_write
          | TiDB_configuration nonnullable_write -> nonnullable_write
          | PostgreSQL_configuration -> Sql_semantics.Nonnullable_write.Reject
          end;
        implicit_column_nullability = Sql_semantics.Implicit_column_nullability.Nullable;
        nullable_scope = Sql_semantics.Nullable_scope.Any_type;
        key_uniqueness = Sql_semantics.Key_uniqueness.Enforced;
        omitted_insert_column = Sql_semantics.Omitted_insert_column.Requires_declared_default;
        empty_aggregate = Sql_semantics.Empty_aggregate.Null;
        where_alias_visibility =
          begin match row_store with
          | SQLite_configuration _ -> Sql_semantics.Where_alias_visibility.Select_aliases_visible
          | MySQL_configuration _ | PostgreSQL_configuration | TiDB_configuration _ ->
            Sql_semantics.Where_alias_visibility.Source_columns_only
          end;
        arrow_operator = Sql_semantics.Arrow_operator.Json_extract;
        column_type_syntax = Sql_semantics.Column_type_syntax.Standard_types;
        function_catalog = Sql_semantics.Function_catalog.Standard_functions;
      }
    | ClickHouse_configuration configuration -> {
        outer_join_padding = configuration.outer_join_padding;
        membership_null_semantics =
          Sql_semantics.Membership_null_semantics.Two_valued configuration.null_membership;
        nonnullable_write = configuration.nonnullable_write;
        implicit_column_nullability = configuration.implicit_column_nullability;
        nullable_scope = Sql_semantics.Nullable_scope.Scalar_types;
        key_uniqueness = Sql_semantics.Key_uniqueness.Not_enforced;
        omitted_insert_column = Sql_semantics.Omitted_insert_column.Filled_with_type_default;
        empty_aggregate = configuration.empty_aggregate;
        where_alias_visibility = Sql_semantics.Where_alias_visibility.Select_aliases_visible;
        arrow_operator = Sql_semantics.Arrow_operator.Lambda;
        column_type_syntax = Sql_semantics.Column_type_syntax.ClickHouse_types;
        function_catalog = Sql_semantics.Function_catalog.ClickHouse_functions;
      }

  let function_names { function_catalog; _ } =
    match function_catalog with
    | Sql_semantics.Function_catalog.Standard_functions -> Sql.Function.names ()
    | Sql_semantics.Function_catalog.ClickHouse_functions ->
      List.rev_append (Sql.Function.names ()) ClickHouse_function.names
      |> List.sort_uniq String.compare

  let resolve_function { function_catalog; _ } name parameters =
    match function_catalog with
    | Sql_semantics.Function_catalog.Standard_functions -> Standard_function
    | Sql_semantics.Function_catalog.ClickHouse_functions ->
      let argument_count = List.length parameters in
      match ClickHouse_function.parse name argument_count with
      | Ok function_ -> Dialect_function (ClickHouse_function.signature function_)
      | Error ClickHouse_function.Unknown_name -> Standard_function
      | Error ClickHouse_function.Invalid_arity -> Invalid_arity { name; argument_count }
end

open Sql

let check_unsigned_type pos = function
  (* UInt64 is a native ClickHouse type. [UNSIGNED] is a separate MySQL/TiDB
     type modifier even though both map to the same inference type. *)
  | Source_type.Infer Type.UInt64 -> []
  | Source_type.Int { sign = Unsigned; _ } -> [get_unsigned_types pos]
  | _ -> []

let check_collation_opt (collation : string located option) =
  match collation with
  | Some { value; pos } -> [get_collation value pos]
  | None -> []

let check_collated (c : _ collated) =
  check_collation_opt c.collation

let rec analyze_expr acc exprs k = match exprs with
  | [] -> k acc
  | expr :: rest ->
    match expr with
    | Fun { parameters; _ } ->
        analyze_expr acc (parameters @ rest) k
    | SelectExpr (select_full, _) -> 
        analyze_select_full acc [select_full] (fun acc -> analyze_expr acc rest k)
    | e ->
        analyze_expr acc (sub_exprs e @ rest) k

and analyze_column acc cols k = match cols with
  | [] -> k acc
  | col :: rest ->
    match col with
    | { value = (All | AllOf _); _ } -> analyze_column acc rest k
    | { value = Expr ({ value = expr; _ }, _); _ } -> analyze_expr acc [expr] (fun acc -> analyze_column acc rest k)

and analyze_source acc srcs k = match srcs with
  | [] -> k acc
  | src :: rest ->
    match src with
    | `Table _ -> analyze_source acc rest k
    | `Select select_full -> analyze_select_full acc [select_full] (fun acc -> analyze_source acc rest k)
    | `Nested nested -> analyze_nested acc [nested] (fun acc -> analyze_source acc rest k)
    | `ValueRows row_values -> analyze_row_values acc [row_values] (fun acc -> analyze_source acc rest k)

and analyze_row_values acc rvs k = match rvs with
  | [] -> k acc
  | { row_constructor_list; row_order; row_limit = _ } :: rest ->
    let constructor_exprs = match row_constructor_list with
      | RowExprList expr_lists -> List.concat expr_lists
      | RowParam _ -> []
    in
    let order_exprs = List.map fst row_order in
    analyze_expr acc (constructor_exprs @ order_exprs) (fun acc -> analyze_row_values acc rest k)

and analyze_nested acc nesteds k = match nesteds with
  | [] -> k acc
  | ((src_kind, _), joins) :: rest ->
    let rec analyze_joins acc joins k = match joins with
      | [] -> k acc
      | { value = ((join_src_kind, _), join_typ, join_cond); pos } :: joins_rest ->
        let acc = match join_src_kind with
          | `Select _ -> get_join_source join_src_kind pos :: acc
          | _ -> acc
        in
        let acc = match join_typ.value with
          | Schema.Join.Straight -> get_straight_join join_typ.pos :: acc
          | Schema.Join.Inner | Left | Right | Full -> acc
        in
        analyze_source acc [join_src_kind] (fun acc ->
          let cond_exprs = match join_cond with
            | Schema.Join.On expr -> [expr]
            | Schema.Join.Using _ | Natural | Default -> []
          in
          analyze_expr acc cond_exprs (fun acc ->
            analyze_joins acc joins_rest k))
    in
    analyze_source acc [src_kind] (fun acc ->
      analyze_joins acc joins (fun acc ->
        analyze_nested acc rest k))

and analyze_select acc sels k = match sels with
  | [] -> k acc
  | { columns; from; where; group; having; _ } :: rest ->
    analyze_column acc columns (fun acc ->
      let nested_opt = option_list from in
      analyze_nested acc nested_opt (fun acc ->
        let where_exprs = option_list where in
        analyze_expr acc (where_exprs @ group) (fun acc ->
          let having_exprs = option_list having in
          analyze_expr acc having_exprs (fun acc ->
            analyze_select acc rest k))))

and analyze_select_complete acc scs k = match scs with
  | [] -> k acc
  | { select = (core, others); order; limit = _; select_row_locking } :: rest ->
    let acc = match select_row_locking with
      | Some { value = For_share; pos } -> get_lock_in_share_mode pos :: acc
      | Some { value = For_update; pos } -> get_row_locking pos :: acc
      | None -> acc
    in
    let all_selects = core :: List.map snd others in
    analyze_select acc all_selects (fun acc ->
      let order_exprs = List.map fst order in
      analyze_expr acc order_exprs (fun acc ->
        analyze_select_complete acc rest k))

and analyze_select_full acc sfs k = match sfs with
  | [] -> k acc
  | { select_complete; cte } :: rest ->
    analyze_select_complete acc [select_complete] (fun acc ->
      let cte_selects = Option.map_default (fun { cte_items; _ } ->
        List.filter_map (fun { stmt; _ } ->
          match stmt with
          | CteInline sc -> Some sc
          | CteSharedQuery _ -> None
        ) cte_items
      ) [] cte in
      analyze_select_complete acc cte_selects (fun acc ->
        analyze_select_full acc rest k))

and analyze_assignment_expr acc aes k = match aes with
  | [] -> k acc
  | ae :: rest ->
    let exprs = match ae with
      | RegularExpr expr -> [expr]
      | WithDefaultParam (expr, _) -> [expr]
      | AssignDefault -> []
    in
    analyze_expr acc exprs (fun acc -> analyze_assignment_expr acc rest k)

and analyze_column_def_internal acc cds k = match cds with
  | [] -> k acc
  | ({ kind; extra; _ }: Alter_action_attr.t) :: rest ->
    let acc = 
      let autoincrement = List.find_opt (fun c ->
        match c.value with 
        | Alter_action_attr.Syntax_constraint Autoincrement -> true 
        | _ -> false
      ) extra in
      match autoincrement with
      | Some { pos; _ } -> get_autoincrement pos :: acc
      | None -> acc
    in
    let acc = extra
      |> List.find_map (fun c ->
          match c.value with
          | Alter_action_attr.Default { expr = { value = expr; _ }; _ } ->
              let col_kind = Option.map (fun k -> k.value.collated) kind in
              Some (get_default_expr ~kind:col_kind ~expr c.pos)
          | _ -> None)
      |> Option.map_default (fun f -> f :: acc) acc
    in
    let acc = kind |> Option.map (fun k -> k.value) |> Option.map_default (fun c -> check_collated c @ acc) acc in
    let acc = match kind with
      | Some { pos; value = { collated; _ } } -> check_unsigned_type pos collated @ acc
      | None -> acc
    in
    analyze_column_def_internal acc rest k

and analyze_alter_action acc actions k = match actions with
  | [] -> k acc
  | action :: rest ->
    match action with
    | `Add (col, _) -> analyze_column_def_internal acc [col] (fun acc -> analyze_alter_action acc rest k)
    | `Change (_, col, _) -> analyze_column_def_internal acc [col] (fun acc -> analyze_alter_action acc rest k)
    | `Default_or_convert_to (_, collation) -> 
        let acc = check_collation_opt collation @ acc in
        analyze_alter_action acc rest k
    | `TtlOptions (_, pos) | `RemoveTtl pos ->
        let acc = get_ttl pos :: acc in
        analyze_alter_action acc rest k
    | `Cache pos | `NoCache pos ->
        let acc = get_cached_table pos :: acc in
        analyze_alter_action acc rest k
    | `AlterColumnPG (_, { value; pos }) ->
        let acc = get_alter_column value pos :: acc in
        analyze_alter_action acc rest k
    | `Drop _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `AddIndex _ | `DropIndex _ | `AddPrimaryKey _ | `DropPrimaryKey | `AddConstraint _ | `DropConstraint _ ->
        analyze_alter_action acc rest k

and analyze_insert_action acc ias k = match ias with
  | [] -> k acc
  | { action; on_conflict_clause; insert_action_kind; _ } :: rest ->
    let acc = match insert_action_kind with
      | Replace_into pos -> get_replace_into pos :: acc
      | Insert_into -> acc
    in
    let acc, conflict_assignments = match on_conflict_clause with
      | Some ({ value = On_duplicate { assignments }; pos }) ->
          (get_on_duplicate_key pos :: acc, assignments)
      | Some ({ value = On_conflict { action = Do_update assignments; _ }; pos }) ->
          (get_on_conflict pos :: acc, assignments)
      | Some ({ value = On_conflict { action = Do_nothing; _ }; pos }) ->
          (get_on_conflict pos :: acc, [])
      | None -> (acc, [])
    in
    let analyze_action acc k = match action with
      | `Values (_, Some values) ->
          let aes = List.concat values in
          analyze_assignment_expr acc aes k
      | `Values (_, None) | `Param _ -> k acc
      | `Select (_, select_full) -> analyze_select_full acc [select_full] k
      | `Set assignments -> 
          let aes = Option.map_default (List.map snd) [] assignments in
          analyze_assignment_expr acc aes k
    in
    analyze_action acc (fun acc ->
      let conflict_aes = List.map snd conflict_assignments in
      analyze_assignment_expr acc conflict_aes (fun acc ->
        analyze_insert_action acc rest k))

let analyze_schema_index idx = match idx.value.Sql.idx_kind with
  | Regular_idx -> None
  | Fulltext -> Some (get_fulltext_index idx.pos)
  | Spatial -> None

let rec analyze stmt = 
  let acc = [] in
  match stmt with
  | Sql.Create (_, Schema { schema; indexes; _ }) ->
      let acc = List.rev_append (List.filter_map analyze_schema_index indexes) acc in
      analyze_column_def_internal acc schema List.rev
  | Create (_, Select { value = select; pos }) ->
      let acc = get_create_table_as_select pos :: acc in
      analyze_select_full acc [select] List.rev
  | Drop _ -> []
  | Alter { alter_actions = actions; alter_options = options; _ } ->
      let acc = List.rev_append (List.map get_alter_option options) acc in
      analyze_alter_action acc actions List.rev
  | Rename _ -> []
  | CreateIndex { ci_cols; _ } -> List.concat_map check_collated ci_cols
  | Insert insert_action ->
      analyze_insert_action acc [insert_action] List.rev
  | Delete (_, where_opt) ->
      analyze_expr acc (option_list where_opt) List.rev
  | DeleteMulti (_, nested, where_opt) ->
      analyze_nested acc [nested] (fun acc ->
        analyze_expr acc (option_list where_opt) List.rev)
  | Set (assignments, stmt_opt) ->
      let exprs = List.map snd assignments in
      analyze_expr acc exprs (fun acc ->
        let stmt_features = Option.map_default analyze [] stmt_opt in
        List.rev (List.rev_append stmt_features acc))
  | Update (_, assignments, where_opt, order, _) ->
      let aes = List.map snd assignments in
      analyze_assignment_expr acc aes (fun acc ->
        let exprs = option_list where_opt @ List.map fst order in
        analyze_expr acc exprs List.rev)
  | UpdateMulti (from, assignments, where_opt, order, _) ->
      analyze_nested acc [from] (fun acc ->
        let aes = List.map snd assignments in
        analyze_assignment_expr acc aes (fun acc ->
          let exprs = option_list where_opt @ List.map fst order in
          analyze_expr acc exprs List.rev))
  | Select select_full ->
      analyze_select_full acc [select_full] List.rev
  | CreateRoutine (_, kind, params) ->
      let acc = kind |> Option.map (fun k -> k.value) |> Option.map_default (fun c -> check_collated c @ acc) acc in
      let acc = match kind with
        | Some { pos; value = { collated; _ } } -> check_unsigned_type pos collated @ acc
        | None -> acc
      in
      let rec process_params acc = function
        | [] -> List.rev acc
        | (_, typ, default_expr_opt) :: rest ->
            let acc = check_collated typ.value @ acc in
            let acc = check_unsigned_type typ.pos typ.value.collated @ acc in
            analyze_expr acc (option_list default_expr_opt) (fun acc -> process_params acc rest)
      in
      process_params acc params
  | CreateType _ -> [get_user_defined_type (0, 0)]
  | DropType _ -> [get_user_defined_type (0, 0)]
  | CreateExtension _ -> [get_extension (0, 0)]
  | DropExtension _ -> [get_extension (0, 0)]
