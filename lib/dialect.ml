open Prelude
open Jsonkit.Primitives

type t =
  | MySQL [@as "mysql"]
  | PostgreSQL [@as "postgresql"]
  | SQLite [@as "sqlite"]
  | TiDB [@as "tidb"]
[@@deriving eq, hash, show { with_path = false }, enumerate, to_string, of_string, json, jsonschema] [@@compact_variants]

let of_yojson = function
  | `String name -> Stdlib.Option.to_result ~none:("unknown dialect " ^ name) (of_string name)
  | _ -> Error "dialect"

let selected = ref MySQL

let set_selected d = selected := d

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
  | OnUpdateTimestamp [@as "on_update_timestamp"]
  | ColumnComment [@as "column_comment"]
  | PartitionOperations [@as "partition_operations"]
  | RecoverTable [@as "recover_table"]
  | RebaseAutoId [@as "rebase_auto_id"]
  | AutoIdCache [@as "auto_id_cache"]
  | ShardRowIdBits [@as "shard_row_id_bits"]
  | IndexVisibility [@as "index_visibility"]
  | IndexComment [@as "index_comment"]
  | TableComment [@as "table_comment"]
  | DatabaseDdl [@as "database_ddl"]
  | DatabaseCharsetAndCollation [@as "database_charset_and_collation"]
  | TruncateTable [@as "truncate_table"]
  | CreateTableLike [@as "create_table_like"]
  | RenameTable [@as "rename_table"]
  | TiflashReplica [@as "tiflash_replica"]
  | SequenceDdl [@as "sequence_ddl"]
  | EnforcedCheck [@as "enforced_check"]
[@@deriving show { with_path = false }, enumerate, to_string, of_string, json, jsonschema] [@@compact_variants]

let show_feature x =
  match x with
  | DefaultExpr -> "with this kind of default expressions"
  | x -> show_feature x

type support_state = {
  supported : t list;
  unsupported : t list;
  unknown : t list;
}
[@@deriving json, jsonschema]

type dialect_support = {
  feature : feature;
  pos : Sql.Pos.t;
  state : support_state;
}
[@@deriving json, jsonschema]

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

let unsupported feature l pos = {
  feature; pos;
  state = { supported = []; unsupported = l; unknown = List.filter (fun x -> not (List.mem x l)) all }
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
  | #Sql.relation -> supported JoinOnSubquery all pos

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
    | SelectExpr _ | InTupleList _ | OptionActions _ | Of_values _ ->
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
  let is_where_aliases_dialect () = !selected = SQLite

  let is_non_strict_mode_is_exists () = List.mem !selected [MySQL; TiDB; SQLite]
end

open Sql

let check_unsigned_type pos = function
  | Source_type.Infer Type.UInt64 -> [get_unsigned_types pos]
  | Source_type.Int { sign = Unsigned; _ } -> [get_unsigned_types pos]
  | _ -> []

let check_collation_opt (collation : string located option) =
  match collation with
  | Some { value; pos } -> [get_collation value pos]
  | None -> []

let check_collated (c : _ collated) =
  check_collation_opt c.collation

(* what a walk over a statement finds, latest first *)
type found = { features : dialect_support list; relation_slots : param_id list }

let note acc feature = { acc with features = feature :: acc.features }
let note_all acc features = { acc with features = features @ acc.features }

let rec analyze_expr acc exprs k = match exprs with
  | [] -> k acc
  | expr :: rest ->
    match expr with
    | SelectExpr (select_full, _) ->
        analyze_select_full acc [select_full] (fun acc -> analyze_expr acc rest k)
    | Value _ | Param _ | Inparam _ | Column _ | Of_values _ | Choices _ | InChoice _ | OptionActions _ | Fun _
    | InTupleList _ | Case _ as e ->
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
    | `Relation (`Table _) -> analyze_source acc rest k
    | `RelationSlot id -> analyze_source { acc with relation_slots = id :: acc.relation_slots } rest k
    | `Relation (`Select select_full) -> analyze_select_full acc [select_full] (fun acc -> analyze_source acc rest k)
    | `Relation (`Nested nested) -> analyze_nested acc [nested] (fun acc -> analyze_source acc rest k)
    | `Relation (`ValueRows row_values) -> analyze_row_values acc [row_values] (fun acc -> analyze_source acc rest k)

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
          | `Relation (`Select _ as relation) -> note acc (get_join_source relation pos)
          | `Relation (`Table _ | `Nested _ | `ValueRows _) | `RelationSlot _ -> acc
        in
        let acc = match join_typ.value with
          | Schema.Join.Straight -> note acc (get_straight_join join_typ.pos)
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
      | Some { value = For_share; pos } -> note acc (get_lock_in_share_mode pos)
      | Some { value = For_update; pos } -> note acc (get_row_locking pos)
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
    let acc, default_feature =
      List.fold_left
        (fun (acc, default_feature) (c : Alter_action_attr.constraint_ located) ->
          match c.value with
          | Syntax_constraint Autoincrement -> note acc (get_autoincrement c.pos), default_feature
          | On_update -> note acc (only OnUpdateTimestamp [MySQL; TiDB] c.pos), default_feature
          | Comment _ -> note acc (only ColumnComment [MySQL; TiDB] c.pos), default_feature
          | Collate col -> note acc (get_collation col c.pos), default_feature
          | Check (Some enforcement) -> note acc (only EnforcedCheck [MySQL; TiDB] enforcement.pos), default_feature
          | Default { expr = { value = expr; _ }; _ } ->
              let default_feature = match default_feature with
                | Some _ -> default_feature
                | None ->
                    let col_kind = Option.map (fun k -> k.value.collated) kind in
                    Some (get_default_expr ~kind:col_kind ~expr c.pos)
              in
              acc, default_feature
          | Syntax_constraint _ | Check None | Generated -> acc, default_feature)
        (acc, None) extra
    in
    let acc = Option.map_default (note acc) acc default_feature in
    let acc = match kind with
      | Some { pos; value } -> note_all acc (check_unsigned_type pos value.collated @ check_collated value)
      | None -> acc
    in
    analyze_column_def_internal acc rest k

and analyze_alter_action acc actions k = match actions with
  | [] -> k acc
  | action :: rest ->
    match action with
    | `Add (col, _) | `Change (_, col, _) ->
        analyze_column_def_internal acc [col] (fun acc -> analyze_alter_action acc rest k)
    | `Default_or_convert_to (_, Some c) | `TableCollation c ->
        analyze_alter_action (note acc (get_collation c.value c.pos)) rest k
    | `TableComment _ ->
        analyze_alter_action (note acc (only TableComment [MySQL; TiDB] dummy_pos)) rest k
    | `AlterIndexVisibility _ ->
        analyze_alter_action (note acc (only IndexVisibility [MySQL; TiDB] dummy_pos)) rest k
    | `TtlOptions (_, pos) | `RemoveTtl pos ->
        analyze_alter_action (note acc (get_ttl pos)) rest k
    | `Cache pos | `NoCache pos ->
        analyze_alter_action (note acc (get_cached_table pos)) rest k
    | `AlterColumnPG (_, { value; pos }) ->
        analyze_alter_action (note acc (get_alter_column value pos)) rest k
    | `AddIndex { add_idx_kind; add_idx_options; _ } ->
        let acc = match add_idx_kind with
          | Fulltext_idx -> note acc (get_fulltext_index dummy_pos)
          | Plain_idx | Unique_idx | Spatial_idx -> acc
        in
        analyze_alter_action (analyze_index_options acc add_idx_options) rest k
    | `AddPrimaryKey (_, opts) ->
        analyze_alter_action (analyze_index_options acc opts) rest k
    | `AddConstraint (_, c) ->
        analyze_alter_action (analyze_table_constraint acc c) rest k
    | `AlterCheck _ ->
        analyze_alter_action (note acc (only EnforcedCheck [MySQL; TiDB] dummy_pos)) rest k
    | `Default_or_convert_to (_, None)
    | `Drop _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `DropIndex _ | `DropPrimaryKey | `DropConstraint _ ->
        analyze_alter_action acc rest k
    | `SetTiflashReplica _ ->
        analyze_alter_action (note acc (only TiflashReplica [TiDB] dummy_pos)) rest k
    | `AutoIncrement (Rebase, _) ->
        analyze_alter_action (note acc (only RebaseAutoId [MySQL; TiDB] dummy_pos)) rest k
    | `AutoIncrement (Force_rebase, _) | `AutoRandomBase _ ->
        analyze_alter_action (note acc (only RebaseAutoId [TiDB] dummy_pos)) rest k
    | `AutoIdCache _ ->
        analyze_alter_action (note acc (only AutoIdCache [TiDB] dummy_pos)) rest k
    | `ShardRowIdBits _ ->
        analyze_alter_action (note acc (only ShardRowIdBits [TiDB] dummy_pos)) rest k
    | `DropPartition _ | `TruncatePartition _ | `TruncateAllPartitions | `ExchangePartition _ | `RemovePartitioning
    | `AddPartition _ | `ReorganizePartition _ | `PartitionBy _ ->
        let exprs = match action with
          | `AddPartition defs | `ReorganizePartition (_, defs) -> Sql.partition_exprs defs
          | `PartitionBy pb -> Sql.partition_by_exprs pb
          | _ -> []
        in
        let acc = note acc (only PartitionOperations [MySQL; TiDB] dummy_pos) in
        analyze_expr acc exprs (fun acc -> analyze_alter_action acc rest k)

and analyze_index_options acc (opts : Sql.index_option Sql.located list) =
  List.fold_left
    (fun acc ({ Sql.value; pos } : Sql.index_option Sql.located) ->
      match value with
      | Index_visibility _ -> note acc (only IndexVisibility [MySQL; TiDB] pos)
      | Index_comment _ -> note acc (only IndexComment [MySQL; TiDB] pos))
    acc opts

and analyze_table_constraint acc = function
  | `Primary (_, opts) | `Unique (_, _, opts) -> analyze_index_options acc opts
  | `Check (Some enforcement) -> note acc (only EnforcedCheck [MySQL; TiDB] enforcement.pos)
  | `Check None | `Foreign _ -> acc

and analyze_insert_action acc ias k = match ias with
  | [] -> k acc
  | { action; on_conflict_clause; insert_action_kind; _ } :: rest ->
    let acc = match insert_action_kind with
      | Replace_into pos -> note acc (get_replace_into pos)
      | Insert_into -> acc
    in
    let acc, conflict_assignments = match on_conflict_clause with
      | Some ({ value = On_duplicate { assignments }; pos }) ->
          (note acc (get_on_duplicate_key pos), assignments)
      | Some ({ value = On_conflict { action = Do_update assignments; _ }; pos }) ->
          (note acc (get_on_conflict pos), assignments)
      | Some ({ value = On_conflict { action = Do_nothing; _ }; pos }) ->
          (note acc (get_on_conflict pos), [])
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

(** the dialect features a statement uses and the relation slots in it, both in the order of the walk:
    the CTEs of a SELECT come after its body *)
let analyze stmt =
  let rec walk acc stmt k =
    (* features that do not depend on what is inside the statement, in order *)
    let listed features = k (note_all acc (List.rev features)) in
    match stmt with
    | Sql.Create (_, Schema { schema; constraints; indexes }) ->
        let acc =
          List.fold_left
            (fun acc (idx : Sql.table_inline_index Sql.located) ->
              let acc = match idx.value.idx_kind with
                | Regular_idx | Spatial -> acc
                | Fulltext -> note acc (get_fulltext_index idx.pos)
              in
              analyze_index_options acc idx.value.idx_options)
            acc indexes
        in
        let acc = List.fold_left analyze_table_constraint acc constraints in
        analyze_column_def_internal acc schema k
    | Create (_, Select { value = select; pos }) ->
        let acc = note acc (get_create_table_as_select pos) in
        analyze_select_full acc [select] k
    | Create (_, Like _) -> listed [only CreateTableLike [MySQL; TiDB] dummy_pos]
    | Drop _ | DropView _ -> k acc
    | Alter { alter_actions = actions; alter_options = options; _ } ->
        let acc = note_all acc (List.rev (List.map get_alter_option options)) in
        analyze_alter_action acc actions k
    | Rename _ -> listed [only RenameTable [MySQL; TiDB] dummy_pos]
    | CreateIndex { ci_cols; ci_options; _ } ->
        k (analyze_index_options (note_all acc (List.rev (List.concat_map check_collated ci_cols))) ci_options)
    | Insert insert_action ->
        analyze_insert_action acc [insert_action] k
    | Delete (_, where_opt) ->
        analyze_expr acc (option_list where_opt) k
    | DeleteMulti (_, nested, where_opt) ->
        analyze_nested acc [nested] (fun acc ->
          analyze_expr acc (option_list where_opt) k)
    | Set (assignments, stmt_opt) ->
        let exprs = List.map snd assignments in
        analyze_expr acc exprs (fun acc ->
          match stmt_opt with
          | Some stmt -> walk acc stmt k
          | None -> k acc)
    | Update (_, assignments, where_opt, order, _) ->
        let aes = List.map snd assignments in
        analyze_assignment_expr acc aes (fun acc ->
          let exprs = option_list where_opt @ List.map fst order in
          analyze_expr acc exprs k)
    | UpdateMulti (from, assignments, where_opt, order, _) ->
        analyze_nested acc [from] (fun acc ->
          let aes = List.map snd assignments in
          analyze_assignment_expr acc aes (fun acc ->
            let exprs = option_list where_opt @ List.map fst order in
            analyze_expr acc exprs k))
    | Select select_full ->
        analyze_select_full acc [select_full] k
    | CreateRoutine (_, kind, params) ->
        let acc = kind |> Option.map (fun k -> k.value) |> Option.map_default (fun c -> note_all acc (check_collated c)) acc in
        let acc = match kind with
          | Some { pos; value = { collated; _ } } -> note_all acc (check_unsigned_type pos collated)
          | None -> acc
        in
        let rec process_params acc = function
          | [] -> k acc
          | (_, typ, default_expr_opt) :: rest ->
              let acc = note_all acc (check_collated typ.value) in
              let acc = note_all acc (check_unsigned_type typ.pos typ.value.collated) in
              analyze_expr acc (option_list default_expr_opt) (fun acc -> process_params acc rest)
        in
        process_params acc params
    | CreateType _ | DropType _ -> listed [get_user_defined_type (0, 0)]
    | CreateExtension _ | DropExtension _ -> listed [get_extension (0, 0)]
    | CreateDatabase (_, None) | DropDatabase _ -> listed [only DatabaseDdl [MySQL; TiDB; PostgreSQL] dummy_pos]
    | CreateDatabase (_, Some opt)
    | AlterDatabase (_, Alter_db_option opt) ->
        let collation_features =
          match opt with
          | Database_charset _ -> []
          | Database_collation c | Database_charset_and_collation (_, c) -> [ get_collation c.value c.pos ]
        in
        listed
          (only DatabaseDdl [MySQL; TiDB; PostgreSQL] dummy_pos
           :: only DatabaseCharsetAndCollation [MySQL; TiDB] dummy_pos
           :: collation_features)
    | AlterDatabase (_, Alter_db_tiflash_replica _) ->
        listed [only DatabaseDdl [MySQL; TiDB; PostgreSQL] dummy_pos; only TiflashReplica [TiDB] dummy_pos]
    | Truncate _ -> listed [only TruncateTable [MySQL; TiDB; PostgreSQL] dummy_pos]
    | RecoverTable _ -> listed [only RecoverTable [TiDB] dummy_pos]
    | CreateSequence _ | AlterSequence _ | DropSequence _ -> listed [only SequenceDdl [PostgreSQL; TiDB] dummy_pos]
  in
  walk { features = []; relation_slots = [] } stmt (fun acc -> List.rev acc.features, List.rev acc.relation_slots)
