open Prelude

type t = MySQL | PostgreSQL | SQLite | TiDB [@@deriving eq, show { with_path = false }]

let selected = ref MySQL

let set_selected d = selected := d

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
[@@deriving show { with_path = false }]

let show_feature x = 
  match x with 
  | DefaultExpr -> "with this kind of default expressions"
  | x -> show_feature x

let feature_to_string = function
  | Collation -> "collation"
  | JoinOnSubquery -> "join_on_subquery"
  | CreateTableAsSelect -> "create_table_as_select"
  | OnDuplicateKey -> "on_duplicate_key"
  | OnConflict -> "on_conflict"
  | StraightJoin -> "straight_join"
  | LockInShareMode -> "lock_in_share_mode"
  | FulltextIndex -> "fulltext_index"
  | UnsignedTypes -> "unsigned_types"
  | AutoIncrement -> "autoincrement"
  | ReplaceInto -> "replace_into"
  | RowLocking -> "row_locking"
  | DefaultExpr -> "default_expr"

let feature_of_string s =
  match String.lowercase_ascii s with
  | "collation" -> Collation
  | "join_on_subquery" -> JoinOnSubquery
  | "create_table_as_select" -> CreateTableAsSelect
  | "on_duplicate_key" -> OnDuplicateKey
  | "on_conflict" -> OnConflict
  | "straight_join" -> StraightJoin
  | "lock_in_share_mode" -> LockInShareMode
  | "fulltext_index" -> FulltextIndex
  | "unsigned_types" -> UnsignedTypes
  | "autoincrement" -> AutoIncrement
  | "replace_into" -> ReplaceInto
  | "row_locking" -> RowLocking
  | "default_expr" -> DefaultExpr
  | _ -> failwith (Printf.sprintf "Unknown feature: %s" s)

type support_state = {
  supported : t list;
  unsupported : t list; 
  unknown : t list;
}

type dialect_support = {
  feature : feature;
  pos : Sql.pos;
  state : support_state;
}

let all = [MySQL; PostgreSQL; SQLite; TiDB]

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
    | Case { case; branches; else_ } ->
        let parts = option_list case @ option_list else_ in
        let parts = parts @ List.concat_map (fun { Sql.when_; then_ } -> [when_; then_]) branches in
        List.fold_left
          (fun (v_acc, c_acc, o_acc) e ->
            let v, c, o = analyze e in
            (v_acc && v, c_acc || c, o_acc && o))
          (true, false, true)
          parts
    | Fun { parameters; _ } ->
        List.fold_left
          (fun (v_acc, c_acc, o_acc) e ->
            let v, c, o = analyze e in
            (v_acc && v, c_acc || c, o_acc && o))
          (true, false, true)
          parameters
    | ( Param _ | Inparam _ | Choices _ | InChoice _
      | SelectExpr _ | InTupleList _ | OptionActions _ | Of_values _ ) ->
        (false, false, false)
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
      base_dialects
      |> List.to_seq
      |> Seq.filter (fun d -> not (has_column && d = PostgreSQL))
      |> Seq.filter (fun d -> only_value || d <> SQLite)
      |> List.of_seq
    in
    only DefaultExpr dialects pos


module Semantic = struct 
  let is_where_aliases_dialect () = !selected = SQLite

  let is_non_strict_mode_is_exists () = List.mem !selected [MySQL; TiDB; SQLite]
end

open Sql

let check_unsigned_type pos = function
  | Source_type.Infer Type.UInt64 -> [get_unsigned_types pos]
  | UInt32 -> [get_unsigned_types pos]
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
    | Value _ | Param _ | Inparam _ | Column _ | Of_values _ -> 
        analyze_expr acc rest k
    | Choices (_, choices) ->
        let new_exprs = List.filter_map (fun (_, expr_opt) -> expr_opt) choices in
        analyze_expr acc (new_exprs @ rest) k
    | InChoice (_, _, e) -> 
        analyze_expr acc (e :: rest) k
    | Fun { parameters; _ } ->
        analyze_expr acc (parameters @ rest) k
    | SelectExpr (select_full, _) -> 
        analyze_select_full acc [select_full] (fun acc -> analyze_expr acc rest k)
    | InTupleList { value = { exprs; _ }; _ } -> 
        analyze_expr acc (exprs @ rest) k
    | OptionActions { choice; _ } -> 
        analyze_expr acc (choice :: rest) k
    | Case { case; branches; else_ } ->
        let case_exprs = option_list case in
        let branches_exprs = List.concat_map (fun { when_; then_ } -> [when_; then_]) branches in
        let else_exprs = option_list else_ in
        analyze_expr acc (case_exprs @ branches_exprs @ else_exprs @ rest) k

and analyze_column acc cols k = match cols with
  | [] -> k acc
  | col :: rest ->
    match col with
    | All | AllOf _ -> analyze_column acc rest k
    | Expr ({ value = expr; _ }, _) -> analyze_expr acc [expr] (fun acc -> analyze_column acc rest k)

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
  | { columns; from; where; group; having } :: rest ->
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
      |> List.find_map (function
          | { value = Alter_action_attr.Default { value = expr; pos }; _ } ->
              let col_kind = Option.map (fun k -> k.value.collated) kind in
              Some (get_default_expr ~kind:col_kind ~expr pos)
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
    | `Default_or_convert_to collation -> 
        let acc = check_collation_opt collation @ acc in
        analyze_alter_action acc rest k
    | `Drop _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `None -> 
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

let analyze_schema_index idx = match idx.value with
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
  | Alter (_, actions) ->
      analyze_alter_action acc actions List.rev
  | Rename _ -> []
  | CreateIndex (_, _, cols) -> List.concat_map check_collated cols
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
  | UpdateMulti (nesteds, assignments, where_opt, order, _) ->
      analyze_nested acc nesteds (fun acc ->
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

    