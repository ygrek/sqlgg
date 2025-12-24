open Sql

(* Проверяет, используется ли unsigned тип *)
let check_unsigned_type pos = function
  | Type.{ t = UInt64; _ } -> 
      [Dialect.get_unsigned_types pos]
  | _ -> []

(* Обход выражений для поиска dialect-специфичных конструкций *)
let rec analyze_expr = function
  | Value _ | Param _ | Inparam _ | Column _ | Of_values _ -> []
  | Choices (_, choices) ->
      List.concat_map (fun (_, expr_opt) -> 
        Option.map_default analyze_expr [] expr_opt
      ) choices
  | InChoice (_, _, expr) -> analyze_expr expr
  | Fun { parameters; _ } ->
      (* Анализируем параметры функций *)
      List.concat_map analyze_expr parameters
  | SelectExpr (select_full, _) -> analyze_select_full select_full
  | InTupleList { exprs; _ } -> List.concat_map analyze_expr exprs
  | OptionActions { choice; _ } -> analyze_expr choice
  | Case { case; branches; else_; } ->
      let case_features = Option.map_default analyze_expr [] case in
      let branches_features = List.concat_map (fun { when_; then_ } ->
        analyze_expr when_ @ analyze_expr then_
      ) branches in
      let else_features = Option.map_default analyze_expr [] else_ in
      case_features @ branches_features @ else_features

(* Обход column *)
and analyze_column = function
  | All | AllOf _ -> []
  | Expr (expr, _) -> analyze_expr expr

(* Обход source *)
and analyze_source (src, _alias, pos) =
  let _ = pos in (* позиция используется неявно через структуру source *)
  match src with
  | `Table _ -> []
  | `Select select_full -> analyze_select_full select_full
  | `Nested nested -> analyze_nested nested
  | `ValueRows row_values -> analyze_row_values row_values

(* Обход row_values *)
and analyze_row_values { row_constructor_list; row_order; row_limit = _ } =
  let constructor_features = match row_constructor_list with
  | RowExprList expr_lists ->
      List.concat_map (List.concat_map analyze_expr) expr_lists
  | RowParam _ -> []
  in
  let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) row_order in
  constructor_features @ order_features

(* Обход nested (joins) *)
and analyze_nested (src, joins) =
  let src_features = analyze_source src in
  let joins_features = List.concat_map (fun (joined_src, join_typ, join_cond) ->
    (* Проверяем join на subquery *)
    let (src_kind, _, _) = joined_src in
    let subquery_features = match src_kind with
    | `Select select_full ->
        Dialect.get_join_source joined_src :: analyze_select_full select_full
    | _ -> []
    in
    let joined_features = analyze_source joined_src in
    let cond_features = match join_cond with
    | Schema.Join.On expr -> analyze_expr expr
    | Schema.Join.Using _ -> []
    | Schema.Join.Natural -> []
    | Schema.Join.Default -> []
    in
    let join_typ_features = match join_typ.Schema.Join.typ with
    | Schema.Join.Inner -> [] (* straight_join уже обрабатывается в парсере *)
    | Schema.Join.Left | Schema.Join.Right | Schema.Join.Full -> []
    in
    subquery_features @ joined_features @ cond_features @ join_typ_features
  ) joins in
  src_features @ joins_features

(* Обход select *)
and analyze_select { columns; from; where; group; having } =
  let columns_features = List.concat_map analyze_column columns in
  let from_features = Option.map_default analyze_nested [] from in
  let where_features = Option.map_default analyze_expr [] where in
  let group_features = List.concat_map analyze_expr group in
  let having_features = Option.map_default analyze_expr [] having in
  columns_features @ from_features @ where_features @ group_features @ having_features

(* Обход select_complete *)
and analyze_select_complete { select; order; limit = _; _ } =
  let (core, others) = select in
  let core_features = analyze_select core in
  let others_features = List.concat_map (fun (_, select) -> analyze_select select) others in
  let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
  core_features @ others_features @ order_features

(* Обход select_full (с CTE) *)
and analyze_select_full { select_complete; cte } =
  let select_features = analyze_select_complete select_complete in
  let cte_features = Option.map_default (fun { cte_items; _ } ->
    List.concat_map (fun { stmt; _ } ->
      match stmt with
      | CteInline select_complete -> analyze_select_complete select_complete
      | CteSharedQuery _ -> []
    ) cte_items
  ) [] cte in
  select_features @ cte_features

(* Обход assignment_expr *)
and analyze_assignment_expr = function
  | RegularExpr expr -> analyze_expr expr
  | WithDefaultParam (expr, _) -> analyze_expr expr
  | AssignDefault -> []

(* Обход assignments *)
and analyze_assignments assignments =
  List.concat_map (fun (_, assignment_expr) -> analyze_assignment_expr assignment_expr) assignments

(* Обход column_def для поиска DEFAULT выражений *)
and analyze_column_def { domain; extra; pos; _ } =
  let pos = Option.default (0, 0) pos in
  (* Проверяем наличие AUTOINCREMENT *)
  let autoincrement_features = 
    if Constraints.mem Autoincrement extra then
      [Dialect.get_autoincrement pos]
    else []
  in
  (* Проверяем unsigned типы *)
  let unsigned_features = check_unsigned_type pos domain in
  (* Note: DEFAULT expressions обрабатываются в парсере через Parser_state.Default,
     но здесь мы их не можем извлечь из extra, так как они не сохранены в AST.
     Это одна из причин, почему некоторые features все еще должны обрабатываться
     в парсере *)
  autoincrement_features @ unsigned_features

(* Обход alter_action *)
and analyze_alter_action = function
  | `Add (col, _) -> analyze_column_def col
  | `Change (_, col, _) -> analyze_column_def col
  | `Drop _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `None -> []

(* Обход insert_action *)
and analyze_insert_action { action; on_conflict_clause; insert_action_kind; _ } =
  let action_features = match action with
  | `Values (_, Some values) ->
      List.concat_map (List.concat_map analyze_assignment_expr) values
  | `Values (_, None) -> []
  | `Param _ -> []
  | `Select (_, select_full) -> analyze_select_full select_full
  | `Set assignments -> Option.map_default analyze_assignments [] assignments
  in
  let conflict_features = match on_conflict_clause with
  | Some ({ conflict_clause_kind = On_duplicate { assignments; }; pos }) ->
      Dialect.get_on_duplicate_key pos :: analyze_assignments assignments
  | Some ({ conflict_clause_kind = On_conflict { action = Do_update assignments; _ }; pos }) ->
      Dialect.get_on_conflict pos :: analyze_assignments assignments
  | Some ({ conflict_clause_kind = On_conflict { action = Do_nothing; _ }; pos }) ->
      [Dialect.get_on_conflict pos]
  | None -> []
  in
  let replace_into_features = match insert_action_kind with
  | Replace_into pos -> [Dialect.get_replace_into pos]
  | Insert_into -> []
  in
  action_features @ conflict_features @ replace_into_features

let analyze_schema_index idx = match idx.index_kind with
  | Regular_idx -> None
  | Fulltext -> Some (Dialect.get_fulltext_index idx.pos)
  | Spatial -> None

(* Основная функция обхода statement *)
let rec analyze = function 
  | Create (_, Schema { schema; indexes; _ }) ->
      let schema_idx_features = List.filter_map analyze_schema_index indexes in
      schema_idx_features @ List.concat_map analyze_column_def schema
  | Create (_, Select { select; pos }) ->
      Dialect.get_create_table_as_select pos :: analyze_select_full select
  | Drop _ -> []
  | Alter (_, actions) ->
      List.concat_map analyze_alter_action actions
  | Rename _ -> []
  | CreateIndex _ -> []
  | Insert insert_action ->
      analyze_insert_action insert_action
  | Delete (_, where_opt) ->
      Option.map_default analyze_expr [] where_opt
  | DeleteMulti (_, nested, where_opt) ->
      let nested_features = analyze_nested nested in
      let where_features = Option.map_default analyze_expr [] where_opt in
      nested_features @ where_features
  | Set (assignments, stmt_opt) ->
      let assignments_features = List.concat_map (fun (_, expr) -> analyze_expr expr) assignments in
      let stmt_features = Option.map_default analyze [] stmt_opt in
      assignments_features @ stmt_features
  | Update (_, assignments, where_opt, order, _) ->
      let assignments_features = analyze_assignments assignments in
      let where_features = Option.map_default analyze_expr [] where_opt in
      let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
      assignments_features @ where_features @ order_features
  | UpdateMulti (nesteds, assignments, where_opt, order, _) ->
      let nesteds_features = List.concat_map analyze_nested nesteds in
      let assignments_features = analyze_assignments assignments in
      let where_features = Option.map_default analyze_expr [] where_opt in
      let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
      nesteds_features @ assignments_features @ where_features @ order_features
  | Select select_full ->
      analyze_select_full select_full
  | CreateRoutine (_, _, params) ->
      List.concat_map (fun (_, _, default_expr_opt) ->
        Option.map_default analyze_expr [] default_expr_opt
      ) params
