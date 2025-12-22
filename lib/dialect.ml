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

let get_join_source (s, _) pos =
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

let get_default_expr ~function_name ~kind ~expr pos =
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
      | Case _ | Fun _ ->
          begin match function_name with
          | Some name
            when List.mem (String.uppercase_ascii name) tidb_only_functions ->
              all
          | _ -> all_except [ TiDB ]
          end
      (* 
        https://docs.pingcap.com/tidb/stable/data-type-default-values/
        TiDB supports assigning default values to BLOB, TEXT, and JSON data types. 
        However, you can only use expressions, not literals, to define default values for these data types.
       *)
      | Value _ when List.exists (fun x -> 
          Option.map_default (Type.equal_kind x) false kind) [Json; Text; Blob] -> all_except [ TiDB ]
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
