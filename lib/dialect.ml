type t = MySQL | PostgreSQL | SQLite | TiDB [@@deriving show { with_path = false }]

type feature =
  | Collation
  | JoinOnSubquery
  | CreateTableAsSelect
  | OnDuplicateKey
  | OnConflict
[@@deriving show { with_path = false }]

let feature_to_string = function
  | Collation -> "collation"
  | JoinOnSubquery -> "join_on_subquery"
  | CreateTableAsSelect -> "create_table_as_select"
  | OnDuplicateKey -> "on_duplicate_key"
  | OnConflict -> "on_conflict"

let feature_of_string s =
  match String.lowercase_ascii s with
  | "collation" -> Collation
  | "join_on_subquery" -> JoinOnSubquery
  | "create_table_as_select" -> CreateTableAsSelect
  | "on_duplicate_key" -> OnDuplicateKey
  | "on_conflict" -> OnConflict
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
