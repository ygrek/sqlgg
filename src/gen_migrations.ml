open Printf
open ExtLib
open Sqlgg

let quote_id name =
  sprintf "`%s`" name

let quote_table_name (name : Sql.table_name) =
  Option.map_default (fun db -> sprintf "`%s`.`%s`" db name.tn) (quote_id name.tn) name.db

let type_kind_to_sql = function
  | Sql.Type.Int -> "INT"
  | UInt64 -> "BIGINT UNSIGNED"
  | Text -> "TEXT"
  | Blob -> "BLOB"
  | Float -> "FLOAT"
  | Bool -> "BOOLEAN"
  | Datetime -> "DATETIME"
  | Decimal { precision = Some p; scale = Some s } -> sprintf "DECIMAL(%d,%d)" p s
  | Decimal { precision = Some p; scale = None } -> sprintf "DECIMAL(%d)" p
  | Decimal _ -> "DECIMAL"
  | Json -> "JSON"
  | Union { ctors; _ } ->
    sprintf "ENUM(%s)"
      (String.concat ", " (List.map (sprintf "'%s'") (Sql.Type.Enum_kind.Ctors.elements ctors)))
  | Any | StringLiteral _ | Json_path | One_or_all -> "TEXT"
  | FloatingLiteral _ -> "FLOAT"

let source_type_kind_to_sql = function
  | Sql.Source_type.Infer k -> type_kind_to_sql k
  | Int (None, Sql.Signed) -> "INT"
  | Int (Some Tiny, Signed) -> "TINYINT" | Int (Some Small, Signed) -> "SMALLINT"
  | Int (Some Medium, Signed) -> "MEDIUMINT" | Int (Some Big, Signed) -> "BIGINT"
  | Int (None, Unsigned) -> "INT UNSIGNED"
  | Int (Some Tiny, Unsigned) -> "TINYINT UNSIGNED" | Int (Some Small, Unsigned) -> "SMALLINT UNSIGNED"
  | Int (Some Medium, Unsigned) -> "MEDIUMINT UNSIGNED" | Int (Some Big, Unsigned) -> "BIGINT UNSIGNED"
  | Float Sql.Single -> "FLOAT"
  | Float Double -> "DOUBLE"
  | Blob None -> "BLOB"
  | Blob (Some Tiny) -> "TINYBLOB" | Blob (Some Medium) -> "MEDIUMBLOB"
  | Blob (Some Long) -> "LONGBLOB"
  | Text None -> "TEXT"
  | Text (Some Tiny) -> "TINYTEXT" | Text (Some Medium) -> "MEDIUMTEXT"
  | Text (Some Long) -> "LONGTEXT"

let constraint_to_sql = function
  | Sql.Constraint.PrimaryKey -> Some "PRIMARY KEY"
  | NotNull -> Some "NOT NULL"
  | Null -> Some "NULL"
  | Unique -> Some "UNIQUE"
  | Autoincrement -> Some "AUTO_INCREMENT"
  | WithDefault -> None
  | OnConflict _ -> None
  | Composite _ -> None

let alter_action_attr_to_sql (col : Sql.Alter_action_attr.t) =
  let name = quote_id col.name in
  let kind = Option.map_default (fun (k : _ Sql.collated Sql.located) -> " " ^ source_type_kind_to_sql k.value.collated) "" col.kind in
  let extras = col.extra |> List.filter_map (fun (c : Sql.Alter_action_attr.constraint_ Sql.located) ->
    match c.value with
    | Syntax_constraint cst -> constraint_to_sql cst
    | Default _ -> None
  ) in
  let extras = match extras with [] -> "" | l -> " " ^ String.concat " " l in
  sprintf "%s%s%s" name kind extras

let attr_to_column_sql (attr : Sql.attr) =
  let col = Sql.Alter_action_attr.from_attr attr in
  alter_action_attr_to_sql col

let alter_pos_to_sql = function
  | `Default -> ""
  | `First -> " FIRST"
  | `After col -> sprintf " AFTER %s" (quote_id col)

let enrich_with_source_kind source_kind (col : Sql.Alter_action_attr.t) =
  Option.map_default (fun sk ->
    { col with kind = Some (Sql.make_located ~pos:(0,0)
      ~value:(Sql.make_collated ~collated:sk ())) }) col source_kind

let find_column columns col_name =
  List.find (fun (c : Tables.column) -> c.attr.Sql.name = col_name) columns

let inverse_action table_name (columns : Tables.column list) (action : Sql.alter_action) : Sql.alter_action =
  match action with
  | `Add (col, _pos) -> `Drop col.Sql.Alter_action_attr.name
  | `Drop col_name ->
    let entry = find_column columns col_name in
    let col = Sql.Alter_action_attr.from_attr entry.attr |> enrich_with_source_kind entry.source_kind in
    `Add (col, `Default)
  | `Change (old_name, _new_col, _pos) ->
    let entry = find_column columns old_name in
    let old_col = Sql.Alter_action_attr.from_attr entry.attr |> enrich_with_source_kind entry.source_kind in
    `Change (_new_col.Sql.Alter_action_attr.name, old_col, `Default)
  | `RenameTable _new_name -> `RenameTable table_name
  | `RenameColumn (old_name, new_name) -> `RenameColumn (new_name, old_name)
  | `RenameIndex (old_name, new_name) -> `RenameIndex (new_name, old_name)
  | `AddIndex (Some name, _cols) -> `DropIndex name
  | `AddIndex (None, _) -> `None
  | `DropIndex _name -> `None
  | `DropPrimaryKey ->
    let pk_cols = Tables.get_primary_key_columns columns in
    `AddPrimaryKey pk_cols
  | `AddPrimaryKey _cols -> `DropPrimaryKey
  | `AddConstraint (Some name) -> `DropConstraint name
  | `AddConstraint None -> `None
  | `DropConstraint _name -> `None
  | `Default_or_convert_to _ ->
    let cs, collation = match Tables.get_charset table_name with
      | Some old -> old.charset, Option.map (fun v -> Sql.make_located ~pos:(0,0) ~value:v) old.collation
      | None -> None, None
    in
    `Default_or_convert_to (cs, collation)
  | `None -> `None

let action_to_sql_fragment (action : Sql.alter_action) =
  match action with
  | `Add (col, pos) ->
    sprintf "ADD COLUMN %s%s" (alter_action_attr_to_sql col) (alter_pos_to_sql pos)
  | `Drop col_name ->
    sprintf "DROP COLUMN %s" (quote_id col_name)
  | `Change (old_name, new_col, pos) ->
    sprintf "CHANGE COLUMN %s %s%s" (quote_id old_name)
      (alter_action_attr_to_sql new_col) (alter_pos_to_sql pos)
  | `RenameTable new_name ->
    sprintf "RENAME TO %s" (quote_table_name new_name)
  | `RenameColumn (old_name, new_name) ->
    sprintf "RENAME COLUMN %s TO %s" (quote_id old_name) (quote_id new_name)
  | `RenameIndex (old_name, new_name) ->
    sprintf "RENAME INDEX %s TO %s" (quote_id old_name) (quote_id new_name)
  | `AddIndex (name, cols) ->
    let name_s = Option.map_default (fun n -> " " ^ quote_id n) "" name in
    sprintf "ADD INDEX%s (%s)" name_s (String.concat ", " (List.map quote_id cols))
  | `DropIndex name ->
    sprintf "DROP INDEX %s" (quote_id name)
  | `AddPrimaryKey cols ->
    sprintf "ADD PRIMARY KEY (%s)" (String.concat ", " (List.map quote_id cols))
  | `DropPrimaryKey ->
    "DROP PRIMARY KEY"
  | `AddConstraint name ->
    sprintf "ADD CONSTRAINT%s" (Option.map_default (fun n -> " " ^ quote_id n) "" name)
  | `DropConstraint name ->
    sprintf "DROP CONSTRAINT %s" (quote_id name)
  | `Default_or_convert_to (cs, collation) ->
    let charset_to_sql = function
      | Sql.Named s -> s
      | Binary -> "binary"
      | Ascii -> "ascii"
      | Unicode -> "unicode"
    in
    let parts = List.filter_map Fun.id [
      Option.map (fun c -> sprintf "CONVERT TO CHARACTER SET %s" (charset_to_sql c)) cs;
      Option.map (fun c -> sprintf "COLLATE %s" c.Sql.value) collation;
    ] in
    begin match parts with
    | [] -> "(* unsupported: unknown previous charset *)"
    | _ -> String.concat " " parts
    end
  | `None -> "(* unsupported: index/constraint operation *)"

let alter_to_sql table_name actions =
  let fragments = List.map action_to_sql_fragment actions in
  sprintf "ALTER TABLE %s %s" (quote_table_name table_name) (String.concat ", " fragments)

type migration = {
  name : string;
  apply : string;
  revert : string;
}

let inverse table_name (columns : Tables.column list) (actions : Sql.alter_action list) =
  let non_invertible = List.filter (function
    | `None -> true
    | `AddIndex (None, _) | `DropIndex _ -> true
    | `AddConstraint None | `DropConstraint _ -> true
    | _ -> false) actions in
  if non_invertible <> [] then
    None
  else
    let inverse_actions = List.rev_map (inverse_action table_name columns) actions in
    let effective_name = List.fold_left (fun name action ->
      match action with `RenameTable new_name -> new_name | _ -> name
    ) table_name actions in
    Some (alter_to_sql effective_name inverse_actions)

let drop_index_sql index_name table_name =
  sprintf "DROP INDEX %s ON %s" (quote_id index_name) (quote_table_name table_name)

let rename_inverse_sql pairs =
  let inverse_pairs = List.map (fun (old_name, new_name) ->
    sprintf "%s TO %s" (quote_table_name new_name) (quote_table_name old_name)
  ) pairs in
  sprintf "RENAME TABLE %s" (String.concat ", " inverse_pairs)

