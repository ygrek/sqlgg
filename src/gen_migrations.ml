open Printf
open Sqlgg

exception Migration_error of string
let fail fmt = ksprintf (fun s -> raise (Migration_error s)) fmt

let loc value = Sql.make_located ~pos:(0,0) ~value

let quote_id name =
  sprintf "`%s`" (String.concat "``" (String.split_on_char '`' name))

let quote_table_name (name : Sql.table_name) =
  Option.map_default (fun db -> sprintf "`%s`.`%s`" db name.tn) (quote_id name.tn) name.db

let with_len base = Option.map_default (sprintf "%s(%d)" base) base

let escape_sql_string s = String.concat "''" (String.split_on_char '\'' s)

let type_kind_to_sql (k : Sql.Type.kind) = match k with
  | Int -> "INT"
  | UInt64 -> "BIGINT UNSIGNED"
  | Text -> "TEXT"
  | Blob -> "BLOB"
  | Float -> "FLOAT"
  | Bool -> "BOOLEAN"
  | Datetime -> "DATETIME"
  | Decimal { precision = Some p; scale = Some s } -> sprintf "DECIMAL(%d,%d)" p s
  | Decimal { precision; scale = None } -> with_len "DECIMAL" precision
  | Decimal { precision = None; scale = Some _ } ->
    fail "DECIMAL with scale but without precision is not representable"
  | Json -> "JSON"
  | Union { ctors; _ } ->
    let ctors = Sql.Type.Enum_kind.Ctors.elements ctors in
    sprintf "ENUM(%s)"
      (String.concat ", " (List.map (fun c -> sprintf "'%s'" (escape_sql_string c)) ctors))
  | (Any | StringLiteral _ | Json_path | One_or_all) as k ->
    fail "cannot serialize inferred type %s for migration SQL (non-storable)"
      (Sql.Type.show_kind k)
  | FloatingLiteral _ -> "FLOAT"

let int_size_name : Sql.int_size option -> string = function
  | None -> "INT"
  | Some Tiny -> "TINYINT"
  | Some Small -> "SMALLINT"
  | Some Medium -> "MEDIUMINT"
  | Some Big -> "BIGINT"

let lob_size_name ~plain ~tiny ~medium ~long : Sql.lob_size option -> string = function
  | None -> plain
  | Some Tiny -> tiny
  | Some Medium -> medium
  | Some Long -> long

let blob_plain_name =
  lob_size_name ~plain:"BLOB" ~tiny:"TINYBLOB" ~medium:"MEDIUMBLOB" ~long:"LONGBLOB"

let text_plain_name =
  lob_size_name ~plain:"TEXT" ~tiny:"TINYTEXT" ~medium:"MEDIUMTEXT" ~long:"LONGTEXT"

let source_type_kind_to_sql (k : Sql.Source_type.kind) = match k with
  | Infer k -> type_kind_to_sql k
  | Int { size; sign; display_width } ->
    let suffix = match sign with Signed -> "" | Unsigned -> " UNSIGNED" in
    with_len (int_size_name size) display_width ^ suffix
  | Float Single -> "FLOAT"
  | Float Double -> "DOUBLE"
  | Blob (PlainBlob s) -> blob_plain_name s
  | Blob (Varbinary len) -> with_len "VARBINARY" len
  | Text (PlainText s) -> text_plain_name s
  | Text (Char len) -> with_len "CHAR" len
  | Text (Varchar len) -> with_len "VARCHAR" len
  | Text (Varchar2 len) -> with_len "VARCHAR2" len

let constraint_to_sql = function
  | Sql.Constraint.PrimaryKey -> Some "PRIMARY KEY"
  | NotNull -> Some "NOT NULL"
  | Null -> Some "NULL"
  | Unique -> Some "UNIQUE"
  | Autoincrement -> Some "AUTO_INCREMENT"
  | WithDefault | OnConflict _ | Composite _ -> None

let alter_action_attr_to_sql ~default_sql_lookup (col : Sql.Alter_action_attr.t) =
  let name = quote_id col.name in
  let kind_sql =
    Option.map_default (fun (k : Sql.Source_type.kind Sql.collated Sql.located) ->
      let type_sql = " " ^ source_type_kind_to_sql k.value.collated in
      Option.map_default (fun c -> type_sql ^ " COLLATE " ^ c.Sql.value) type_sql k.value.collation)
      "" col.kind
  in
  let extras =
    col.extra |> List.filter_map (fun (c : Sql.Alter_action_attr.constraint_ Sql.located) ->
      match c.value with
      | Syntax_constraint cst -> constraint_to_sql cst
      | Default _ -> Option.map (sprintf "DEFAULT %s") (default_sql_lookup col.name))
  in
  let extras_sql = match extras with [] -> "" | l -> " " ^ String.concat " " l in
  sprintf "%s%s%s" name kind_sql extras_sql

let alter_pos_to_sql = function
  | `Default -> ""
  | `First -> " FIRST"
  | `After col -> sprintf " AFTER %s" (quote_id col)

let inverse_action ~table_name ~current_name ~pk_columns ~pk_dropped
    (action : Sql.alter_action) =
  let restore_column ~src col_name =
    match Tables.column_find table_name ~column_name:col_name with
    | Some (e : Tables.column) ->
      let col = Sql.Alter_action_attr.from_attr e.attr in
      Option.map_default (fun sk -> { col with kind = Some (loc sk) }) col e.source_kind
    | None -> fail "%s %s: column not tracked in schema state" src (quote_id col_name)
  in
  match action with
  | `Add (col, _pos) -> `Drop col.Sql.Alter_action_attr.name
  | `Drop col_name when List.mem col_name pk_columns
                     && pk_columns <> [col_name]
                     && not pk_dropped ->
    fail "DROP COLUMN %s: column participates in composite PRIMARY KEY (%s); \
          write a composite ALTER explicitly: \
          DROP COLUMN %s, DROP PRIMARY KEY, ADD PRIMARY KEY (<remaining cols>)"
      (quote_id col_name)
      (String.concat ", " (List.map quote_id pk_columns))
      (quote_id col_name)
  | `Drop col_name ->
    `Add (restore_column ~src:"DROP COLUMN" col_name, `Default)
  | `Change (old_name, new_col, `Default) ->
    let col = restore_column ~src:"CHANGE COLUMN" old_name in
    `Change (new_col.Sql.Alter_action_attr.name, col, `Default)
  | `Change (old_name, _, (`First | `After _)) ->
    fail "CHANGE COLUMN %s with FIRST/AFTER: original column position is not tracked, \
          cannot restore on revert" (quote_id old_name)
  | `RenameTable _ -> `RenameTable current_name
  | `RenameColumn (old_name, new_name) -> `RenameColumn (new_name, old_name)
  | `RenameIndex (old_name, new_name) -> `RenameIndex (new_name, old_name)
  | `AddIndex { add_idx_name = Some name; _ } -> `DropIndex name
  | `AddIndex { add_idx_name = None; _ } ->
    fail "ADD INDEX without a name has no identifier to DROP later"
  | `DropIndex name ->
    (match Tables.index_find table_name ~index_name:name with
     | Some (idx : Tables.stored_index) ->
       `AddIndex { Sql.add_idx_name = Some name;
                   add_idx_kind = idx.kind;
                   add_idx_cols = idx.cols }
     | None -> fail "DROP INDEX %s: index not tracked in schema state" (quote_id name))
  | `DropPrimaryKey ->
    (match pk_columns with
     | [] -> fail "DROP PRIMARY KEY: no primary key in schema state, cannot restore on revert"
     | cols -> `AddPrimaryKey cols)
  | `AddPrimaryKey _ -> `DropPrimaryKey
  | `AddConstraint (Some name) -> `DropConstraint name
  | `AddConstraint None -> fail "ADD CONSTRAINT without a name cannot be reverted"
  | `DropConstraint name ->
    fail "DROP CONSTRAINT %s: definition not tracked in schema state" (quote_id name)
  | `Default_or_convert_to _ ->
    (match Tables.get_charset table_name with
     | Some { charset = Some _ as cs; collation } ->
       `Default_or_convert_to (cs, Option.map loc collation)
     | _ ->
       fail "CONVERT TO CHARACTER SET: previous CHARACTER SET is not tracked \
             (no explicit charset in CREATE TABLE), revert cannot restore column encodings")
  | `TtlOptions _ -> `RemoveTtl (0, 0)
  | `RemoveTtl _ ->
    fail "REMOVE TTL: previous TTL parameters are not tracked in schema, \
          cannot restore on revert"
  | `None -> fail "no-op ALTER clause cannot be auto-reverted"

let action_to_sql_fragment ~default_sql_lookup (action : Sql.alter_action) = match action with
  | `None -> None
  | `Add (col, pos) ->
    Some (sprintf "ADD COLUMN %s%s"
            (alter_action_attr_to_sql ~default_sql_lookup col)
            (alter_pos_to_sql pos))
  | `Drop col_name ->
    Some (sprintf "DROP COLUMN %s" (quote_id col_name))
  | `Change (old_name, new_col, pos) ->
    Some (sprintf "CHANGE COLUMN %s %s%s"
            (quote_id old_name)
            (alter_action_attr_to_sql ~default_sql_lookup new_col)
            (alter_pos_to_sql pos))
  | `RenameTable new_name ->
    Some (sprintf "RENAME TO %s" (quote_table_name new_name))
  | `RenameColumn (old_name, new_name) ->
    Some (sprintf "RENAME COLUMN %s TO %s" (quote_id old_name) (quote_id new_name))
  | `RenameIndex (old_name, new_name) ->
    Some (sprintf "RENAME INDEX %s TO %s" (quote_id old_name) (quote_id new_name))
  | `AddIndex { add_idx_name; add_idx_kind; add_idx_cols } ->
    let kw = match add_idx_kind with
      | Sql.Plain_idx    -> "INDEX"
      | Sql.Unique_idx   -> "UNIQUE INDEX"
      | Sql.Fulltext_idx -> "FULLTEXT INDEX"
      | Sql.Spatial_idx  -> "SPATIAL INDEX"
    in
    let name_sql = Option.map_default (fun n -> " " ^ quote_id n) "" add_idx_name in
    Some (sprintf "ADD %s%s (%s)" kw name_sql
            (String.concat ", " (List.map quote_id add_idx_cols)))
  | `DropIndex name ->
    Some (sprintf "DROP INDEX %s" (quote_id name))
  | `AddPrimaryKey cols ->
    Some (sprintf "ADD PRIMARY KEY (%s)" (String.concat ", " (List.map quote_id cols)))
  | `DropPrimaryKey -> Some "DROP PRIMARY KEY"
  | `AddConstraint _ ->
    fail "ADD CONSTRAINT body is not preserved in AST, cannot be serialized"
  | `DropConstraint name ->
    Some (sprintf "DROP CONSTRAINT %s" (quote_id name))
  | `Default_or_convert_to (cs, collation) ->
    let charset_name = function
      | Sql.Named s -> s
      | Binary  -> "binary"
      | Ascii   -> "ascii"
      | Unicode -> "unicode"
    in
    let collate c = sprintf "COLLATE %s" c.Sql.value in
    let convert cs = sprintf "CONVERT TO CHARACTER SET %s" (charset_name cs) in
    (match cs, collation with
     | None,    None   -> None
     | None,    Some c -> Some (collate c)
     | Some cs, None   -> Some (convert cs)
     | Some cs, Some c -> Some (convert cs ^ " " ^ collate c))
  | `TtlOptions (opts, _) ->
    Some (opts
          |> List.map (function
            | `TtlSet (col, n, unit) ->
              sprintf "TTL = %s + INTERVAL %d %s" (quote_id col) n (String.uppercase_ascii unit)
            | `TtlEnable v -> sprintf "TTL_ENABLE = '%s'" v)
          |> String.concat " ")
  | `RemoveTtl _ -> Some "REMOVE TTL"

let alter_to_sql ~default_sql_lookup table_name actions =
  match List.filter_map (action_to_sql_fragment ~default_sql_lookup) actions with
  | [] -> None
  | fs -> Some (sprintf "ALTER TABLE %s %s"
                  (quote_table_name table_name) (String.concat ", " fs))

type migration = {
  props : Props.t;
  kind : Stmt.kind;
  apply : string;
  revert : string;
}

let inverse table_name (columns : Tables.column list) (actions : Sql.alter_action list) =
  let pk_columns = Tables.get_primary_key_columns columns in
  let pk_dropped = List.mem `DropPrimaryKey actions in
  try
    let inverse_actions, effective_name =
      List.fold_left (fun (revs, current_name) action ->
        let inv = inverse_action ~table_name ~current_name ~pk_columns ~pk_dropped action in
        let next_name = match action with `RenameTable n -> n | _ -> current_name in
        (inv :: revs, next_name))
        ([], table_name) actions
    in
    let default_sql_lookup column_name =
      Tables.column_find table_name ~column_name
      |> Option.map_default (fun (c : Tables.column) -> c.default_sql) None
    in
    match alter_to_sql ~default_sql_lookup effective_name inverse_actions with
    | Some sql -> Ok sql
    | None -> Error "ALTER has no actions to revert"
  with Migration_error msg ->
    Error (sprintf "table %s:\n  %s" (quote_table_name table_name) msg)

let drop_index_sql index_name table_name =
  sprintf "DROP INDEX %s ON %s" (quote_id index_name) (quote_table_name table_name)

let rename_inverse_sql = function
  | [] -> None
  | pairs ->
    Some (sprintf "RENAME TABLE %s"
      (pairs
       |> List.map (fun (old_name, new_name) ->
         sprintf "%s TO %s" (quote_table_name new_name) (quote_table_name old_name))
       |> String.concat ", "))
