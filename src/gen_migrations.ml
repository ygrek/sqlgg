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

let charset_name = function
  | Sql.Named s -> s | Sql.Binary -> "binary" | Sql.Ascii -> "ascii" | Sql.Unicode -> "unicode"

let quote_cols cols = String.concat ", " (List.map quote_id cols)

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
      | Default _ ->
        match default_sql_lookup col.name with
         | Some sql -> Some (sprintf "DEFAULT %s" sql)
         | None ->
           fail "column `%s` has a DEFAULT whose source SQL was not captured; \
                 cannot serialize it for migration (write this migration by hand)"
             col.name)
  in
  let extras_sql = match extras with [] -> "" | l -> " " ^ String.concat " " l in
  sprintf "%s%s%s" name kind_sql extras_sql

let alter_pos_to_sql = function
  | `Default -> ""
  | `First -> " FIRST"
  | `After col -> sprintf " AFTER %s" (quote_id col)

let action_to_sql_fragment ~default_sql_lookup (action : Sql.alter_action) = match action with
  | `Add (col, pos) ->
    sprintf "ADD COLUMN %s%s"
      (alter_action_attr_to_sql ~default_sql_lookup col)
      (alter_pos_to_sql pos)
  | `Drop col_name ->
    sprintf "DROP COLUMN %s" (quote_id col_name)
  | `Change (old_name, new_col, pos) ->
    sprintf "CHANGE COLUMN %s %s%s"
      (quote_id old_name)
      (alter_action_attr_to_sql ~default_sql_lookup new_col)
      (alter_pos_to_sql pos)
  | `RenameTable new_name ->
    sprintf "RENAME TO %s" (quote_table_name new_name)
  | `RenameColumn (old_name, new_name) ->
    sprintf "RENAME COLUMN %s TO %s" (quote_id old_name) (quote_id new_name)
  | `RenameIndex (old_name, new_name) ->
    sprintf "RENAME INDEX %s TO %s" (quote_id old_name) (quote_id new_name)
  | `AddIndex { add_idx_name; add_idx_kind; add_idx_cols } ->
    let kw = match add_idx_kind with
      | Sql.Plain_idx    -> "INDEX"
      | Sql.Unique_idx   -> "UNIQUE INDEX"
      | Sql.Fulltext_idx -> "FULLTEXT INDEX"
      | Sql.Spatial_idx  -> "SPATIAL INDEX"
    in
    let name_sql = Option.map_default (fun n -> " " ^ quote_id n) "" add_idx_name in
    sprintf "ADD %s%s (%s)" kw name_sql (quote_cols add_idx_cols)
  | `DropIndex name ->
    sprintf "DROP INDEX %s" (quote_id name)
  | `AddPrimaryKey cols ->
    sprintf "ADD PRIMARY KEY (%s)" (quote_cols cols)
  | `DropPrimaryKey -> "DROP PRIMARY KEY"
  | `AddConstraint _ ->
    fail "ADD CONSTRAINT body is not preserved in AST, cannot be serialized"
  | `DropConstraint name ->
    sprintf "DROP CONSTRAINT %s" (quote_id name)
  | `Default_or_convert_to (cs, collation) ->
    let convert = sprintf "CONVERT TO CHARACTER SET %s" (charset_name cs) in
    Option.map_default (fun c -> convert ^ sprintf " COLLATE %s" c.Sql.value) convert collation
  | `TtlOptions (opts, _) ->
    opts
    |> List.map (function
      | `TtlSet (col, n, unit) ->
        sprintf "TTL = %s + INTERVAL %d %s" (quote_id col) n (String.uppercase_ascii unit)
      | `TtlEnable v -> sprintf "TTL_ENABLE = '%s'" v)
    |> String.concat " "
  | `RemoveTtl _ -> "REMOVE TTL"
  | `AlterColumnPG (col_name, change) ->
    sprintf "ALTER COLUMN %s %s" (quote_id col_name)
      (match change.Sql.value with
      | Sql.Alter_column_pg.Set_type t -> "TYPE " ^ source_type_kind_to_sql t.Sql.value.Sql.collated
      | Set_not_null -> "SET NOT NULL"
      | Drop_not_null -> "DROP NOT NULL"
      | Set_default ->
        (match default_sql_lookup col_name with
         | Some sql -> "SET DEFAULT " ^ sql
         | None ->
           fail "column `%s` has a DEFAULT whose source SQL was not captured; \
                 cannot serialize it for migration (write this migration by hand)"
             col_name)
      | Drop_default -> "DROP DEFAULT")

type ddl_column = { col : Sql.Alter_action_attr.t; col_default_sql : string option }

type ddl_index = { ix_name : string; ix_kind : Sql.index_op_kind; ix_cols : string list }

type ddl_charset = { ch_name : Sql.charset_name; ch_collation : string option }

type create_table = {
  table : Sql.table_name;
  columns : ddl_column list;
  primary_key : string list;
  indexes : ddl_index list;
  charset : ddl_charset option;
  ttl : Sql.ttl_option list option;
}

let column_to_sql c =
  alter_action_attr_to_sql ~default_sql_lookup:(fun _ -> c.col_default_sql) c.col

let primary_key_clause = function
  | [] -> []
  | cols -> [ sprintf "PRIMARY KEY (%s)" (quote_cols cols) ]

let columns_body ?(extra=[]) ct =
  String.concat ", "
    (List.map column_to_sql ct.columns @ primary_key_clause ct.primary_key @ extra)

let index_add_action ix =
  `AddIndex { Sql.add_idx_name = Some ix.ix_name; add_idx_kind = ix.ix_kind; add_idx_cols = ix.ix_cols }

let index_cols_sql ix = quote_cols ix.ix_cols

let inline_index_clause ix =
  match ix.ix_kind with
  | Sql.Unique_idx -> Some (sprintf "UNIQUE KEY %s (%s)" (quote_id ix.ix_name) (index_cols_sql ix))
  | Sql.Plain_idx -> Some (sprintf "KEY %s (%s)" (quote_id ix.ix_name) (index_cols_sql ix))
  | Sql.Fulltext_idx | Sql.Spatial_idx -> None

type alter_clause =
  | Columns of Sql.alter_action list
  | Add_key of ddl_index
  | Default_charset of ddl_charset
  | Ttl_options of Sql.ttl_option list

let alter_clause_body ~default_sql_lookup = function
  | Columns actions ->
    (match List.map (action_to_sql_fragment ~default_sql_lookup) actions with
     | [] -> None
     | fs -> Some (String.concat ", " fs))
  | Add_key ix ->
    let key kw = sprintf "%s %s (%s)" kw (quote_id ix.ix_name) (index_cols_sql ix) in
    (match ix.ix_kind with
     | Sql.Fulltext_idx -> Some (key "ADD FULLTEXT KEY")
     | Sql.Spatial_idx -> Some (key "ADD SPATIAL KEY")
     | Sql.Unique_idx | Sql.Plain_idx -> None)
  | Default_charset { ch_name; ch_collation } ->
    let coll = Option.map_default (fun c -> " COLLATE " ^ c) "" ch_collation in
    Some (sprintf "DEFAULT CHARSET %s%s" (charset_name ch_name) coll)
  | Ttl_options opts ->
    Some (action_to_sql_fragment ~default_sql_lookup (`TtlOptions (opts, (0, 0))))

let alter_table_sql ~default_sql_lookup table clause =
  Option.map (sprintf "ALTER TABLE %s %s" (quote_table_name table))
    (alter_clause_body ~default_sql_lookup clause)

let drop_table_sql name =
  sprintf "DROP TABLE %s" (quote_table_name name)

let create_line ?extra ct =
  sprintf "CREATE TABLE %s (%s)" (quote_table_name ct.table) (columns_body ?extra ct)

let no_default _ = None

let opt_clause make = Option.map_default (fun x -> [make x]) []

let create_with_clauses ?extra ct clauses =
  create_line ?extra ct
  :: List.filter_map (alter_table_sql ~default_sql_lookup:no_default ct.table) clauses

let ttl_clause ct = opt_clause (fun t -> Ttl_options t) ct.ttl

let create_table_migration ct =
  create_with_clauses ct
    (Columns (List.map index_add_action ct.indexes) :: ttl_clause ct)

let create_table_schema ct =
  create_with_clauses ~extra:(List.filter_map inline_index_clause ct.indexes) ct
    (opt_clause (fun c -> Default_charset c) ct.charset
     @ ttl_clause ct
     @ List.map (fun ix -> Add_key ix) ct.indexes)

type migration = {
  props : Props.t;
  kind : Stmt.kind;
  apply : string list;
  revert : string list;
}
