open Printf
open Sqlgg

module SMap = Tables.SMap
module Name = Migration_id.Name

let mig_name naming head actions = Name.render naming (Name.make head actions)

let index_by key xs =
  List.fold_left (fun m x -> SMap.add (key x) x m) SMap.empty xs

let attr_of_column (c : Tables.column) =
  let attr =
    { c.attr with extra =
        Sql.Constraints.filter (fun cst -> not (Tables.is_pk_constraint cst)) c.attr.Sql.extra }
  in
  let base = Sql.Alter_action_attr.from_attr attr in
  Option.map_default
    (fun sk -> { base with kind = Some (Gen_migrations.loc sk) })
    base c.Tables.source_kind

let col_fragment c =
  Gen_migrations.alter_action_attr_to_sql
    ~default_sql_lookup:(fun _ -> c.Tables.default_sql) (attr_of_column c)

module Column_sig = struct
  type t = {
    name : string;
    kind : Sql.Source_type.kind option;
    collation : string option;
    constraints : Sql.Constraint.t list;
    has_default : bool;
    default_sql : string option;
  } [@@deriving eq]

  let of_column (c : Tables.column) =
    let open Sql.Alter_action_attr in
    let a = attr_of_column c in
    let kind = Option.map (fun (k : _ Sql.collated Sql.located) -> k.value.collated) a.kind in
    let collation =
      Option.map_default
        (fun (k : _ Sql.collated Sql.located) ->
          Option.map (fun (l : string Sql.located) -> l.value) k.value.collation)
        None a.kind
    in
    let constraints, has_default =
      List.fold_left
        (fun (cs, dflt) (e : constraint_ Sql.located) ->
          match e.value with
          | Default _ -> cs, true
          | Syntax_constraint cst when Gen_migrations.constraint_to_sql cst <> None -> cst :: cs, dflt
          | Syntax_constraint _ -> cs, dflt)
        ([], false) a.extra
    in
    { name = a.name;
      kind;
      collation;
      constraints = List.sort Sql.Constraint.compare constraints;
      has_default;
      default_sql = c.Tables.default_sql }

  let changed old new_ = not (equal (of_column old) (of_column new_))
end

let cols_by_name cols =
  index_by (fun (c : Tables.column) -> c.attr.Sql.name) cols

let table_by_name ts =
  index_by (fun t -> t.Tables.name.tn) ts

let unique_singleton_cols t =
  SMap.fold (fun _ idx acc ->
    match Tables.singleton_unique_col idx with Some c -> c :: acc | None -> acc)
    t.Tables.tbl_indexes []

let columns_without_index_unique t =
  Tables.remove_constraint_from_columns t.Tables.columns
    ~cols:(unique_singleton_cols t) Sql.Constraint.Unique

let materialize_inline_unique (t : Tables.stored_table) =
  let covered = unique_singleton_cols t in
  let is_inline_unique (c : Tables.column) =
    Sql.Constraints.mem Sql.Constraint.Unique c.attr.Sql.extra
    && not (List.mem c.attr.Sql.name covered)
  in
  t.Tables.columns
  |> List.to_seq
  |> Seq.filter is_inline_unique
  |> Seq.fold_left
       (fun t (c : Tables.column) ->
         let cols = [c.attr.Sql.name] in
         Tables.add_index_record t
           ~index_name:(Tables.synth_index_name t.Tables.tbl_indexes cols)
           ~kind:Sql.Unique_idx ~cols)
       t

type change =
  | Create_table of Tables.stored_table
  | Drop_table of Tables.stored_table
  | Alter_table of { table : Sql.table_name; sql : string; actions : Sql.alter_action list }

let index_kind_slug = function
  | Sql.Plain_idx -> "index"
  | Sql.Unique_idx -> "unique"
  | Sql.Fulltext_idx -> "fulltext"
  | Sql.Spatial_idx -> "spatial"

let verb spelling = Name.words spelling
let action v = Name.action (verb v) []
let action_on v target = Name.action (verb v) (Name.words target)
let rename kind old_ new_ =
  Name.action (verb "rename" @ verb kind) (Name.words old_ @ Name.words new_)

let action_of : Sql.alter_action -> Name.action = function
  | `Add (col, _) -> action_on "add_col" col.name
  | `Drop name -> action_on "drop_col" name
  | `Change (old_name, new_col, _) ->
    if old_name = new_col.name then action_on "change_col" new_col.name
    else rename "col" old_name new_col.name
  | `RenameTable t -> action_on "rename_to" t.tn
  | `RenameColumn (o, n) -> rename "col" o n
  | `RenameIndex (o, n) -> rename "index" o n
  | `AddIndex { add_idx_name; add_idx_kind; _ } ->
    let v = verb "add" @ verb (index_kind_slug add_idx_kind) in
    Option.map_default (fun n -> Name.action v (Name.words n)) (Name.action v []) add_idx_name
  | `DropIndex name -> action_on "drop_index" name
  | `AddPrimaryKey _ -> action "add_pk"
  | `DropPrimaryKey -> action "drop_pk"
  | `AddConstraint _ -> action "add_constraint"
  | `DropConstraint name -> action_on "drop_constraint" name
  | `Default_or_convert_to _ -> action "set_charset"
  | `TtlOptions _ -> action "set_ttl"
  | `RemoveTtl _ -> action "remove_ttl"
  | `AlterColumnPG (col, _) -> action_on "alter_col" col

let diff_columns ~from_ ~to_ =
  let from_cols = columns_without_index_unique from_ in
  let to_cols = columns_without_index_unique to_ in
  let a = cols_by_name from_cols in
  let b = cols_by_name to_cols in
  let adds =
    to_cols |> List.filter (fun (c : Tables.column) -> not (SMap.mem c.attr.Sql.name a))
    |> List.map (fun c -> `Add (attr_of_column c, `Default))
  in
  let drops =
    from_cols |> List.filter (fun (c : Tables.column) -> not (SMap.mem c.attr.Sql.name b))
    |> List.map (fun (c : Tables.column) -> `Drop c.attr.Sql.name)
  in
  let changes =
    to_cols |> List.filter_map (fun (c : Tables.column) ->
      match SMap.find_opt c.attr.Sql.name a with
      | Some old when Column_sig.changed old c ->
        Some (`Change (c.attr.Sql.name, attr_of_column c, `Default))
      | _ -> None)
  in
  drops @ adds @ changes

let add_index name (idx : Tables.stored_index) =
  `AddIndex { Sql.add_idx_name = Some name; add_idx_kind = idx.kind; add_idx_cols = idx.cols }

let diff_indexes ~from_ ~to_ =
  let a = from_.Tables.tbl_indexes and b = to_.Tables.tbl_indexes in
  let same (x : Tables.stored_index) (y : Tables.stored_index) =
    Sql.equal_index_op_kind x.kind y.kind && x.cols = y.cols in
  let drops = SMap.fold (fun name _ acc ->
    if SMap.mem name b then acc else `DropIndex name :: acc) a [] in
  let adds = SMap.fold (fun name idx acc ->
    match SMap.find_opt name a with
    | Some old when same old idx -> acc
    | Some _ -> `DropIndex name :: add_index name idx :: acc
    | None -> add_index name idx :: acc) b [] in
  drops @ adds

let diff_pk ~from_ ~to_ =
  let pa = Tables.get_primary_key_columns from_.Tables.columns in
  let pb = Tables.get_primary_key_columns to_.Tables.columns in
  if pa = pb then []
  else (if pa = [] then [] else [`DropPrimaryKey])
       @ (if pb = [] then [] else [`AddPrimaryKey pb])

let diff_property get render ~from_ ~to_ =
  if get from_ = get to_ then [] else render (get to_)

let diff_charset =
  diff_property (fun t -> t.Tables.tbl_charset)
    (Option.map_default
      (fun ({ charset; collation } : Tables.table_charset) ->
        [`Default_or_convert_to (charset, Option.map Gen_migrations.loc collation)])
      [])

let ttl_options_of (t : Tables.table_ttl) =
  [ `TtlSet (t.ttl_col, t.ttl_n, t.ttl_unit);
    `TtlEnable (if t.ttl_enabled then "ON" else "OFF") ]

let diff_ttl =
  diff_property (fun t -> t.Tables.tbl_ttl)
    (Option.map_default (fun t -> [`TtlOptions (ttl_options_of t, (0, 0))])
      [`RemoveTtl (0, 0)])

let diff_table ~from_ ~to_ =
  diff_columns ~from_ ~to_ @ diff_pk ~from_ ~to_
  @ diff_indexes ~from_ ~to_ @ diff_charset ~from_ ~to_ @ diff_ttl ~from_ ~to_

let alter_change name target actions =
  let default_sql_lookup col_name =
    Stdlib.Option.bind (Tables.find_column ~name:col_name target.Tables.columns)
      (fun (c : Tables.column) -> c.default_sql)
  in
  Option.map
    (fun sql -> Alter_table { table = name; sql; actions })
    (Gen_migrations.alter_table_sql ~default_sql_lookup name (Gen_migrations.Columns actions))

let diff ~ddl_as_migration ~from_ ~to_ ~by_from ~by_to =
  let creates =
    if not ddl_as_migration then []
    else
      to_ |> List.filter (fun (t : Tables.stored_table) -> not (SMap.mem t.name.tn by_from))
          |> List.map (fun t -> Create_table t) in
  let drops =
    from_ |> List.filter (fun (t : Tables.stored_table) -> not (SMap.mem t.name.tn by_to))
         |> List.map (fun t -> Drop_table t) in
  let alters =
    to_ |> List.filter_map (fun (t : Tables.stored_table) ->
      Stdlib.Option.bind (SMap.find_opt t.name.tn by_from)
        (fun old -> alter_change t.name t (diff_table ~from_:old ~to_:t))) in
  drops @ creates @ alters

let create_table_of t =
  let ddl_column (c : Tables.column) =
    { Gen_migrations.col = attr_of_column c; col_default_sql = c.default_sql }
  in
  let ddl_index (name, (i : Tables.stored_index)) =
    { Gen_migrations.ix_name = name; ix_kind = i.kind; ix_cols = i.cols }
  in
  { Gen_migrations.table = t.Tables.name;
    columns = List.map ddl_column (columns_without_index_unique t);
    primary_key = Tables.get_primary_key_columns t.Tables.columns;
    indexes = List.map ddl_index (SMap.bindings t.Tables.tbl_indexes);
    charset = t.Tables.tbl_charset |> Option.map (fun ({ charset; collation } : Tables.table_charset) ->
      { Gen_migrations.ch_name = charset; ch_collation = collation });
    ttl = Option.map ttl_options_of t.Tables.tbl_ttl }

let render_apply c =
  match c with
  | Drop_table t -> [Gen_migrations.drop_table_sql t.Tables.name]
  | Create_table t -> Gen_migrations.create_table_migration (create_table_of t)
  | Alter_table { sql; _ } -> [sql]

let change_table = function
  | Create_table t -> t.Tables.name
  | Drop_table t -> t.Tables.name
  | Alter_table { table; _ } -> table

let change_name ~naming = function
  | Create_table t -> mig_name naming (Name.words "create" @ Name.words t.Tables.name.tn) []
  | Drop_table t -> mig_name naming (Name.words "drop" @ Name.words t.Tables.name.tn) []
  | Alter_table { table; actions; _ } ->
    mig_name naming (Name.words "alter" @ Name.words table.tn) (List.map action_of actions)

let kind_of_change = function
  | Create_table t -> Stmt.Create t.Tables.name
  | Drop_table t -> Stmt.Drop t.Tables.name
  | Alter_table { table; _ } -> Stmt.Alter [table]

let invert ~by_from ~by_to up =
  let irreversible reason =
    Gen_migrations.fail
      "table %s: this change cannot be auto-reverted (%s); \
       write this migration's down by hand"
      (Gen_migrations.quote_table_name (change_table up)) reason
  in
  match up with
  | Create_table t -> Drop_table t
  | Drop_table t -> Create_table t
  | Alter_table { table = name; _ } ->
    match SMap.find_opt name.Sql.tn by_from, SMap.find_opt name.Sql.tn by_to with
    | None, _ | _, None ->
      irreversible "table is missing from the baseline or target snapshot"
    | Some f, Some t ->
      if f.Tables.tbl_charset = None && t.Tables.tbl_charset <> None then
        irreversible "a DEFAULT CHARSET / COLLATE was added while the baseline has \
                      no explicit charset to restore"
      else
        match alter_change name f (diff_table ~from_:t ~to_:f) with
        | Some down -> down
        | None -> irreversible "reverse diff renders to nothing"

let generate ~naming ~ddl_as_migration ~from_ ~to_ =
  let from_ = List.map materialize_inline_unique from_ in
  let to_ = List.map materialize_inline_unique to_ in
  let by_from = table_by_name from_ in
  let by_to = table_by_name to_ in
  diff ~ddl_as_migration ~from_ ~to_ ~by_from ~by_to |> List.map (fun up ->
    { Gen_migrations.props = Props.set Props.empty "name" (change_name ~naming up);
      kind = kind_of_change up;
      apply = render_apply up;
      revert = render_apply (invert ~by_from ~by_to up) })

let canonical ts =
  let index_sig (name, (i : Tables.stored_index)) =
    sprintf "%s:%s:%s" name (Sql.show_index_op_kind i.kind) (String.concat "," i.cols)
  in
  let charset_sig =
    Option.map_default
      (fun ({ charset; collation } : Tables.table_charset) ->
        sprintf "%s/%s" (Sql.show_charset_name charset) (Option.default "" collation))
      ""
  in
  let ttl_sig =
    Option.map_default
      (fun ({ ttl_col; ttl_n; ttl_unit; ttl_enabled } : Tables.table_ttl) ->
        sprintf "%s+%d %s/%s" ttl_col ttl_n ttl_unit (if ttl_enabled then "on" else "off"))
      ""
  in
  let table_sig (t : Tables.stored_table) =
    let cols = List.sort compare (List.map col_fragment t.columns) in
    let idx = SMap.bindings t.tbl_indexes |> List.map index_sig |> List.sort compare in
    let fields =
      [ "pk", String.concat "," (Tables.get_primary_key_columns t.columns);
        "idx", String.concat ";" idx;
        "cs", charset_sig t.tbl_charset;
        "ttl", ttl_sig t.tbl_ttl ]
      |> List.map (fun (k, v) -> sprintf "%s:%s" k v)
      |> String.concat " "
    in
    sprintf "%s(%s) %s" t.name.tn (String.concat "," cols) fields
  in
  ts
  |> List.sort (fun (a : Tables.stored_table) b -> compare a.name.tn b.name.tn)
  |> List.map table_sig
  |> String.concat "\n"

exception Verification_failed of { reason : string; got : string; expected : string }

let () = Printexc.register_printer @@ function
  | Verification_failed { reason; got; expected } ->
    Some (sprintf "%s\n\nthe schema we got:\n%s\n\nthe schema we expected:\n%s" reason got expected)
  | _ -> None

let replay_migrations ~replay ~from_ ~to_ migs =
  let from_ = List.map materialize_inline_unique from_ in
  let to_ = List.map materialize_inline_unique to_ in
  let rebuild baseline steps =
    Tables.restore baseline;
    List.iter replay steps;
    canonical (Tables.snapshot ())
  in
  let target = canonical to_ in
  let baseline = canonical from_ in
  let applied  = rebuild from_ (List.concat_map (fun m -> m.Gen_migrations.apply) migs) in
  let reverted = rebuild to_   (List.concat_map (fun m -> m.Gen_migrations.revert) (List.rev migs)) in
  if applied <> target then
    raise (Verification_failed
      { reason = "applying the migrations did not rebuild the target schema";
        got = applied; expected = target });
  if reverted <> baseline then
    raise (Verification_failed
      { reason = "reverting the migrations did not restore the original schema";
        got = reverted; expected = baseline })

let table_ddl t =
  Gen_migrations.create_table_schema (create_table_of t)

let dump state =
  state
  |> List.rev
  |> List.concat_map table_ddl
  |> List.map (fun s -> s ^ ";")
  |> String.concat "\n"

let verify_ddl ~replay state ddl =
  let expected = canonical state in
  Tables.reset ();
  String.split_on_char ';' ddl
  |> List.iter (fun s -> if String.trim s <> "" then replay s);
  let got = canonical (Tables.snapshot ()) in
  Tables.restore state;
  if got <> expected then
    raise (Verification_failed
      { reason = "the generated DDL did not parse back into the same schema"; got; expected })
