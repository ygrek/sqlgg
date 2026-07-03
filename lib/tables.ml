(** Global list of tables *)

open Printf
open ExtLib
open Prelude

type column = {
  attr : Sql.attr;
  source_kind : Sql.Source_type.kind Sql.collated option;
  default_sql : string option;
}

type table = Sql.table

type table_charset = { charset : Sql.charset_name; collation : string option }

type table_ttl = { ttl_col : string; ttl_n : int; ttl_unit : string; ttl_enabled : bool }

module SMap = Map.Make(String)

type stored_index = { kind : Sql.index_op_kind; cols : string list }

type stored_table = {
  name : Sql.table_name;
  columns : column list;
  tbl_charset : table_charset option;
  tbl_ttl : table_ttl option;
  tbl_indexes : stored_index SMap.t;
}

let store : stored_table list ref = ref []

(** FIXME table names case sensitivity? *)
let by_name (name:Sql.table_name) ((n, _) : Sql.table) = n = name

let col_name c = c.attr.Sql.name

let map_attr f c = { c with attr = f c.attr }

let map_extra f c = map_attr (fun a -> { a with Sql.extra = f a.Sql.extra }) c

let find_column ~name cols = List.find_opt (fun c -> col_name c = name) cols

let columns_to_schema cols = List.map (fun c -> c.attr) cols

let column_of_attr attr = { attr; source_kind = None; default_sql = None }

let no_such_table name = failwith (sprintf "no such table %s" (Sql.show_table_name name))
let table_exists name = failwith (sprintf "table %s already exists" (Sql.show_table_name name))

let to_table t = (t.name, columns_to_schema t.columns)

let get_from (tables : Sql.table list) name =
  try List.find (by_name name) tables
  with Not_found -> no_such_table name

let find_stored name = List.find_opt (fun t -> t.name = name) !store

let with_stored name default f = Option.map_default f default (find_stored name)

let get_stored name = match find_stored name with
  | Some t -> t
  | None -> no_such_table name

let get name = to_table (get_stored name)

let get_schema name = snd (get name)

let get_columns name = (get_stored name).columns

let check name = ignore (get_stored name)

let add_columns (name, cols) =
  match find_stored name with
  | None -> store := { name; columns = cols; tbl_charset = None; tbl_ttl = None; tbl_indexes = SMap.empty } :: !store
  | Some _ -> table_exists name

let add (name, schema) =
  add_columns (name, List.map column_of_attr schema)

let alter name f =
  check name;
  let alter_stored t = if t.name = name then f t else t in
  store := List.map alter_stored !store

let alter_columns name f = alter name (fun t -> { t with columns = f t.columns })

let get_charset name = with_stored name None (fun t -> t.tbl_charset)

let set_charset name cs = alter name (fun t -> { t with tbl_charset = Some cs })

let get_ttl name = with_stored name None (fun t -> t.tbl_ttl)

let set_ttl name ttl = alter name (fun t -> { t with tbl_ttl = ttl })

let index_find name ~index_name = with_stored name None (fun t -> SMap.find_opt index_name t.tbl_indexes)

let column_find name ~column_name = with_stored name None (fun t -> find_column ~name:column_name t.columns)

let add_constraint_to_columns columns ~cols constr =
  List.map (fun c ->
    if List.mem (col_name c) cols then map_extra (Sql.Constraints.add constr) c else c) columns

let remove_constraint_from_columns columns ~cols constr =
  List.map (fun c ->
    if List.mem (col_name c) cols then map_extra (Sql.Constraints.remove constr) c else c) columns

let grouped_constraint ~single ~composite = function
  | [ _ ] -> single
  | cols -> composite cols

let mark_unique_columns columns ~cols =
  add_constraint_to_columns columns ~cols
    (grouped_constraint cols ~single:Sql.Constraint.Unique ~composite:Sql.Constraint.make_composite_unique)

let add_index_record t ~index_name ~kind ~cols =
  let columns = match kind with
    | Sql.Unique_idx -> mark_unique_columns t.columns ~cols
    | _ -> t.columns in
  { t with columns; tbl_indexes = SMap.add index_name { kind; cols } t.tbl_indexes }

let synth_index_name existing cols =
  let base = match cols with c :: _ -> c | [] -> "index" in
  if not (SMap.mem base existing) then base
  else
    let rec loop n =
      let candidate = sprintf "%s_%d" base n in
      if SMap.mem candidate existing then loop (n + 1) else candidate
    in
    loop 2

let index_add name ~index_name ~kind ~cols =
  alter name (fun t -> add_index_record t ~index_name ~kind ~cols)

let index_add_auto name ~kind ~cols =
  alter name (fun t ->
    add_index_record t ~index_name:(synth_index_name t.tbl_indexes cols) ~kind ~cols)

let add_inline_indexes name ~indexes ~constraints =
  List.iter (fun (idx : Sql.table_inline_index Sql.located) ->
    let { Sql.idx_name; idx_kind; idx_cols = cols; idx_unique } = idx.value in
    let kind = match idx_kind with
      | Sql.Regular_idx -> if idx_unique then Sql.Unique_idx else Sql.Plain_idx
      | Sql.Fulltext -> Sql.Fulltext_idx
      | Sql.Spatial -> Sql.Spatial_idx
    in
    match idx_name with
    | Some index_name -> index_add name ~index_name ~kind ~cols
    | None -> index_add_auto name ~kind ~cols
  ) indexes;
  List.iter (function
    | `Unique (Some index_name, (_ :: _ as cols)) ->
      index_add name ~index_name ~kind:Sql.Unique_idx ~cols
    | `Unique (None, (_ :: _ as cols)) ->
      index_add_auto name ~kind:Sql.Unique_idx ~cols
    | _ -> ()
  ) constraints

let singleton_unique_col (i : stored_index) =
  match i.kind, i.cols with Sql.Unique_idx, [ col ] -> Some col | _ -> None

let index_drop name ~index_name =
  alter name (fun t ->
    match SMap.find_opt index_name t.tbl_indexes with
    | None -> t
    | Some idx ->
      let tbl_indexes = SMap.remove index_name t.tbl_indexes in
      let columns =
        match singleton_unique_col idx with
        | Some col when not (SMap.exists (fun _ i -> singleton_unique_col i = Some col) tbl_indexes) ->
          remove_constraint_from_columns t.columns ~cols:[col] Sql.Constraint.Unique
        | _ -> t.columns
      in
      { t with tbl_indexes; columns })

let index_rename name ~old_name ~new_name =
  alter name (fun t ->
    t.tbl_indexes
    |> SMap.find_opt old_name
    |> Option.map_default (fun idx ->
      let m = SMap.remove old_name t.tbl_indexes in
      { t with tbl_indexes = SMap.add new_name idx m }) t)

let drop name =
  check name;
  store := List.filter (fun t -> t.name <> name) !store

let rename oldname newname =
  match find_stored newname with
  | Some _ -> table_exists newname
  | None -> alter oldname (fun t -> { t with name = newname })

let schema_to_columns ~cols ~new_col new_schema =
  List.map (fun attr ->
    if attr.Sql.name = col_name new_col then new_col
    else
      find_column ~name:attr.Sql.name cols
      |> Option.map_default (fun existing -> { existing with attr }) (column_of_attr attr)
  ) new_schema

let is_pk_constraint = function
  | Sql.Constraint.PrimaryKey | Sql.Constraint.Composite (CompositePrimary _) -> true
  | _ -> false

let has_pk_constraint c = Sql.Constraints.exists is_pk_constraint c.attr.Sql.extra

let alter_add name ~col ~pos =
  alter_columns name (fun cols ->
    let new_schema = Sql.Schema.add (columns_to_schema cols) col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let alter_drop name ~col =
  alter_columns name (List.filter (fun c -> col_name c <> col))

let alter_change name ~oldcol ~col ~pos =
  alter_columns name (fun cols ->
    let col =
      match find_column ~name:oldcol cols with
      | Some old when has_pk_constraint old && not (has_pk_constraint col) ->
        let pk = Sql.Constraints.filter is_pk_constraint old.attr.Sql.extra in
        map_extra (Sql.Constraints.union pk) col
      | _ -> col
    in
    let new_schema = Sql.Schema.change (columns_to_schema cols) oldcol col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let rename_column name ~old_name ~new_name =
  alter_columns name (List.map (fun c ->
    if col_name c = old_name then map_attr (fun a -> { a with Sql.name = new_name }) c else c))

let get_primary_key_columns = List.map col_name $ List.filter has_pk_constraint

let drop_primary_key name =
  alter_columns name (List.map (map_extra (Sql.Constraints.filter (Fun.negate is_pk_constraint))))

let add_primary_key name ~cols =
  let pk = grouped_constraint cols ~single:Sql.Constraint.PrimaryKey ~composite:Sql.Constraint.make_composite_primary in
  alter_columns name (fun columns -> add_constraint_to_columns columns ~cols pk)

let alter_column_pg name ~col_name (change : Sql.Alter_column_pg.t) =
  let set_not_null nn =
    let update = if nn then Sql.Constraints.add else Sql.Constraints.remove in
    map_attr (fun a -> { a with domain = if nn then Sql.Type.make_strict a.domain else Sql.Type.make_nullable a.domain })
    $ map_extra (update NotNull)
  in
  let f = match change with
    | Set_type { Sql.value = kind; _ } ->
      fun c ->
        { c with source_kind = Some kind;
          attr = { c.attr with domain =
            { c.attr.domain with Sql.Type.t = Sql.Source_type.kind_to_type_kind kind.Sql.collated } } }
    | Set_not_null -> set_not_null true
    | Drop_not_null -> set_not_null false
    | Set_default -> map_extra (Sql.Constraints.add WithDefault)
    | Drop_default -> map_extra (Sql.Constraints.remove WithDefault)
  in
  alter_columns name (List.map (fun c -> if c.attr.Sql.name = col_name then f c else c))

let print ch tables = let out = IO.output_channel ch in List.iter (Sql.print_table out) tables; IO.flush out

let all () = List.map to_table !store
let print_all () = print stdout (all ())
let print1 name = print stdout [get @@ Sql.make_table_name name]

let reset () = store := []

let snapshot () : stored_table list = !store
let restore (s : stored_table list) = store := s
