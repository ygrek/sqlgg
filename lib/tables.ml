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

type table_charset = { charset : Sql.charset_name option; collation : string option }

module SMap = Map.Make(String)

type stored_index = { kind : Sql.index_op_kind; cols : string list }

type stored_table = {
  name : Sql.table_name;
  columns : column list;
  tbl_charset : table_charset option;
  tbl_indexes : stored_index SMap.t;
}

let store : stored_table list ref = ref []

(** FIXME table names case sensitivity? *)
(* NB compares with db name.  *)
let by_name (name:Sql.table_name) ((n, _) : Sql.table) = n = name

let columns_to_schema cols = List.map (fun c -> c.attr) cols

let column_of_attr attr = { attr; source_kind = None; default_sql = None }

(** @raise Failure when no such table is present *)
let get_from (tables : Sql.table list) name =
  try
    List.find (by_name name) tables
  with Not_found -> failwith (sprintf "no such table %s" (Sql.show_table_name name))

let find_stored name = List.find_opt (fun t -> t.name = name) !store

let get_stored name = match find_stored name with
  | Some t -> t
  | None -> failwith (sprintf "no such table %s" (Sql.show_table_name name))

let get name =
  let t = get_stored name in
  (t.name, columns_to_schema t.columns)

let get_schema name = snd (get name)

let get_columns name = (get_stored name).columns

let check name = ignore (get_stored name)

let add_columns (name, cols) =
  match find_stored name with
  | None -> store := { name; columns = cols; tbl_charset = None; tbl_indexes = SMap.empty } :: !store
  | Some _ -> failwith (sprintf "table %s already exists" (Sql.show_table_name name))

let add (name, schema) =
  add_columns (name, List.map column_of_attr schema)

let alter name f =
  check name;
  let alter_stored t = if t.name = name then f t else t in
  store := List.map alter_stored !store

let alter_columns name f = alter name (fun t -> { t with columns = f t.columns })

let get_charset name = Option.map_default (fun t -> t.tbl_charset) None (find_stored name)

let set_charset name cs = alter name (fun t -> { t with tbl_charset = Some cs })

let index_find name ~index_name =
  Option.map_default (fun t -> SMap.find_opt index_name t.tbl_indexes) None (find_stored name)

let column_find name ~column_name =
  Option.map_default
    (fun t -> List.find_opt (fun c -> c.attr.Sql.name = column_name) t.columns)
    None (find_stored name)

let index_add name ~index_name ~kind ~cols =
  alter name (fun t ->
    { t with tbl_indexes = SMap.add index_name { kind; cols } t.tbl_indexes })

let add_inline_indexes name ~indexes ~constraints =
  List.iter (fun (idx : Sql.table_inline_index Sql.located) ->
    match idx.value with
    | { idx_kind = Sql.Regular_idx; idx_name = Some index_name; idx_cols = cols; idx_unique } ->
      let kind = if idx_unique then Sql.Unique_idx else Sql.Plain_idx in
      index_add name ~index_name ~kind ~cols
    | _ -> ()
  ) indexes;
  List.iter (function
    | `Unique (Some index_name, (_ :: _ as cols)) ->
      index_add name ~index_name ~kind:Sql.Unique_idx ~cols
    | _ -> ()
  ) constraints

let index_drop name ~index_name =
  alter name (fun t -> { t with tbl_indexes = SMap.remove index_name t.tbl_indexes })

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
  | Some _ -> failwith (sprintf "table %s already exists" (Sql.show_table_name newname))
  | None -> alter oldname (fun t -> { t with name = newname })

let schema_to_columns ~cols ~new_col new_schema =
  List.map (fun attr ->
    if attr.Sql.name = new_col.attr.Sql.name then new_col
    else
      List.find_opt (fun c -> c.attr.Sql.name = attr.Sql.name) cols
      |> Option.map_default (fun existing -> { existing with attr }) (column_of_attr attr)
  ) new_schema

let alter_add name ~col ~pos =
  alter_columns name (fun cols ->
    let new_schema = Sql.Schema.add (columns_to_schema cols) col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let alter_drop name ~col:col_name =
  alter_columns name (fun cols ->
    List.filter (fun c -> c.attr.Sql.name <> col_name) cols)

let alter_change name ~oldcol ~col ~pos =
  alter_columns name (fun cols ->
    let new_schema = Sql.Schema.change (columns_to_schema cols) oldcol col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let rename_column name ~old_name ~new_name =
  alter_columns name (fun cols ->
    List.map (fun c ->
      if c.attr.Sql.name = old_name then { c with attr = { c.attr with name = new_name } }
      else c
    ) cols)

let is_pk_constraint = function
  | Sql.Constraint.PrimaryKey | Sql.Constraint.Composite (CompositePrimary _) -> true
  | _ -> false

let get_primary_key_columns =
  List.map (fun c -> c.attr.Sql.name) $ List.filter (fun c -> Sql.Constraints.exists is_pk_constraint c.attr.Sql.extra) 

let drop_primary_key name =
  alter_columns name (List.map (fun c ->
    { c with attr = { c.attr with
      extra = Sql.Constraints.filter (Fun.negate is_pk_constraint) c.attr.Sql.extra } }))

let add_primary_key name ~cols:col_names =
  let pk = match col_names with
    | [_] -> Sql.Constraint.PrimaryKey
    | _ -> Sql.Constraint.make_composite_primary col_names
  in
  alter_columns name (List.map (fun c ->
    if List.mem c.attr.Sql.name col_names then
      { c with attr = { c.attr with extra = Sql.Constraints.add pk c.attr.Sql.extra } }
    else c))

let print ch tables = let out = IO.output_channel ch in List.iter (Sql.print_table out) tables; IO.flush out
let print_all () = print stdout (List.map (fun t -> (t.name, columns_to_schema t.columns)) !store)
let print1 name = print stdout [get @@ Sql.make_table_name name]

let reset () = store := []

let all () = List.map (fun t -> (t.name, columns_to_schema t.columns)) !store
