(** Global list of tables *)

open Printf
open ExtLib
open Prelude

type column = { attr : Sql.attr; source_kind : Sql.Source_type.kind option }

type table = Sql.table

type table_charset = { charset : Sql.charset_name option; collation : string option }

type stored_table = Sql.table_name * column list

let store : stored_table list ref = ref []
let charsets : (Sql.table_name, table_charset) Hashtbl.t = Hashtbl.create 16

(** FIXME table names case sensitivity? *)
(* NB compares with db name *)
let by_name (name:Sql.table_name) = fun (n,_) -> n = name

let columns_to_schema cols = List.map (fun c -> c.attr) cols

let column_of_attr attr = { attr; source_kind = None }

(** @raise Error when no such table *)
let get_from tables name =
  try
    List.find (by_name name) tables
  with Not_found -> failwith (sprintf "no such table %s" (Sql.show_table_name name))

let get_stored name = get_from !store name

let get name =
  let (n, cols) = get_stored name in
  (n, columns_to_schema cols)

let get_schema name = snd (get name)

let get_columns name = snd (get_stored name)

let check name = ignore (get_stored name)

let add_columns (name, cols) =
  match List.find_all (by_name name) !store with
  | [] -> store := (name, cols) :: !store
  | _ -> failwith (sprintf "table %s already exists" (Sql.show_table_name name))

let add (name, schema) =
  add_columns (name, List.map column_of_attr schema)

let get_charset name =
  Hashtbl.find_opt charsets name

let set_charset name cs =
  Hashtbl.replace charsets name cs

let drop name = check name; store := List.remove_if (by_name name) !store; Hashtbl.remove charsets name

let rename oldname newname =
  let (_,cols) = get_stored oldname in
  add_columns (newname, cols);
  Option.may (set_charset newname) (get_charset oldname);
  drop oldname

let alter name f =
  check name;
  let alter_stored ((n, cols) as tbl) =
    if n = name then
      name, f cols
    else
      tbl
  in
  store := List.map alter_stored !store

let schema_to_columns ~cols ~new_col new_schema =
  List.map (fun attr ->
    if attr.Sql.name = new_col.attr.Sql.name then new_col
    else
      List.find_opt (fun c -> c.attr.Sql.name = attr.Sql.name) cols
      |> Option.map_default (fun existing -> { existing with attr }) (column_of_attr attr)
  ) new_schema

let alter_add name ~col ~pos =
  alter name (fun cols ->
    let new_schema = Sql.Schema.add (columns_to_schema cols) col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let alter_drop name ~col:col_name =
  alter name (fun cols ->
    List.filter (fun c -> c.attr.Sql.name <> col_name) cols)

let alter_change name ~oldcol ~col ~pos =
  alter name (fun cols ->
    let new_schema = Sql.Schema.change (columns_to_schema cols) oldcol col.attr pos in
    schema_to_columns ~cols ~new_col:col new_schema)

let rename_column name ~old_name ~new_name =
  alter name (fun cols ->
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
  alter name (List.map (fun c ->
    { c with attr = { c.attr with
      extra = Sql.Constraints.filter (Fun.negate is_pk_constraint) c.attr.Sql.extra } }))

let add_primary_key name ~cols:col_names =
  let pk = match col_names with
    | [_] -> Sql.Constraint.PrimaryKey
    | _ -> Sql.Constraint.make_composite_primary col_names
  in
  alter name (List.map (fun c ->
    if List.mem c.attr.Sql.name col_names then
      { c with attr = { c.attr with extra = Sql.Constraints.add pk c.attr.Sql.extra } }
    else c))

let print ch tables = let out = IO.output_channel ch in List.iter (Sql.print_table out) tables; IO.flush out
let print_all () = print stdout (List.map (fun (n, cols) -> (n, columns_to_schema cols)) !store)
let print1 name = print stdout [get @@ Sql.make_table_name name]

let reset () = store := []; Hashtbl.clear charsets

let all () = List.map (fun (n, cols) -> (n, columns_to_schema cols)) !store
