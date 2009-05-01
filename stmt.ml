(* $Id$ *)

open Printf
open ExtString
open ListMore

type value_t = string * Sql.Type.t (** name and type *)
     deriving (Show)

type values = value_t list 
deriving (Show)

module Raw =
struct
  type param_id = | Named of string | Numbered of int | Next deriving (Show)
  type param_type = Sql.Type.t option deriving (Show)
  type param = param_id * param_type deriving (Show)
  type params = param list deriving (Show)
 
  let to_string ps = Show.show<params>(ps)

  type kind = | Select
              | Insert
              | Create
              | Update
              | Delete
       deriving (Show)

(*
  type parsed = kind 
                * string  (** table name as string *)
                * values  (** placeholders for input parameters *)
       deriving (Show)

  type t = kind 
           * string  (** table name as string *)
           * values  (** placeholders for input parameters *)
           * Props.t (** some user directives *)
           * string  (** raw sql untouched *)
       deriving (Show)

let to_sql k table sql =
  match k with
  | Insert All ->
      sprintf "%s (%s)"
        sql
        (String.concat "," (List.map (fun _ -> "?") table.Sql.Table.cols))
  | Insert (Cols cols) ->
      sprintf "%s (%s)"
        sql
        (String.concat "," (List.map (fun _ -> "?") cols))
  | Delete
  | Update _
  | Create _
  | Select _ -> sql

let to_sql k table sql =
  let escape = function
  | '\n' -> "\\\n"
  | c -> String.make 1 c
  in  
  String.replace_chars escape (to_sql k table sql)

*)
end (* module Raw *)

(*
type column = Sql.Col.t * Sql.Table.t
     deriving (Show)

type kind = | Select of column list (** rowset *)
                        * values (** rowset expressions *)
                        * values (** input parameters *)
            | Create 
            | Modify of column list (** modified columns *)
                        * values (** input parameters *)
            | Delete of values (** input parameters *)
     deriving (Show)

type t = kind
         * Sql.Table.t (** this is temporary workaround *)
         * Props.t     (** some user directives *)
         * string      (** corresponding SQL query *)
     deriving (Show)

exception Bad_column of Sql.Table.t * string

(** resolve all names given as strings to corresponding values *)
let resolve stmts =
  let tables = ref [] in
  let get_table name = List.find_option (fun table -> table.Sql.Table.name = name) !tables in
  let get_column table name = List.find_option (fun col -> col.Sql.Col.name = name) table.Sql.Table.cols in
  (* fix here for multiple tables *)
  let resolve_columns table columns =
    match columns with
    | Raw.All -> List.map (fun col -> col,table) table.Sql.Table.cols
    | Raw.Cols cols ->
        List.map (fun colname -> match get_column table colname with
                                 | Some col -> col, table
                                 | None -> raise (Bad_column (table,colname))) cols
  in
  let resolve_one stmt =
    let (kind,name,inputs,props,raw_sql) = stmt in
    let sql table = Raw.to_sql kind table raw_sql in
    match get_table name with
    | None ->
        begin match kind with
        | Raw.Create table ->
            tables := table::!tables; 
            assert (List.length inputs = 0);
            Some (Create, table, props, sql table)
        | _ -> Error.log "No such table %s" name; None
        end
    | Some table -> 
        begin try
          (match kind with
          | Raw.Create _ -> Error.log "Duplicate CREATE for table %s" name; None
          | Raw.Select colnames -> 
              let outputs = resolve_columns table colnames in
              Some (Select (outputs,[],inputs), table, props, sql table)
          | Raw.Insert colnames
          | Raw.Update colnames ->
              let cols = resolve_columns table colnames in
              Some (Modify (cols,inputs), table, props, sql table)
          | Raw.Delete -> Some (Delete inputs, table, props, sql table))
          with
            Bad_column (table,column) -> 
              Error.log "Column %s not found in %s" column table.Sql.Table.name; 
              None
        end
  in
  List.filter_map resolve_one stmts
*)
