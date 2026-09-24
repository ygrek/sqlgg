open Printf
open ExtLib
open Sqlgg
open Stmt
open Jsonkit.Primitives

type t = {
  sql : string;
  schema : Sql.schema_column list;
  vars : Sql.var list;
  kind : kind;
  props : Props.t list;
}
[@@deriving json, jsonschema]

let of_result props ({ typed = { sql; schema; vars; kind }; _ } : Syntax.result) =
  { sql; schema; vars; kind; props }

let verbatim ~props sql = { sql; schema = []; vars = []; kind = Other; props }

let name props kind index =
  let sanitize_chars =
    String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as c) -> c
        | _ -> '_')
  in
  let sanitize_name s =
    match Props.substs props with
    | x :: _ ->
        let _, s =
          String.replace ~str:s ~sub:("%%" ^ x ^ "%%") ~by:x
        in
        sanitize_chars s
    | [] -> sanitize_chars s
  in
  let sanitize_table t = sanitize_name @@ Sql.show_table_name t in
  let name =
    match kind with
    | Create t -> sprintf "create_%s" (sanitize_table t)
    | CreateIndex t -> sprintf "create_index_%s" (sanitize_name t)
    | Update (Some t) -> sprintf "update_%s_%u" (sanitize_table t) index
    | Update None -> sprintf "update_%u" index
    | Insert (_, t) -> sprintf "insert_%s_%u" (sanitize_table t) index
    | Delete t ->
        sprintf "delete_%s_%u"
          (String.concat "_" @@ List.map sanitize_table t)
          index
    | Alter t ->
        sprintf "alter_%s_%u" (String.concat "_" @@ List.map sanitize_table t) index
    | Drop t -> sprintf "drop_%s" (sanitize_table t)
    | Select _ -> sprintf "select_%u" index
    | CreateRoutine s -> sprintf "create_routine_%s" (sanitize_table s)
    | Other -> sprintf "statement_%u" index
    | CreateType n -> sprintf "create_type_%s" (sanitize_name n)
    | DropType n -> sprintf "drop_type_%s" (sanitize_name n)
  in
  Stdlib.Option.value ~default:name (Props.name props)

let template q = Sql_template.of_sql q.sql q.vars

type named = {
  name : string;
  stmt : t;
  template : Sql_template.t list;
}
[@@deriving json, jsonschema]

let named index stmt =
  { name = name stmt.props stmt.kind index; stmt; template = template stmt }

type document = {
  sqlgg_version : string option;
  module_name : string;
  dialect : Dialect.t;
  params : Sql_template.placeholder option;
  queries : named list;
  tables : Tables.stored_table list;
}
[@@deriving json, jsonschema]
