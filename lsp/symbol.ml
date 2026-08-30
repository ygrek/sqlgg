open Sqlgg

type loc = {
  file : string;
  pos : Pos.t;
}

type kind =
  | Table
  | Cte
  | Local

type column = {
  attr : Sql.attr;
  loc : loc option;
}

type t = {
  name : string;
  kind : kind;
  loc : loc option;
  columns : column list;
}

let loc ~file pos = { file; pos }
let column ?loc attr = { attr; loc }
let make ~name ~kind ?loc columns = { name; kind; loc; columns }
let rename name t = { t with name }

let columns t = List.map (fun col -> col.attr) t.columns

let find_column t name = List.find_opt (fun col -> String.equal col.attr.name name) t.columns

let find symbols name = List.find_opt (fun sym -> String.equal sym.name name) symbols

let unique = Prelude.unique_by (module String) (fun sym -> sym.name)
