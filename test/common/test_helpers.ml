open Sqlgg

let messages diagnostics =
  String.concat "; " (List.map (fun (d : Analysis.diagnostic) -> d.message) diagnostics)

let get_ok = function
  | Ok value -> value
  | Error (first, rest) -> OUnit2.assert_failure (messages (first :: rest))

let context sql =
  get_ok (Result.map_error (fun (_, first, rest) -> first, rest) (Analysis.context_of_schema_sql sql))

let compile_all sql =
  List.iter
    (fun statement -> ignore (Compile.statement ~dynamic_select:Props.Off statement : Compile.outcome))
    (Statements.split sql)

let statement sql = List.hd (Statements.split sql)

let parse ?allow_extensions sql = Analysis.parse ?allow_extensions (statement sql)

let analyze ?allow_extensions ~context sql =
  Analysis.analyze ?allow_extensions ~context (statement sql)
