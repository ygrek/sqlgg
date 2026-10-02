open Sqlgg
open Jsonkit.Primitives

type resolution =
  | Parsed of Parser.parse_result
  | Analyzed of {
      parsed : Parser.parse_result;
      typed : Syntax.signature;
    }
  | Invalid of {
      diagnostics : Analysis.diagnostic list;
    }
[@@deriving json, jsonschema]

type statement = {
  sql : string;
  props : Props.t list;
  resolution : resolution;
}
[@@deriving json, jsonschema]

type document = { statements : statement list }
[@@deriving json, jsonschema]

let statements resolve sql =
  List.map
    (fun (statement : Statements.t) ->
      let resolution =
        match resolve statement with
        | Ok resolution -> resolution
        | Error (first, rest) -> Invalid { diagnostics = first :: rest }
      in
      { sql = statement.text; props = statement.props; resolution })
    (Statements.split sql)

let parse ?allow_extensions sql =
  statements
    (fun statement ->
      Result.map (fun parsed -> Parsed parsed) (Analysis.parse ?allow_extensions statement))
    sql

let analyze ?allow_extensions ~context sql =
  statements
    (fun statement ->
      Result.map
        (fun (parsed, (result : Syntax.result)) -> Analyzed { parsed; typed = result.typed })
        (Analysis.analyze ?allow_extensions ~context statement))
    sql
