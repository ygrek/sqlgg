open Stdlib
open Jsonkit.Primitives

type parse_result = {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
  relation_slots : Relation_slot.Name.t Sql.located list; [@default []] [@drop_default_if_json_equal]
}
[@@deriving json, jsonschema]

let parse_stmt sql =
  Parser_state.mode_normal ();
  let lexbuf = Lexing.from_string sql in
  let stmt =
    Parser_state.with_lexbuf lexbuf @@ fun () ->
    try Sql_parser.input Sql_lexer.parse_rule lexbuf
    with exn ->
      let error_position, error_token =
        Sql_lexer.pos lexbuf, Lexing.lexeme lexbuf
      in
      raise (Parser_utils.Error (exn, {
        pos = error_position;
        token = error_token;
        tail = Sql_lexer.ruleTail lexbuf;
      }))
  in
  let (dialect_features, slots) = Dialect.analyze stmt in
  let relation_slots =
    List.filter_map (fun (id : Sql.param_id) ->
      Option.map (fun name -> { id with value = name }) (Relation_slot.Name.of_param_id id)) slots
    |> List.stable_sort (fun (a : _ Sql.located) (b : _ Sql.located) -> Sql.Pos.compare a.pos b.pos)
  in
  { stmt; dialect_features; relation_slots }
