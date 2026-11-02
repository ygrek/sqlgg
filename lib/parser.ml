open Stdlib
open Jsonkit.Primitives

type parse_result = {
  stmt : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
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
  { stmt; dialect_features = Dialect.analyze stmt }
