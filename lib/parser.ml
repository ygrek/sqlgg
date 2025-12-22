module T_SQL_parser =
  struct
    type token = Sql_parser.token
    type result = Sql.stmt
    let rule = Sql_lexer.parse_rule
    let input = Sql_parser.input
  end

module T = Parser_utils.Make (T_SQL_parser)

type parse_result = {
  statement : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}

let parse_stmt stmt = 
  let stmt = T.parse_buf_exn (Lexing.from_string stmt) in
  let dialect_features = Dialect.analyze stmt in
  { statement = stmt; dialect_features }
