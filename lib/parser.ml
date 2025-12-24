module T_SQL_parser =
  struct
    type token = Sql_parser.token
    type result = Dialect_pass.Internal.stmt
    let rule = Sql_lexer.parse_rule
    let input = Sql_parser.input
  end

module T = Parser_utils.Make (T_SQL_parser)

type parse_result = {
  statement : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}

let parse_stmt stmt = 
  Parser_state.Dialect_feature.reset ();
  let internal_stmt = T.parse_buf_exn (Lexing.from_string stmt) in
  let parser_features = !Parser_state.Dialect_feature.state in
  let { Dialect_pass.ast; dialect_features = pass_features } = Dialect_pass.analyze internal_stmt in
  let dialect_features = parser_features @ pass_features in
   { statement = ast; dialect_features }
