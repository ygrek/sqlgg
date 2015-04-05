
module T_SQL_parser =
  struct
    type token = Sql_parser.token
    type result = Sql.Schema.t * Stmt.params * Stmt.kind
    let rule = Sql_lexer.parse_rule
    let input = Sql_parser.input
  end

module T = Parser_utils.Make (T_SQL_parser)

let parse_stmt stmt = T.parse_buf_exn (Lexing.from_string stmt)
