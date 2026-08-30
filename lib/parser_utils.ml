type error_info = { pos : Sql.pos; token : string; tail : string }

exception Error of exn * error_info

let rec message_of_exn = function
  | Sql.Schema.Error (_, msg) -> msg
  | Failure msg -> msg
  | Prelude.At (_, exn) -> message_of_exn exn
  | Sql_parser.Error -> "syntax error"
  | Sql_lexer.Error (msg, _) -> msg
  | exn -> Printexc.to_string exn
