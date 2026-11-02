type error_info = { pos : Sql.Pos.t; token : string; tail : string }

exception Error of exn * error_info

let rec message_of_exn = function
  | Sql.Schema.Error (_, msg) -> msg
  | Failure msg -> msg
  | Prelude.At (_, exn) -> message_of_exn exn
  | Sql_parser.Error -> "syntax error"
  | Sql_lexer.Error (msg, _) -> msg
  | exn -> Printexc.to_string exn

let rec is_reportable_sql_error = function
  | Sql_parser.Error | Sql_lexer.Error _ | Sql.Schema.Error _ | Failure _ -> true
  | Prelude.At (_, exn) | Error (exn, _) -> is_reportable_sql_error exn
  | _ -> false
