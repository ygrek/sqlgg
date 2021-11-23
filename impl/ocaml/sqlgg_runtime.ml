

(* cf. https://dev.mysql.com/doc/refman/5.7/en/string-literals.html *)
let mysql_string_to_literal s =
  let b = Buffer.create (String.length s + String.length s / 4) in
  Buffer.add_string b "'";
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '\\' -> Buffer.add_string b "\\\\"
    | '\000' -> Buffer.add_string b "\\0"
    | '\'' -> Buffer.add_string b "\\'"
    | c -> Buffer.add_char b c
  done;
  Buffer.add_string b "'";
  Buffer.contents b

(* cf. https://sqlite.org/lang_expr.html "Literal Values"
    "A string constant is formed by enclosing the string in single quotes
    ('). A single quote within the string can be encoded by putting two
    single quotes in a row - as in Pascal. C-style escapes using the
    backslash character are not supported because they are not standard
    SQL." *)
let sqlite3_string_to_literal s =
  let b = Buffer.create (String.length s + String.length s / 4) in
  Buffer.add_string b "'";
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '\'' -> Buffer.add_string b "''"
    | c -> Buffer.add_char b c
  done;
  Buffer.add_string b "'";
  Buffer.contents b
