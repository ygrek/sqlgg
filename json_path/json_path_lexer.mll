{
  open Json_path_parser

  exception LexError of string
}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier_start = letter | '_' | '$'
let identifier_char = identifier_start | digit

rule token = parse
  | whitespace+ { token lexbuf }
  | newline     { Lexing.new_line lexbuf; token lexbuf }  (* ← используйте встроенную функцию *)
  | '$'         { DOLLAR }
  | '.'         { DOT }
  | '*'         { ASTERISK }
  | "**"        { DOUBLE_ASTERISK }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | '-'         { MINUS }
  | "to"        { TO }
  | "last"      { LAST }
  | digit+ as n { INTEGER (int_of_string n) }
  | identifier_start identifier_char* as id { IDENTIFIER id }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | eof         { EOF }
  | _ as c      { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' _    { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | newline   { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | eof       { raise (LexError "Unterminated string") }
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf }

{
}
