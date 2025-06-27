open Ast

module Lexer = Json_path_lexer
module Parser =Json_path_parser

let parse_json_path input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.path_expression Lexer.token lexbuf
  with
  | Lexer.LexError msg ->
      failwith ("Lexer error: " ^ msg)
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      failwith (Printf.sprintf "Parser error at line %d, column %d" 
                pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

let is_valid input =
  try
    let _ = parse_json_path input in
    true
  with
  | _ -> false

let string_of_array_index = function
  | Index n -> string_of_int n
  | Wildcard -> "*"
  | Last -> "last"
  | LastOffset n -> "last-" ^ string_of_int n

let string_of_range range =
  string_of_array_index range.start_idx ^ " to " ^ string_of_array_index range.end_idx

let string_of_path_leg = function
  | Member key when String.contains key ' ' || String.contains key '-' -> ".\"" ^ key ^ "\""
  | Member key -> "." ^ key
  | ArrayAccess idx -> "[" ^ string_of_array_index idx ^ "]"
  | ArrayRange range -> "[" ^ string_of_range range ^ "]"
  | MemberWildcard -> ".*"
  | DoubleWildcard -> "**"

let string_of_json_path path =
  path.scope ^ String.concat "" (List.map string_of_path_leg path.legs)
