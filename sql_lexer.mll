(*
  $Id$ 
*)

{
  open Sql_parser

  let curStr = ref ""
  let store str = curStr := str

let error buf callerID =
  Error.report "Lexer error : %s" callerID;
(*	update_pos buf;*)
	raise Parsing.Parse_error

let advance_line_pos pos = 
  let module L = Lexing in
  {L.pos_fname=pos.L.pos_fname;
   pos_lnum = pos.L.pos_lnum + 1;
   pos_bol = 0;
   pos_cnum = pos.L.pos_cnum}

let advance_line lexbuf = 
  lexbuf.Lexing.lex_curr_p <- advance_line_pos lexbuf.Lexing.lex_curr_p
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )+
let wsp = [' ' '\t']
let mnot = ("NOT" wsp+)?

rule ruleStatement props = parse
  | ['\n' ' ' '\t']+ { ruleStatement props lexbuf }
(* fixme strings *)
  | "--" wsp* "[sql2cpp]" wsp+ (ident+ as n) wsp* "=" wsp* ([^'\n']* as v) '\n' 
      { 
        ruleStatement (Props.set props n v) lexbuf
      }
  | "--" { store ""; ignore (ruleComment lexbuf); ruleStatement props lexbuf }
  | alpha [^ ';']+ as stmt ';' { Some (stmt,props) }
  | _ { None }
and
ruleMain = parse
  | wsp   { ruleMain lexbuf }
  (* update line number *)
  | '\n'  { advance_line lexbuf; ruleMain lexbuf}

  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','   { COMMA }
  | '.'   { DOT }

  | "--" { store ""; ignore (ruleComment lexbuf); ruleMain lexbuf }
(*  | '"' { store ""; ruleInQuotes lexbuf } *)

  | "SELECT" { SELECT }
  | "CREATE" wsp+ "TABLE" { CREATE_TABLE }
  | "INSERT" { INSERT }
  | "REPLACE" { REPLACE }
  | "UPDATE" { UPDATE }
  | "DELETE" wsp+ "FROM" { DELETE_FROM }

  | "OR" { OR }
  | "INTO" { INTO }
  | "VALUES" { VALUES }
  | "WHERE" { WHERE }
  | "FROM" { FROM }
  | "*" { ASTERISK }
  | "SET" { SET }

  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIVIDE }
  | "%" { PERCENT }
  | "!" { EXCL }
  | "~" { TILDE }
  | "NOT" { NOT }

  (* column-constraint *)
  | "NOT" wsp+ "NULL" { NOT_NULL }
  | "UNIQUE" { UNIQUE }
  | "PRIMARY" wsp+ "KEY" { PRIMARY_KEY }
  | "AUTOINCREMENT" { AUTOINCREMENT }
  | "DEFAULT" { DEFAULT }

  | "TEXT" { T_TEXT }
  | "INTEGER" { T_INTEGER }
  | "INT" { T_INTEGER }
  | "BLOB" { T_BLOB }

  | "DISTINCT" { DISTINCT }
  | "ALL" { ALL }

  | "ORDER" wsp+ "BY" { ORDER_BY }
  | "LIMIT" { LIMIT }
  | "DESC" { DESC }
  | "ASC" { ASC }
  | "OFFSET" { OFFSET }

  | "?" { PARAM Stmt.Raw.Next }

  | "ON" { ON }
  | "CONFLICT" { CONFLICT }
  | "USING" { USING }

  | "IGNORE" { IGNORE }
  | "REPLACE" { REPLACE }
  | "ABORT" { ABORT }
  | "FAIL" { FAIL }
  | "ROLLBACK" { ROLLBACK }

  | ("NATURAL" wsp+)? 
    (("LEFT"|"RIGHT"|"FULL") wsp+)? 
    (("INNER"|"OUTER"|"CROSS") wsp+)? 
    "JOIN" { JOIN }

  | mnot ("LIKE" | "GLOB" | "REGEXP" | "MATCH") { LIKE_OP }

  | "MAX" | "MIN" | "CONCAT" { FUNCTION }
  | "ISNULL" | "NOTNULL" { TEST_NULL }
  | "BETWEEB" { BETWEEN }
  | "AND" { AND }
  | "ESCAPE" { ESCAPE }

  | ident as str { IDENT (str) }
  | digit+ as str { INTEGER (int_of_string str) }
  | eof		{ EOF }
  | _		{ error lexbuf "ruleMain" }
and 
(*ruleInQuotes = parse
  | '"'	        { TEXT (!curStr) }
  | eof	        { error lexbuf "no terminating quote"; }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating quote"; }
  | [^'"' '\n']+  { store (Lexing.lexeme lexbuf); ruleInQuotes lexbuf; }
  | _		{ error lexbuf "ruleInQuotes"; }
and*)
ruleComment = parse
  | '\n'	      { advance_line lexbuf; !curStr }
  | eof	        { !curStr }
  | [^'\n']+    { store (Lexing.lexeme lexbuf); ruleComment lexbuf; }
  | _		{ error lexbuf "ruleComment"; }

{

  let parse_rule lexbuf = ruleMain lexbuf

}
