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

(* use Map or Hashtbl ? *)
let keywords = 
  let k = ref [ 
   "as",AS;
   "on",ON;
   "conflict",CONFLICT;
   "using",USING;
   "natural",NATURAL;
   "join",JOIN;
   "isnull",TEST_NULL;
   "notnull",TEST_NULL;
   "between",BETWEEN;
   "and",AND;
   "escape",ESCAPE;
   "not",NOT;
   "null",NULL;
   "unique",UNIQUE;
   "primary",PRIMARY;
   "key",KEY;
   "autoincrement",AUTOINCREMENT;
   "default",DEFAULT;
   "text",T_TEXT;
   "integer",T_INTEGER;
   "int",T_INTEGER;
   "blob",T_BLOB;
   "distinct",DISTINCT;
   "all",ALL;
   "order",ORDER;
   "by",BY;
   "limit",LIMIT;
   "desc",DESC;
   "asc",ASC;
   "offset",OFFSET;
  ] in
  let all token l = k := !k @ List.map (fun x -> x,token) l in
  all FUNCTION ["max"; "min"; "concat"; "length"; "random";];
  all CONFLICT_ALGO ["ignore"; "replace"; "abort"; "fail"; "rollback";];
  all JOIN_TYPE1 ["left";"right";"full"];
  all JOIN_TYPE2 ["inner";"outer";"cross"];
  all LIKE_OP ["like";"glob";"regexp";"match"];
  !k 

let keywords = List.map (fun (k,v) -> (String.lowercase k, v)) keywords

let get_ident str =
  let str = String.lowercase str in
  try List.assoc str keywords with Not_found -> IDENT str 
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )*
let wsp = [' ' '\t']

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

  | "UNION" (wsp+ "ALL")? | "EXCEPT" | "INTERSECT" { COMPOUND_OP }

  | "=" { EQUAL }
  | "!" { EXCL }
  | "~" { TILDE }
  | "NOT" { NOT }
  | "||" { CONCAT_OP }
  | "+" { PLUS }
  | "-" { MINUS }

  | "/" | "%" | ">" | ">=" | "<=" | "<" | "&" | "|" { NUM_BINARY_OP }

  | "?" { PARAM Stmt.Raw.Next }
  | "?" (digit+ as str) { PARAM (Stmt.Raw.Numbered (int_of_string str)) }
  | [':' '@'] (ident as str) { PARAM (Stmt.Raw.Named str) }

  | "'" { TEXT (ruleInSingleQuotes "" lexbuf) }
  | ['x' 'X'] "'" { BLOB (ruleInSingleQuotes "" lexbuf) }

  | ident as str { get_ident str }
  | digit+ as str { INTEGER (int_of_string str) }
  | eof		{ EOF }
  | _		{ error lexbuf "ruleMain" }
and 
ruleInSingleQuotes acc = parse
  | '\''	      { acc }
  | eof	        { error lexbuf "no terminating quote" }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating quote" }
  | "''"        { ruleInSingleQuotes (acc ^ "'") lexbuf }
  | [^'\'' '\n']+  { ruleInSingleQuotes (acc ^ Lexing.lexeme lexbuf) lexbuf }
  | _		{ error lexbuf "ruleInSingleQuotes" }
and
ruleComment = parse
  | '\n'	      { advance_line lexbuf; !curStr }
  | eof	        { !curStr }
  | [^'\n']+    { store (Lexing.lexeme lexbuf); ruleComment lexbuf; }
  | _		{ error lexbuf "ruleComment"; }

{

  let parse_rule lexbuf = ruleMain lexbuf

}
