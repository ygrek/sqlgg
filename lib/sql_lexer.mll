
{
  open Printf
  open Lexing
  open ExtLib
  open Sql_parser
  module T = Sql.Type

let error _ callerID =
  prerr_endline (sprintf "Lexer error : %s" callerID);
(*	update_pos buf;*)
	raise Parsing.Parse_error

let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

let advance_line_pos pos =
  { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

let advance_line lexbuf =
  lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

let keywords =
  let k = ref [
   "as",AS;
   "on",ON;
   "to",TO;
   "conflict",CONFLICT;
   "using",USING;
   "natural",NATURAL;
   "join",JOIN;
   "between",BETWEEN;
   "and",AND;
   "escape",ESCAPE;
   "not",NOT;
   "null",NULL;
   "unique",UNIQUE;
   "primary",PRIMARY;
   "key",KEY;
   "default",DEFAULT;
   "precision",PRECISION;
   "varying",VARYING;
   "charset",CHARSET;
   "collate",COLLATE;
   "national",NATIONAL;
   "ascii",ASCII;
   "unicode",UNICODE;
   "distinct",DISTINCT;
   "character",CHARACTER;
   "binary",BINARY;
   "bytea",BINARY;
   "all",ALL;
   "any",ANY;
   "some",SOME;
   "order",ORDER;
   "by",BY;
   "limit",LIMIT;
   "desc",DESC;
   "asc",ASC;
   "offset",OFFSET;
   "select",SELECT;
   "create",CREATE;
   "table",TABLE;
   "view",VIEW;
   "insert",INSERT;
   "replace",REPLACE;
   "update",UPDATE;
   "delete",DELETE;
   "from",FROM;
   "or",OR;
   "into",INTO;
   "values",VALUES;
   "where",WHERE;
   "set",SET;
   "in",IN;
   "group",GROUP;
   "having",HAVING;
   "union",UNION;
   "except",EXCEPT;
   "intersect",INTERSECT;
   "cross",CROSS;
   "temporary",TEMPORARY;
   "if",IF;
   "exists",EXISTS;
   "foreign",FOREIGN;
   "global",GLOBAL;
   "local",LOCAL;
   "value",VALUE;
   "references",REFERENCES;
   "check",CHECK;
   "date",DATE;
   "time",TIME;
   "timestamp",TIMESTAMP;
   "alter",ALTER;
   "rename",RENAME;
   "add",ADD;
   "cascade",CASCADE;
   "restrict",RESTRICT;
   "drop",DROP;
   "constraint",CONSTRAINT;
   "after",AFTER;
   "index",INDEX;
   "fulltext",FULLTEXT;
   "unsigned",UNSIGNED;
   "first",FIRST;
   "column",COLUMN;
   "like", LIKE;
   "case", CASE;
   "when", WHEN;
   "then", THEN;
   "else", ELSE;
   "end", END;
   "change", CHANGE;
   "modify", MODIFY;
   "delayed", DELAYED;
   "enum", ENUM;
   "for", FOR;
   "share", SHARE;
   "mode", MODE;
   "lock", LOCK;
   "of", OF;
   "with", WITH;
   "nowait", NOWAIT;
   "action", ACTION;
   "no", NO;
   "is", IS;
   "interval", INTERVAL;
   "microsecond", MICROSECOND;
   "second", SECOND;
   "minute", MINUTE;
   "hour", HOUR;
   "day", DAY;
   "week", WEEK;
   "month", MONTH;
   "quarter", QUARTER;
   "year", YEAR;
   "second_microsecond", SECOND_MICROSECOND;
   "minute_microsecond", MINUTE_MICROSECOND;
   "minute_second", MINUTE_SECOND;
   "hour_microsecond", HOUR_MICROSECOND;
   "hour_second", HOUR_SECOND;
   "hour_minute", HOUR_MINUTE;
   "day_microsecond", DAY_MICROSECOND;
   "day_second", DAY_SECOND;
   "day_minute", DAY_MINUTE;
   "day_hour", DAY_HOUR;
   "year_month", YEAR_MONTH;
   "false", FALSE;
   "true", TRUE;
   "duplicate", DUPLICATE;
   "function", FUNCTION;
   "procedure", PROCEDURE;
   "returns", RETURNS;
   "begin", BEGIN;
   "comment", COMMENT;
   "language", LANGUAGE;
   "substring", SUBSTRING;
   "substr", SUBSTRING;
  ] in (* more *)
  let all token l = k := !k @ List.map (fun x -> x,token) l in
  all DATETIME_FUNC ["current_date";"current_timestamp";"current_time";"localtime";"localtimestamp";"now";];
  all DATETIME_FUNC ["getdate"]; (* mssql? *)
  all CONFLICT_ALGO ["ignore"; "abort"; "fail"; "rollback"];
  all JOIN_TYPE1 ["left";"right";"full"];
  all JOIN_TYPE2 ["inner";"outer"];
  all LIKE_OP ["glob";"regexp";"match"];
  all AUTOINCREMENT ["autoincrement";"auto_increment"];
(* standard built-in types
      CHARACTER, CHARACTER VARYING, CHARACTER LARGE OBJECT,
      BINARY, BINARY VARYING, BINARY LARGE OBJECT,
      NUMERIC, DECIMAL, INTEGER, SMALLINT, BIGINT,
      FLOAT, REAL, DOUBLE PRECISION,
      BOOLEAN,
      DATE, TIME, TIMESTAMP, INTERVAL
    *)
  all T_INTEGER ["integer";"int";"smallint";"bigint";"tinyint";"mediumint";"middleint";"serial";"identity"];
  all T_INTEGER ["numeric";"decimal";"dec";"fixed"];
  all T_INTEGER ["number"]; (* oracle *)
  all T_BOOLEAN ["bool";"boolean"];
  all T_FLOAT ["float";"real";"double";"float4";"float8";"int1";"int2";"int3";"int4";"int8"];
  all T_BLOB ["blob";"varbinary";"tinyblob";"mediumblob";"longblob"];
  all T_TEXT ["text";"char";"varchar";"tinytext";"mediumtext";"longtext"];
  all T_TEXT ["varchar2"]; (* oracle *)
  all T_DATETIME ["datetime"];
  all T_UUID ["uuid"]; (* http://www.postgresql.org/docs/9.4/static/datatype-uuid.html *)
  !k

(*
  Q: Why not convert all input to lowercase before lexing?
  A: Sometimes SQL is case-sensitive, also string contents should be preserved
*)

module Keywords = Map.Make(String)

let keywords =
  let add map (k,v) =
    let k = String.lowercase k in
    if Keywords.mem k map then
      failwith (sprintf "Lexeme %s is already associated with keyword." k)
    else
      Keywords.add k v map
  in
  List.fold_left add Keywords.empty keywords

(* FIXME case sensitivity??! *)

let get_ident str =
  let str = String.lowercase str in
  try Keywords.find str keywords with Not_found -> IDENT str

let ident str = IDENT (String.lowercase str)

let as_literal ch s =
  let s = String.replace_chars (fun x -> String.make (if x = ch then 2 else 1) x) s in
  sprintf "%c%s%c" ch s ch
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )*
let wsp = [' ' '\r' '\t']
let cmnt = "--" | "//" | "#"

(* extract separate statements *)
rule ruleStatement = parse
  | ['\n' ' ' '\r' '\t']+ as tok { `Space tok }
  | cmnt wsp* "[sqlgg]" wsp+ (ident+ as n) wsp* "=" wsp* ([^'\n']* as v) '\n' { `Prop (n,v) }
  | cmnt wsp* "@" (ident+ as name) [^'\n']* '\n' { `Prop ("name",name) }
  | '"' { let s = ruleInQuotes "" lexbuf in `Token (as_literal '"' s) }
  | "'" { let s = ruleInSingleQuotes "" lexbuf in `Token (as_literal '\'' s) }
  | "$" (ident? as tag) "$" { let s = ruleInDollarQuotes tag "" lexbuf in `Token (sprintf "$%s$%s$%s$" tag s tag) }
  | cmnt as s { `Comment (s ^ ruleComment "" lexbuf) }
  | "/*" { `Comment ("/*" ^ ruleCommentMulti "" lexbuf ^ "*/") }
  | ';' { `Semicolon }
  | [^ ';'] as c { `Char c }
  | eof { `Eof }
and
(* extract tail of the input *)
ruleTail acc = parse
  | eof { acc }
  | _* as str { ruleTail (acc ^ str) lexbuf }
and
ruleMain = parse
  | wsp   { ruleMain lexbuf }
  (* update line number *)
  | '\n'  { advance_line lexbuf; ruleMain lexbuf}

  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','   { COMMA }
  | '.'   { DOT }
  | '{'   { LCURLY (lexeme_start lexbuf) }
  | '}'   { RCURLY (lexeme_start lexbuf) }

  | cmnt { ignore (ruleComment "" lexbuf); ruleMain lexbuf }
  | "/*" { ignore (ruleCommentMulti "" lexbuf); ruleMain lexbuf }

  | "*" { ASTERISK }
  | "=" { EQUAL }
  | "!" { EXCL }
  | "~" { TILDE }
  | "||" { CONCAT_OP }
  | "+" { PLUS }
  | "-" { MINUS }

  | "/" | "%" { NUM_DIV_OP }
  | "<<" | ">>" { NUM_BIT_SHIFT }
  | "|" { NUM_BIT_OR }
  | "&" { NUM_BIT_AND }
  | ">" | ">=" | "<=" | "<" { NUM_CMP_OP }
  | "<>" | "!=" | "==" { NUM_EQ_OP }
  | "<=>" { NOT_DISTINCT_OP }

  | "?" { PARAM (None,pos lexbuf) }
  | [':' '@'] (ident as str) { PARAM (Some str,pos lexbuf) }

  | '"' { ident (ruleInQuotes "" lexbuf) }
  | "'" { TEXT (ruleInSingleQuotes "" lexbuf) }
  (* http://www.postgresql.org/docs/current/interactive/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING *)
  | "$" (ident? as tag) "$" { TEXT (ruleInDollarQuotes tag "" lexbuf) }
  | "`" { ident (ruleInBackQuotes "" lexbuf) }
  | "[" { ident (ruleInBrackets "" lexbuf) }
  | ['x' 'X'] "'" { BLOB (ruleInSingleQuotes "" lexbuf) }

  | ident as str { get_ident str }
  | digit+ as str { INTEGER (int_of_string str) }
  | digit+ '.' digit+ as str { FLOAT (float_of_string str) }
  | eof		{ EOF }
  | _	{ error lexbuf "ruleMain" }
and
(* FIXME factor out all that ruleIn* rules *)
ruleInQuotes acc = parse
  | '"'	        { acc }
  | eof	        { error lexbuf "no terminating quote" }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating quote" }
  | "\"\""      { ruleInQuotes (acc^"\"") lexbuf }
  | [^'"' '\n']+ as s { ruleInQuotes (acc^s) lexbuf }
  | _		{ error lexbuf "ruleInQuotes" }
and
ruleInBrackets acc = parse
  | ']'	        { acc }
  | eof	        { error lexbuf "no terminating bracket" }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating bracket" }
(*   | "\"\""      { ruleInQuotes (acc ^ "\"") lexbuf } *)
  | [^']' '\n']+  { ruleInBrackets (acc ^ lexeme lexbuf) lexbuf }
  | _		{ error lexbuf "ruleInBrackets" }
and
ruleInSingleQuotes acc = parse
  | '\''	      { acc }
  | eof	        { error lexbuf "no terminating single quote" }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating single quote" }
  | "''"        { ruleInSingleQuotes (acc ^ "'") lexbuf }
  | [^'\'' '\n']+  { ruleInSingleQuotes (acc ^ lexeme lexbuf) lexbuf }
  | _		{ error lexbuf "ruleInSingleQuotes" }
and
ruleInBackQuotes acc = parse
  | '`'	        { acc }
  | eof	        { error lexbuf "no terminating back quote" }
  | '\n'        { advance_line lexbuf; error lexbuf "EOL before terminating back quote" }
  | "``"        { ruleInBackQuotes (acc ^ "`") lexbuf }
  | [^'`' '\n']+  { ruleInBackQuotes (acc ^ lexeme lexbuf) lexbuf }
  | _		{ error lexbuf "ruleInBackQuotes" }
and
ruleInDollarQuotes tag acc = parse
  | "$" (ident? as tag_) "$" { if tag_ = tag then acc else ruleInDollarQuotes tag (acc ^ sprintf "$%s$" tag_) lexbuf }
  | eof	        { error lexbuf "no terminating dollar quote" }
  | '\n'        { advance_line lexbuf; ruleInDollarQuotes tag (acc ^ "\n") lexbuf }
  (* match one char at a time to make sure delimiter matches longer *)
  | [^'\n']     { ruleInDollarQuotes tag (acc ^ lexeme lexbuf) lexbuf }
  | _		{ error lexbuf "ruleInDollarQuotes" }
and
ruleComment acc = parse
  | '\n'	{ advance_line lexbuf; acc }
  | eof	        { acc }
  | [^'\n']+    { let s = lexeme lexbuf in ruleComment (acc ^ s) lexbuf; }
  | _		{ error lexbuf "ruleComment"; }
and
ruleCommentMulti acc = parse
  | '\n'	{ advance_line lexbuf; ruleCommentMulti (acc ^ "\n") lexbuf }
  | "*/"	{ acc }
  | "*"
  | [^'\n' '*']+    { let s = lexeme lexbuf in ruleCommentMulti (acc ^ s) lexbuf }
  | _	        { error lexbuf "ruleCommentMulti" }

{

  let parse_rule lexbuf =
    let module P = Parser_state in
    let token = ruleMain lexbuf in
    match !P.mode with
    | P.Normal -> token
    | P.Ignore ->
(*         eprintf "ignored: %s\n" (lexeme lexbuf); *)
      if (token = EOF) then token else IGNORED

}

