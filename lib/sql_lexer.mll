
{
  open Printf
  open Lexing
  open ExtLib
  open Sql_tokens
  module T = Sql.Type

let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

exception Error of string * Sql.pos

let error lexbuf msg = raise (Error (msg, pos lexbuf))

let unescape char s =
  let quote = String.make 1 char in
  String.nsplit s (quote ^ quote) |> String.concat quote

let keywords =
  let k = ref [
   "action", ACTION;
   "add",ADD;
   "after",AFTER;
   "all",ALL;
   "alter",ALTER;
   "and",AND;
   "any",ANY;
   "as",AS;
   "asc",ASC;
   "ascii",ASCII;
   "begin", BEGIN;
   "between",BETWEEN;
   "binary",BINARY;
   "by",BY;
   "bytea",BINARY;
   "cache",CACHE;
   "cascade",CASCADE;
   "case", CASE;
   "cast", CAST;
   "change", CHANGE;
   "character",CHARACTER;
   "charset",CHARSET;
   "check",CHECK;
   "collate",COLLATE;
   "column",COLUMN;
   "comment", COMMENT;
   "conflict",CONFLICT;
   "constraint",CONSTRAINT;
   "convert", CONVERT;
   "create",CREATE;
   "cross",CROSS;
   "current", CURRENT;
   "date",DATE;
   "day_hour", DAY_HOUR;
   "day_microsecond", DAY_MICROSECOND;
   "day_minute", DAY_MINUTE;
   "day_second", DAY_SECOND;
   "default",DEFAULT;
   "delayed", DELAYED;
   "delete",DELETE;
   "desc",DESC;
   "distinct",DISTINCT;
   "div", DIV;
   "drop",DROP;
   "duplicate", DUPLICATE;
   "else", ELSE;
   "end", END;
   "enum", ENUM;
   "escape",ESCAPE;
   "except",EXCEPT;
   "exists",EXISTS;
   "extract",EXTRACT;
   "false", FALSE;
   "first",FIRST;
   "first_value",FIRST_VALUE;
   "following", FOLLOWING;
   "for", FOR;
   "foreign",FOREIGN;
   "from",FROM;
   "fulltext",FULLTEXT;
   "function", FUNCTION;
   "global",GLOBAL;
   "group",GROUP;
   "group_concat", GROUP_CONCAT;
   "having",HAVING;
   "json_arrayagg", JSON_ARRAYAGG;
   "hour_microsecond", HOUR_MICROSECOND;
   "hour_minute", HOUR_MINUTE;
   "hour_second", HOUR_SECOND;
   "if",IF;
   "in",IN;
   "index",INDEX;
   "insert",INSERT;
   "intersect",INTERSECT;
   "interval", INTERVAL;
   "into",INTO;
   "is", IS;
   "join",JOIN;
   "straight_join",STRAIGHT_JOIN;
   "key",KEY;
   "lag", LAG;
   "language", LANGUAGE;
   "last_value", LAST_VALUE;
   "lead", LEAD;
   "like", LIKE;
   "limit",LIMIT;
   "local",LOCAL;
   "lock", LOCK;
   "shared", SHARED;
   "exclusive", EXCLUSIVE;
   "none", NONE;
   "minute_microsecond", MINUTE_MICROSECOND;
   "minute_second", MINUTE_SECOND;
   "mod", MOD;
   "mode", MODE;
   "modify", MODIFY;
   "national",NATIONAL;
   "natural",NATURAL;
   "no", NO;
   "nocache",NOCACHE;
   "not",NOT;
   "nothing", NOTHING;
   "nowait", NOWAIT;
   "null",NULL;
   "of", OF;
   "offset",OFFSET;
   "on",ON;
   "do",DO;
   "or",OR;
   "order",ORDER;
   "over",OVER;
   "partition",PARTITION;
   "preceding", PRECEDING;
   "precision",PRECISION;
   "primary",PRIMARY;
   "procedure", PROCEDURE;
   "range", RANGE;
   "references",REFERENCES;
   "rename",RENAME;
   "replace",REPLACE;
   "restrict",RESTRICT;
   "returns", RETURNS;
   "row", ROW;
   "rows", ROWS;
   "second_microsecond", SECOND_MICROSECOND;
   "select",SELECT;
   "set",SET;
   "separator", SEPARATOR;
   "share", SHARE;
   "some",SOME;
   "spatial", SPATIAL;
   "statement", STATEMENT;
   "substr", SUBSTRING;
   "substring", SUBSTRING;
   "table",TABLE;
   "temporary",TEMPORARY;
   "then", THEN;
   "time",TIME;
   "timestamp",TIMESTAMP;
   "to",TO;
   "true", TRUE;
   "unbounded", UNBOUNDED;
   "unicode",UNICODE;
   "union",UNION;
   "unique",UNIQUE;
   "unsigned",UNSIGNED;
   "update",UPDATE;
   "using",USING;
   "values",VALUES;
   "varying",VARYING;
   "view",VIEW;
   "when", WHEN;
   "where",WHERE;
   "with", WITH;
   "year_month", YEAR_MONTH;
   "generated", GENERATED;
   "always", ALWAYS;
   "virtual", VIRTUAL;
   "stored", STORED;
   "left", LEFT;
   "right", RIGHT;
   "full", FULL;
   "inner", INNER;
   "outer", OUTER;
   "instant", INSTANT;
   "inplace", INPLACE;
   "algorithm", ALGORITHM;
   "copy", COPY;
   "recursive", RECURSIVE;
   "ttl", TTL;
   "ttl_enable", TTL_ENABLE;
   "remove", REMOVE;
   "type", TYPE "type";
  ] in (* more *)
  k := !k @ List.map (fun s -> s, INTERVAL_UNIT s) [ "microsecond"; "second"; "minute"; "hour"; "day"; "week"; "month"; "quarter"; "year" ];
  let all token l = k := !k @ List.map (fun x -> x,token) l in
  all DATETIME_FUNC ["current_date";"current_timestamp";"current_time";"localtime";"localtimestamp";"now";];
  all DATETIME_FUNC ["getdate"]; (* mssql? *)
  all CONFLICT_ALGO ["ignore"; "abort"; "fail"; "rollback"];
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
  let open Sql in
  all (T_INTEGER None) ["integer";"int";"serial";"identity"];
  all (T_INTEGER (Some Tiny)) ["tinyint"];
  all (T_INTEGER (Some Small)) ["smallint"];
  all (T_INTEGER (Some Medium)) ["mediumint";"middleint"];
  all (T_INTEGER (Some Big)) ["bigint"];
  all T_DECIMAL ["numeric";"decimal";"dec";"fixed"];
  all (T_INTEGER None) ["number"]; (* oracle *)
  all T_BOOLEAN ["bool";"boolean"];
  all T_FLOAT ["float";"real";"float4";"float8";"int1";"int2";"int3";"int4";"int8"];
  all T_DOUBLE ["double"];
  all T_BLOB ["blob"];
  all T_TINYBLOB ["tinyblob"];
  all T_MEDIUMBLOB ["mediumblob"];
  all T_LONGBLOB ["longblob"];
  all T_VARBINARY ["varbinary"];
  all T_TEXT ["text"];
  all T_TINYTEXT ["tinytext"];
  all T_MEDIUMTEXT ["mediumtext"];
  all T_LONGTEXT ["longtext"];
  all T_CHAR ["char"];
  all T_VARCHAR ["varchar"];
  all T_VARCHAR2 ["varchar2"];
  all T_JSON ["json"];
  all T_DATETIME ["datetime"];
  all T_UUID ["uuid"]; (* http://www.postgresql.org/docs/9.4/static/datatype-uuid.html *)
  !k

(*
  Q: Why not convert all input to lowercase before lexing?
  A: Sometimes SQL is case-sensitive, also string contents should be preserved
*)

module Keywords = Map.Make(String)

let keywords =
  let add map (k, v) =
    let k = String.lowercase_ascii k in
    if Keywords.mem k map then failwith (sprintf "Lexeme %s is already associated with keyword." k)
    else Keywords.add k v map
  in
  List.fold_left add Keywords.empty keywords

let is_keyword =
  let tokens = Hashtbl.create (Keywords.cardinal keywords) in
  Keywords.iter (fun _ token -> Hashtbl.replace tokens token ()) keywords;
  fun token -> Hashtbl.mem tokens token

(* FIXME case sensitivity??! *)

let get_ident str =
  let str = String.lowercase_ascii str in
  match Keywords.find_opt str keywords with
  | Some token -> token
  | None -> IDENT str

let ident str = IDENT (String.lowercase_ascii str)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )*
let wsp = [' ' '\r' '\t']
let blank = [' ' '\n' '\r' '\t']

let cmnt = "--" | "//" | "#"
let line_comment = cmnt [^'\n']* '\n'?
let comment_body = ([^'*'] | '*'+ [^'*' '/'])*
let block_comment = "/*" comment_body '*'+ '/'
let open_comment = "/*" comment_body '*'*

let dq = ([^'"' '\n'] | "\"\"")*
let sq = ([^'\'' '\n'] | "''")*
let bq = ([^'`' '\n'] | "``")*
let sb = [^']' '\n']*
let open_string = '"' dq | "'" sq | '`' bq | '[' sb

let plain = [^ ';' '"' '\'' '`' '[' '$' '-' '/' '#' ' ' '\n' '\r' '\t']

(* extract separate statements *)
rule ruleStatement = parse
  | cmnt wsp* "[sqlgg]" wsp+ (ident+ as n) wsp* "=" wsp* ([^'\n']* as v) '\n' { `Props [(n, String.trim v)] }
  | cmnt wsp* "[sqlgg]" wsp+ (ident+ as n) wsp* '\n' { `Props [(n, "")] }
  | cmnt wsp* "@" (ident+ as name) wsp* "|" ([^'\n']* as props) '\n'?
    { match rulePropList [] (Lexing.from_string props) with
      | Some props -> `Props (("name", name) :: props)
      | None -> `Bad_props }
  | cmnt wsp* "@" (ident+ as name) [^'|' '\n']* '\n' { `Props [("name", name)] }
  | '"' dq '"' | "'" sq "'" | '`' bq '`' | '[' sb ']' { `Literal }
  | "$" (ident? as tag) "$" { if Option.is_some (ruleInDollarQuotes tag lexbuf) then `Literal else `Open_literal }
  | line_comment | block_comment { `Comment }
  | open_string | open_comment { `Open_literal }
  | ';' { `Semicolon }
  | blank+ { `Blank }
  | plain+ (blank+ plain+)* | [^ ';'] { `Text }
  | eof { `Eof }
and
rulePropList acc = parse
  | wsp* (ident as k) wsp* ':' wsp* ([^',']* as v) { rulePropListNext ((k, String.trim v) :: acc) lexbuf }
  | wsp* eof { Some (List.rev acc) }
  | _ { None }
and
rulePropListNext acc = parse
  | ',' { rulePropList acc lexbuf }
  | wsp* eof { Some (List.rev acc) }
  | _ { None }
(* extract tail of the input *)
and
ruleTail = parse
  | _* as tail { tail }
and
ruleMain = parse
  | blank { ruleMain lexbuf }

  | '('                { LPAREN }
  | ')'                { RPAREN }
  | ','   { COMMA }
  | '.'   { DOT }
  | '{'   { LCURLY (lexeme_start lexbuf) }
  | '}'   { RCURLY (lexeme_start lexbuf) }

  | line_comment | block_comment { ruleMain lexbuf }

  | "*" { ASTERISK }
  | "=" { EQUAL }
  | "!" { EXCL }
  | "~" { TILDE }
  | "||" { CONCAT_OP }
  | "+" { PLUS }
  | "-" { MINUS }

  | "/" | "%" { NUM_DIV_OP } (* FIXME: in PostgreSQL, "%" is both int modulo and a trigram comparison operator *)
  | "<<" | ">>" { NUM_BIT_SHIFT }
  | "|" { NUM_BIT_OR }
  | "&" { NUM_BIT_AND }
  | ">" | ">=" | "<=" | "<" { NUM_CMP_OP }
  | "<>" | "!=" | "==" { NUM_EQ_OP }
  | "<=>" { NOT_DISTINCT_OP }
  | "<%" | "%>" | "<<%" | "%>>" { TEXT_CMP_OP }
  | "<->" | "<<->" | "<->>" | "<<<->" | "<->>>" { TEXT_DIST_OP }

  | "->"  { JSON_EXTRACT_OP }
  | "->>" { JSON_UNQUOTE_EXTRACT_OP }

  | "?"   { QSTN }
  | "??"  { TWO_QSTN }
  | [':' '@'] (ident as str) { PARAM (Sql.make_located ~value:(Some str) ~pos:(pos lexbuf)) }
  | '&' (ident as value) { SHARED_QUERY_REF { value; pos = pos lexbuf } }
  | "::" { DOUBLECOLON }

  | '"' (dq as s) '"' { ident (unescape '"' s) }
  | '`' (bq as s) '`' { ident (unescape '`' s) }
  | '[' (sb as s) ']' { ident s }
  | "'" (sq as s) "'" { TEXT (unescape '\'' s) }
  | ['x' 'X'] "'" (sq as s) "'" { BLOB (unescape '\'' s) }
  (* http://www.postgresql.org/docs/current/interactive/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING *)
  | "$" (ident? as tag) "$" {
      let start_p = lexeme_start_p lexbuf and body = lexbuf.lex_curr_pos in
      match ruleInDollarQuotes tag lexbuf with
      | Some stop -> lexbuf.lex_start_p <- start_p; TEXT (sub_lexeme lexbuf body stop)
      | None -> raise (Error ("unterminated dollar quote", (start_p.pos_cnum, lexeme_end lexbuf)))
    }
  | open_string { error lexbuf "unterminated string literal" }
  | open_comment { error lexbuf "unterminated comment" }

  | ident as str { if !Parser_state.mode = Ident then IDENT str (* no keywords, preserve case *) else get_ident str }
  | digit+ as str { INTEGER (int_of_string str) }
  | digit+ '.' digit+ as str { FLOAT (float_of_string str) }
  | eof		{ EOF }
  | _	{ error lexbuf "unexpected character" }
and
ruleInDollarQuotes tag = parse
  | "$" (ident? as tag_) "$" { if String.equal tag_ tag then Some lexbuf.lex_start_pos else ruleInDollarQuotes tag lexbuf }
  | eof	        { None }
  | [^'$']+ | '$' { ruleInDollarQuotes tag lexbuf }

{

  let parse_rule lexbuf =
    let token = ruleMain lexbuf in
    match !Parser_state.mode with
    | Normal -> token
    | Ignore ->
(*         eprintf "ignored: %s\n" (lexeme lexbuf); *)
      if token = EOF then token else IGNORED
    | Ident -> token

}
