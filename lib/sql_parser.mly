/*
  Simple SQL parser
*/


%{
  open Sql
  open Sql.Type
  open Sql.Constraint
  open Syntax

  (* preserve order *)
  let make_limit l =
    let param = function
      | _, `Const _ -> None
      | x, `Param (None,pos) -> Some ((Some (match x with `Limit -> "limit" | `Offset -> "offset"),pos),Int)
      | _, `Param p -> Some (p,Int)
    in
    list_filter_map param l, List.mem (`Limit,`Const 1) l

%}

%token <int> INTEGER
%token <string> IDENT TEXT BLOB
%token <float> FLOAT
%token <Sql.param_id> PARAM
%token <Sql.Type.func> FUNCTION
%token LPAREN RPAREN COMMA EOF DOT NULL
%token CONFLICT_ALGO
%token SELECT INSERT OR INTO CREATE UPDATE VIEW TABLE VALUES WHERE ASTERISK DISTINCT ALL ANY SOME
       LIMIT ORDER BY DESC ASC EQUAL DELETE FROM DEFAULT OFFSET SET JOIN LIKE_OP LIKE
       EXCL TILDE NOT BETWEEN AND ESCAPE USING UNION EXCEPT INTERSECT AS
       CONCAT_OP JOIN_TYPE1 JOIN_TYPE2 NATURAL CROSS REPLACE IN GROUP HAVING
       UNIQUE PRIMARY KEY FOREIGN AUTOINCREMENT ON CONFLICT TEMPORARY IF EXISTS
       PRECISION UNSIGNED ZEROFILL VARYING CHARSET NATIONAL ASCII UNICODE COLLATE BINARY CHARACTER
       DATETIME_FUNC DATE TIME TIMESTAMP ALTER ADD COLUMN CASCADE RESTRICT DROP
       GLOBAL LOCAL VALUE REFERENCES CHECK CONSTRAINT IGNORED AFTER INDEX FULLTEXT FIRST
       CASE WHEN THEN ELSE END CHANGE MODIFY DELAYED ENUM FOR SHARE MODE LOCK
       OF WITH NOWAIT ACTION NO IS INTERVAL
%token MICROSECOND SECOND MINUTE HOUR DAY WEEK MONTH QUARTER YEAR
       SECOND_MICROSECOND MINUTE_MICROSECOND MINUTE_SECOND
       HOUR_MICROSECOND HOUR_SECOND HOUR_MINUTE
       DAY_MICROSECOND DAY_SECOND DAY_MINUTE DAY_HOUR
       YEAR_MONTH
%token NUM_DIV_OP NUM_BIT_OP NUM_EQ_OP NUM_CMP_OP PLUS MINUS
%token T_INTEGER T_BLOB T_TEXT T_FLOAT T_BOOLEAN T_DATETIME

(*
%left COMMA_JOIN
%left JOIN_JOIN
*)
(* FIXME precedence of COMMA and JOIN *)

%left OR
%left AND
%nonassoc EQUAL NUM_EQ_OP
%nonassoc NUM_CMP_OP
%nonassoc NUM_BIT_OP
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP
%left CONCAT_OP
%nonassoc UNARY_MINUS

%type <Sql.expr> expr

%start <Sql.stmt> input

%%

input: statement EOF { $1 }

if_not_exists: IF NOT EXISTS { }
if_exists: IF EXISTS {}
temporary: either(GLOBAL,LOCAL)? TEMPORARY { }

statement: CREATE ioption(temporary) TABLE ioption(if_not_exists) name=IDENT schema=table_definition
              {
                Create (name,`Schema schema)
              }
         | CREATE either(TABLE,VIEW) name=IDENT AS select=maybe_parenth(select_stmt)
              {
                Create (name,`Select select)
              }
         | ALTER TABLE name=table_name actions=commas(alter_action)
              {
                Alter (name,actions)
              }
         | DROP TABLE if_exists? name=IDENT
              {
                Drop name
              }
         | CREATE UNIQUE? INDEX if_not_exists? name=table_name ON table=table_name cols=sequence(index_column)
              {
                CreateIndex (name, table, cols)
              }
         | select_stmt { Select $1 }
         | insert_cmd table=IDENT names=sequence(IDENT)? VALUES values=sequence(expr)?
              {
                Insert (table,`Values (names, values))
              }
         | insert_cmd table=IDENT names=sequence(IDENT)? select=maybe_parenth(select_stmt)
              {
                Insert (table,`Select (names, select))
              }
         | insert_cmd table=IDENT SET ss=commas(set_column)?
              {
                Insert (table, `Set ss)
              }
         | update_cmd table=IDENT SET ss=commas(set_column) w=where? o=loption(order) lim=loption(limit)
              {
                Update (table,ss,w,o,lim)
              }
         /* http://dev.mysql.com/doc/refman/5.1/en/update.html multi-table syntax */
         | update_cmd tables=commas(source) SET ss=commas(set_column) w=where?
              {
                UpdateMulti (tables,ss,w)
              }
         | DELETE FROM table=IDENT w=where?
              {
                Delete (table,w)
              }
         | SET name=IDENT EQUAL e=expr
              {
                Set (name, e)
              }

table_name: name=IDENT | IDENT DOT name=IDENT { name } (* FIXME db name *)
index_prefix: LPAREN n=INTEGER RPAREN { n }
index_column: name=IDENT index_prefix? collate? order_type? { name }

table_definition: t=sequence_(column_def1) table_def_done { list_filter_map (function `Attr a -> Some a | `Constraint _ -> None) t }
                | LIKE name=maybe_parenth(IDENT) { Tables.get name |> snd } (* mysql *)

(* ugly, can you fixme? *)
(* ignoring everything after RPAREN (NB one look-ahead token) *)
table_def_done: table_def_done1 RPAREN IGNORED* { Parser_state.mode_normal () }
table_def_done1: { Parser_state.mode_ignore () }

select_stmt: select_core other=list(preceded(compound_op,select_core)) o=loption(order) lim=limit_t? select_row_locking?
              {
                ($1, other, o, lim)
              }

select_core: SELECT select_type? r=commas(column1) f=from?  w=where?  g=loption(group) h=having?
              {
                { columns=r; from=f; where=w; group=g; having=h; }
              }

table_list: src=source joins=join_source* { (src,joins) }

join_source: NATURAL maybe_join_type JOIN src=source { src,`Natural }
           | CROSS JOIN src=source { src,`Cross }
           | qualified_join src=source cond=join_cond { src,cond }

qualified_join: COMMA | maybe_join_type JOIN { }

join_cond: ON e=expr { `Search e }
         | USING l=sequence(IDENT) { `Using l }
         | (* *) { `Default }

source1: IDENT { `Table $1 }
       | LPAREN s=select_core RPAREN { `Select s }

source: src=source1 alias=maybe_as { src, alias }

insert_cmd: INSERT DELAYED? OR? conflict_algo INTO | INSERT INTO | REPLACE INTO { }
update_cmd: UPDATE | UPDATE OR conflict_algo { }
conflict_algo: CONFLICT_ALGO | REPLACE { }

select_type: DISTINCT | ALL { }

select_row_locking: 
    for_update_or_share+
      { }
  | LOCK IN SHARE MODE
      { }

for_update_or_share:
  FOR either(UPDATE, SHARE) update_or_share_of? NOWAIT? with_lock? { }

update_or_share_of: OF commas(IDENT) { }

with_lock: WITH LOCK { }

int_or_param: i=INTEGER { `Const i }
            | p=PARAM { `Param p }

limit_t: LIMIT lim=int_or_param { make_limit [`Limit,lim] }
       | LIMIT ofs=int_or_param COMMA lim=int_or_param { make_limit [`Offset,ofs; `Limit,lim] }
       | LIMIT lim=int_or_param OFFSET ofs=int_or_param { make_limit [`Limit,lim; `Offset,ofs] }

limit: limit_t { fst $1 }

order: ORDER BY l=commas(terminated(expr,order_type?)) { l }
order_type: DESC | ASC { }

from: FROM t=table_list { t }
where: WHERE e=expr { e }
group: GROUP BY l=expr_list { l }
having: HAVING e=expr { e }

column1:
       | IDENT DOT ASTERISK { Sql.AllOf $1 }
       | ASTERISK { Sql.All }
       | e=expr m=maybe_as { Sql.Expr (e,m) }

maybe_as: AS? name=IDENT { Some name }
        | { None }

maybe_parenth(X): x=X | LPAREN x=X RPAREN { x }

alter_action: ADD COLUMN? col=maybe_parenth(column_def) pos=alter_pos { `Add (col,pos) }
            | ADD index_type IDENT? sequence(IDENT) { `None }
            | DROP INDEX IDENT { `None }
            | DROP PRIMARY KEY { `None }
            | DROP COLUMN? col=IDENT drop_behavior? { `Drop col } (* FIXME behavior? *)
            | CHANGE COLUMN? old_name=IDENT column=column_def pos=alter_pos { `Change (old_name,column,pos) }
            | MODIFY COLUMN? column=column_def pos=alter_pos { `Change (column.name,column,pos) }
            | SET IDENT IDENT { `None }
index_type: INDEX | FULLTEXT | PRIMARY KEY { }
alter_pos: AFTER col=IDENT { `After col }
         | FIRST { `First }
         | { `Default }
drop_behavior: CASCADE | RESTRICT { }

column_def: name=IDENT t=sql_type? column_def_extra*
    { attr name (match t with Some x -> x | None -> Int) }

column_def1: c=column_def { `Attr c }
           | pair(CONSTRAINT,IDENT)? c=table_constraint_1 { `Constraint c }

on_conflict: ON CONFLICT algo=conflict_algo { algo }
column_def_extra: PRIMARY KEY { Some PrimaryKey }
                | NOT NULL { Some NotNull }
                | NULL { None }
                | UNIQUE { Some Unique }
                | AUTOINCREMENT { Some Autoincrement }
                | on_conflict { None }
                | CHECK LPAREN expr RPAREN { None }
                | DEFAULT default_value { None } (* FIXME check type with column *)
                | COLLATE IDENT { None }

default_value: single_literal_value | datetime_value { } (* sub expr ? *)

(* FIXME check columns *)
table_constraint_1:
      | some_key IDENT? key_arg { [] }
      | FOREIGN KEY IDENT? sequence(IDENT) REFERENCES IDENT sequence(IDENT)?
        reference_action_clause*
          { [] }
      | CHECK LPAREN expr RPAREN { [] }

reference_action_clause:
  ON either(DELETE, UPDATE) reference_action { }

reference_action:
  RESTRICT | CASCADE | SET NULL | NO ACTION | SET DEFAULT { }

some_key: UNIQUE KEY? | PRIMARY? KEY | FULLTEXT KEY { }
key_arg: LPAREN VALUE RPAREN | sequence(IDENT) { }

set_column: name=attr_name EQUAL e=expr { name,e }

(* expr: expr1 { $1 |> Syntax.expr_to_string |> prerr_endline; $1 } *)

anyall: ANY | ALL | SOME { }

mnot(X): NOT x = X | x = X { x }

attr_name: name=IDENT { (name,None) }
         | table=IDENT DOT name=IDENT
         | IDENT DOT table=IDENT DOT name=IDENT { (name,Some table) } (* FIXME database identifier *)

expr:
      expr numeric_bin_op expr %prec PLUS { Fun ((Ret Any),[$1;$3]) } (* TODO default Int *)
    | expr boolean_bin_op expr %prec AND { Fun ((Func (Bool,[Bool;Bool])),[$1;$3]) }
    | e1=expr comparison_op anyall? e2=expr %prec EQUAL { Fun ((Poly Bool),[e1;e2]) }
    | expr CONCAT_OP expr { Fun ((Func (Text,[Text;Text])),[$1;$3]) }
    | e1=expr mnot(like) e2=expr e3=escape?
      {
        match e3 with
        | None -> Fun ((Func (Bool, [Text; Text])), [e1;e2])
        | Some e3 -> Fun ((Func (Bool, [Text; Text; Text])), [e1;e2;e3])
      }
    | unary_op expr { $2 }
    | MINUS expr %prec UNARY_MINUS { $2 }
    | INTERVAL expr interval_unit { $2 }
    | LPAREN expr RPAREN { $2 }
    | attr_name { Column $1 }
    | v=literal_value | v=datetime_value { v }
    | e1=expr mnot(IN) l=sequence(expr) { Fun ((Poly Bool),e1::l) }
    | e1=expr mnot(IN) LPAREN select=select_stmt RPAREN
      {
        Fun ((Poly Bool),[e1; Select (select, true)])
      }
    | e1=expr IN table=IDENT { Tables.check table; e1 }
    | LPAREN select=select_stmt RPAREN { Select (select, true) }
    | PARAM { Param ($1,Any) }
    | f=FUNCTION LPAREN p=func_params RPAREN { Fun (f,p) }
    | expr IS NOT? NULL { Fun (Ret Bool, [$1]) }
    | expr mnot(BETWEEN) expr AND expr { Fun ((Poly Bool),[$1;$3;$5]) }
    | mnot(EXISTS) LPAREN select=select_stmt RPAREN { Fun ((Ret Bool),[Select (select,false)]) } (* FIXME Poly Bool *)
    | CASE e1=expr? branches=nonempty_list(case_branch) e2=preceded(ELSE,expr)? END (* FIXME typing *)
      {
        let l = function None -> [] | Some x -> [x] in
        Fun ((Ret Any),l e1 @ List.flatten branches @ l e2)
      }

case_branch: WHEN e1=expr THEN e2=expr { [e1;e2] }
like: LIKE | LIKE_OP { }

datetime_value: | DATETIME_FUNC | DATETIME_FUNC LPAREN INTEGER? RPAREN { Value Datetime }

literal_value:
    | TEXT { Value Text }
    | BLOB { Value Blob }
    | INTEGER { Value Int }
    | FLOAT { Value Float }
    | DATE TEXT
    | TIME TEXT
    | TIMESTAMP TEXT { Value Datetime }
    | NULL { Value Any } (* he he *)

single_literal_value:
    | literal_value { $1 }
    | MINUS INTEGER { Value Int }
    | MINUS FLOAT { Value Float }

expr_list: l=commas(expr) { l }
func_params: expr_list { $1 }
           | ASTERISK { [] }
           | (* *) { [] }
escape: ESCAPE expr { $2 }
numeric_bin_op: PLUS | MINUS | ASTERISK | NUM_DIV_OP | NUM_BIT_OP { }
comparison_op: EQUAL | NUM_CMP_OP | NUM_EQ_OP { }
boolean_bin_op: AND | OR { }

unary_op: EXCL { }
        | TILDE { }
        | NOT { }

interval_unit: MICROSECOND | SECOND | MINUTE | HOUR | DAY | WEEK | MONTH | QUARTER | YEAR
             | SECOND_MICROSECOND | MINUTE_MICROSECOND | MINUTE_SECOND
             | HOUR_MICROSECOND | HOUR_SECOND | HOUR_MINUTE
             | DAY_MICROSECOND | DAY_SECOND | DAY_MINUTE | DAY_HOUR
             | YEAR_MONTH { }

sql_type_flavor: T_INTEGER UNSIGNED? ZEROFILL? { Int }
               | binary { Blob }
               | NATIONAL? text VARYING? charset? collate? { Text }
               | ENUM sequence(TEXT) charset? collate? { Text }
               | T_FLOAT PRECISION? { Float }
               | T_BOOLEAN { Bool }
               | T_DATETIME | YEAR | DATE | TIME | TIMESTAMP { Datetime }

binary: T_BLOB | BINARY | BINARY VARYING { }
text: T_TEXT | T_TEXT LPAREN INTEGER RPAREN | CHARACTER { }

%inline either(X,Y): X | Y { }
%inline commas(X): l=separated_nonempty_list(COMMA,X) { l }
(* (x1,x2,...,xn) *)
%inline sequence_(X): LPAREN l=commas(X) { l }
%inline sequence(X): l=sequence_(X) RPAREN { l }

charset: CHARSET either(IDENT,BINARY) | CHARACTER SET either(IDENT,BINARY) | ASCII | UNICODE { }
collate: COLLATE IDENT { }

sql_type: t=sql_type_flavor
        | t=sql_type_flavor LPAREN INTEGER RPAREN UNSIGNED?
        | t=sql_type_flavor LPAREN INTEGER COMMA INTEGER RPAREN
        { t }

compound_op: UNION ALL? | EXCEPT | INTERSECT { }

maybe_join_type: JOIN_TYPE1? JOIN_TYPE2? { }

