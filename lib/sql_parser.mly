/*
  Simple SQL parser
*/


%{
  open Printf
  open Sql.Constraint
  open Sql.Type
  open Stmt
  open Syntax
  open Prelude

  let params_of select = List.map (fun x -> `Param x) (snd select)

  let select_value select =
    let (s,_) = select in
    if (List.length s <> 1) then
      raise (RA.Schema.Error (s,"only one column allowed for SELECT operator in this expression"));
    params_of select

  let values_or_all table names =
    let schema = Tables.get_schema table in
    match names with
    | Some names -> RA.Schema.project names schema
    | None -> schema

  let update_tables tables ss w =
    let (tables,params) = List.split tables in
    let p1 = Syntax.params_of_assigns tables ss in
    let p2 = get_params_opt tables (Syntax.all_tbl_columns tables) w in
    (List.flatten params) @ p1 @ p2

  (* preserve order *)
  let limit l =
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
%token <Stmt.param_id> PARAM
%token <Sql.Type.t * bool> FUNCTION /* return type * is grouping function? */
%token LPAREN RPAREN COMMA EOF DOT NULL
%token CONFLICT_ALGO
%token SELECT INSERT OR INTO CREATE UPDATE VIEW TABLE VALUES WHERE ASTERISK DISTINCT ALL ANY SOME
       LIMIT ORDER BY DESC ASC EQUAL DELETE FROM DEFAULT OFFSET SET JOIN LIKE_OP LIKE
       EXCL TILDE NOT TEST_NULL BETWEEN AND ESCAPE USING UNION EXCEPT INTERSECT AS
       CONCAT_OP JOIN_TYPE1 JOIN_TYPE2 NATURAL CROSS REPLACE IN GROUP HAVING
       UNIQUE PRIMARY KEY FOREIGN AUTOINCREMENT ON CONFLICT TEMPORARY IF EXISTS
       PRECISION UNSIGNED ZEROFILL VARYING CHARSET NATIONAL ASCII UNICODE COLLATE BINARY CHARACTER
       DATETIME_FUNC DATE TIME TIMESTAMP ALTER ADD COLUMN CASCADE RESTRICT DROP
       GLOBAL LOCAL VALUE REFERENCES CHECK CONSTRAINT IGNORED AFTER INDEX FULLTEXT FIRST
       CASE WHEN THEN ELSE END CHANGE MODIFY DELAYED ENUM
%token NUM_DIV_OP NUM_BIT_OP NUM_EQ_OP NUM_CMP_OP PLUS MINUS
%token T_INTEGER T_BLOB T_TEXT T_FLOAT T_BOOLEAN T_DATETIME

(*
%left COMMA_JOIN
%left JOIN_JOIN
*)
(* FIXME precedence of COMMA and JOIN *)

%left TEST_NULL
%left OR
%left AND
%nonassoc EQUAL NUM_EQ_OP
%nonassoc NUM_CMP_OP
%nonassoc NUM_BIT_OP
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP
%left CONCAT_OP
%nonassoc UNARY_MINUS

%type <Syntax.expr> expr

%start <RA.Schema.t * Stmt.params * Stmt.kind> input

%%

input: statement EOF { $1 }

if_not_exists: IF NOT EXISTS { }
if_exists: IF EXISTS {}
temporary: either(GLOBAL,LOCAL)? TEMPORARY { }

statement: CREATE ioption(temporary) TABLE ioption(if_not_exists) name=IDENT schema=table_definition
              {
                Tables.add (name,schema);
                ([],[],Create name)
              }
         | ALTER TABLE name=table_name actions=commas(alter_action)
              {
                List.iter (function
                | `Add (col,pos) -> Tables.alter_add name col pos
                | `Drop col -> Tables.alter_drop name col
                | `Change (oldcol,col,pos) -> Tables.alter_change name oldcol col pos
                | `None -> ()) actions;
                ([],[],Alter name)
              }
         | DROP TABLE if_exists? name=IDENT
              {
                Tables.drop name;
                ([],[],Drop name)
              }
         | CREATE either(TABLE,VIEW) name=IDENT AS select=maybe_parenth(select_stmt)
              {
                let (s,p) = select in
                Tables.add (name,s);
                ([],p,Create name)
              }
         | CREATE UNIQUE? INDEX if_not_exists? name=table_name
                ON table=table_name cols=sequence(index_column)
              {
                RA.Schema.project cols (Tables.get_schema table) |> ignore; (* just check *)
                [],[],CreateIndex name
              }
         | select_stmt_t { $1 }
         | insert_cmd table=IDENT names=sequence(IDENT)? VALUES values=sequence(expr)?
              {
                let expect = values_or_all table names in
                let params, inferred = match values with
                | None -> [], Some (Values, expect)
                | Some values ->
                  let vl = List.length values in
                  let cl = List.length expect in
                  if vl <> cl then
                    failwith (sprintf "Expected %u expressions in VALUES list, %u provided" cl vl);
                  let assigns = List.combine (List.map (fun a -> a.RA.name, None) expect) values in
                  Syntax.params_of_assigns [Tables.get table] assigns, None
                in
                [], params, Insert (inferred,table)
              }
         | insert_cmd table=IDENT names=sequence(IDENT)? select=maybe_parenth(select_stmt)
              {
                let (schema,params) = select in
                let expect = values_or_all table names in
                ignore (RA.Schema.compound expect schema); (* test equal types *)
                [], params, Insert (None,table)
              }
         | insert_cmd table=IDENT SET ss=commas(set_column)?
              {
                let (params,inferred) = match ss with
                | None -> [], Some (Assign, Tables.get_schema table)
                | Some ss -> Syntax.params_of_assigns [Tables.get table] ss, None
                in
                [], params, Insert (inferred,table)
              }
         | update_cmd table=IDENT SET ss=commas(set_column) w=where? o=loption(order) lim=loption(limit)
              {
                let params = update_tables [Tables.get table,[]] ss w in
                let p3 = Syntax.params_of_order o [] [Tables.get table] in
                [], params @ p3 @ lim, Update (Some table)
              }
         /* http://dev.mysql.com/doc/refman/5.1/en/update.html multi-table syntax */
         | update_cmd tables=commas(source) SET ss=commas(set_column) w=where?
              {
                let params = update_tables tables ss w in
                [], params, Update None
              }
         | DELETE FROM table=IDENT w=where?
              {
                let t = Tables.get table in
                let p = get_params_opt [t] (snd t) w in
                [], p, Delete table
              }
         | SET IDENT EQUAL e=expr
              {
                let p = match e with
                  | `Column _ -> [] (* this is not column but some db-specific identifier *)
                  | _ -> get_params_q (ensure_simple_expr e)
                in
                [], p, Other
              }

table_name: name=IDENT | IDENT DOT name=IDENT { name } (* FIXME db name *)
index_column: name=IDENT collate? order_type? { name }

table_definition: t=sequence_(column_def1) table_def_done { list_filter_map (function `Attr a -> Some a | `Constraint _ -> None) t }
                | LIKE name=maybe_parenth(IDENT) { Tables.get name |> snd } (* mysql *)

(* ugly, can you fixme? *)
(* ignoring everything after RPAREN (NB one look-ahead token) *)
table_def_done: table_def_done1 RPAREN IGNORED* { Parser_state.mode_normal () }
table_def_done1: { Parser_state.mode_ignore () }

select_stmt_t: select_core other=list(preceded(compound_op,select_core))
               o=loption(order) lim=limit_t?
              {
                let (s1,p1,tbls,cardinality) = $1 in
                let (s2l,p2l) = List.split (List.map (fun (s,p,_,_) -> s,p) other) in
                if false then
                  eprintf "cardinality=%s other=%u\n%!"
                          (cardinality_to_string cardinality)
                          (List.length other);
                let cardinality = if other = [] then cardinality else `Nat in
                (* ignoring tables in compound statements - they cannot be used in ORDER BY *)
                let final_schema = List.fold_left RA.Schema.compound s1 s2l in
                let p3 = Syntax.params_of_order o final_schema tbls in
                let (p4,limit1) = match lim with | Some x -> x | None -> [],false in
(*                 RA.Schema.check_unique schema; *)
                let cardinality =
                  if limit1 && cardinality = `Nat then `Zero_one
                                                  else cardinality in
                final_schema,(p1@(List.flatten p2l)@p3@p4), Select cardinality
              }

select_stmt: select_stmt_t { let (s,p,_) = $1 in s,p }

select_core: SELECT select_type? r=commas(column1)
             f=from?
             w=where?
             g=loption(group)
             h=having?
              {
                let (tbls,p2,joined_schema) = match f with Some t -> Syntax.join t | None -> [], [], [] in
                let singlerow = g = [] && Syntax.test_all_grouping r in
                let singlerow2 = w = None && g = [] && Syntax.test_all_const r in
                let p1 = Syntax.params_of_columns tbls joined_schema r in
                let p3 = Syntax.get_params_opt tbls joined_schema w in
                let p4 = Syntax.get_params_l tbls joined_schema g in
                let p5 = Syntax.get_params_opt tbls joined_schema h in
                let cardinality = if singlerow then `One else
                                  if singlerow2 then `Zero_one else `Nat in
                (Syntax.infer_schema r tbls joined_schema, p1 @ p2 @ p3 @ p4 @ p5, tbls, cardinality)
              }

table_list: src=source joins=join_source* { (src,joins) }

join_source: NATURAL maybe_join_type JOIN src=source { src,`Natural }
           | CROSS JOIN src=source { src,`Cross }
           | qualified_join src=source cond=join_cond { src,cond }

qualified_join: COMMA | maybe_join_type JOIN { }

join_cond: ON e=expr { `Search e }
         | USING l=sequence(IDENT) { `Using l }
         | (* *) { `Default }

source1: IDENT { Tables.get $1,[] }
       | LPAREN s=select_core RPAREN { let (s,p,_,_) = s in ("",s),p }

source: src=source1 alias=maybe_as
    {
      match alias with
      | Some name -> let ((_,s),p) = src in ((name,s),p)
      | None -> src
    }

insert_cmd: INSERT DELAYED? OR? conflict_algo INTO | INSERT INTO | REPLACE INTO { }
update_cmd: UPDATE | UPDATE OR conflict_algo { }
conflict_algo: CONFLICT_ALGO | REPLACE { }

select_type: DISTINCT | ALL { }

int_or_param: i=INTEGER { `Const i }
            | p=PARAM { `Param p }

limit_t: LIMIT lim=int_or_param { limit [`Limit,lim] }
       | LIMIT ofs=int_or_param COMMA lim=int_or_param { limit [`Offset,ofs; `Limit,lim] }
       | LIMIT lim=int_or_param OFFSET ofs=int_or_param { limit [`Limit,lim; `Offset,ofs] }

limit: limit_t { fst $1 }

order: ORDER BY l=commas(terminated(expr,order_type?)) { l }
order_type: DESC | ASC { }

from: FROM t=table_list { t }
where: WHERE e=expr { e }
group: GROUP BY l=expr_list { l }
having: HAVING e=expr { e }

column1:
       | IDENT DOT ASTERISK { Syntax.AllOf $1 }
       | ASTERISK { Syntax.All }
       | e=expr m=maybe_as { Syntax.Expr (e,m) }

maybe_as: AS? name=IDENT { Some name }
        | { None }

maybe_parenth(X): x=X | LPAREN x=X RPAREN { x }

alter_action: ADD COLUMN? col=maybe_parenth(column_def) pos=alter_pos { `Add (col,pos) }
            | ADD index_type IDENT? sequence(IDENT) { `None }
            | DROP INDEX IDENT { `None }
            | DROP PRIMARY KEY { `None }
            | DROP COLUMN? col=IDENT drop_behavior? { `Drop col } (* FIXME behavior? *)
            | CHANGE COLUMN? old_name=IDENT column=column_def pos=alter_pos { `Change (old_name,column,pos) }
            | MODIFY COLUMN? column=column_def pos=alter_pos { `Change (column.RA.name,column,pos) }
            | SET IDENT IDENT { `None }
index_type: INDEX | FULLTEXT | PRIMARY KEY { }
alter_pos: AFTER col=IDENT { `After col }
         | FIRST { `First }
         | { `Default }
drop_behavior: CASCADE | RESTRICT { }

column_def: name=IDENT t=sql_type? column_def_extra*
    { RA.attr name (match t with Some x -> x | None -> Int) }

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
      | FOREIGN KEY IDENT? sequence(IDENT) REFERENCES IDENT sequence(IDENT)? { [] }
      | CHECK LPAREN expr RPAREN { [] }

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
      expr numeric_bin_op expr %prec PLUS { `Func ((Any,false),[$1;$3]) } (* TODO default Int *)
    | expr boolean_bin_op expr %prec AND { `Func ((Bool,false),[$1;$3]) }
    | e1=expr comparison_op anyall? e2=expr %prec EQUAL { `Func ((Bool,false),[e1;e2]) }
    | expr CONCAT_OP expr { `Func ((Text,false),[$1;$3]) }
    | e1=expr mnot(like) e2=expr e3=escape?
      { `Func ((Any,false),(list_filter_map identity [Some e1; Some e2; e3])) }
    | unary_op expr { $2 }
    | MINUS expr %prec UNARY_MINUS { $2 }
    | LPAREN expr RPAREN { $2 }
    | attr_name { `Column $1 }
    | v=literal_value | v=datetime_value { v }
    | e1=expr mnot(IN) l=sequence(expr) { `Func ((Any,false),e1::l) }
    | e1=expr mnot(IN) LPAREN select=select_stmt RPAREN
      {
        `Func ((Any,false),e1::select_value select)
      }
    | e1=expr IN table=IDENT { Tables.check(table); e1 }
    | LPAREN select=select_stmt RPAREN
      {
        `Func ((Any,false),select_value select)
      }
    | PARAM { `Param ($1,Any) }
    | f=FUNCTION LPAREN p=func_params RPAREN { `Func (f,p) }
    | expr TEST_NULL { $1 }
    | expr mnot(BETWEEN) expr AND expr { `Func ((Any,false),[$1;$3;$5]) } (* TODO default Int *)
    | mnot(EXISTS) LPAREN select=select_stmt RPAREN { `Func ((Bool,false),params_of select) }
    | CASE e1=expr? branches=nonempty_list(case_branch) e2=preceded(ELSE,expr)? END
      {
        let l = function None -> [] | Some x -> [x] in
        `Func ((Any,false),l e1 @ List.flatten branches @ l e2)
      }

case_branch: WHEN e1=expr THEN e2=expr { [e1;e2] }
like: LIKE | LIKE_OP { }

datetime_value: | DATETIME_FUNC | DATETIME_FUNC LPAREN INTEGER? RPAREN { `Value Datetime }

literal_value:
    | TEXT { `Value Text }
    | BLOB { `Value Blob }
    | INTEGER { `Value Int }
    | FLOAT { `Value Float }
    | DATE TEXT
    | TIME TEXT
    | TIMESTAMP TEXT { `Value Datetime }
    | NULL { `Value Any } (* he he *)

single_literal_value:
    | literal_value { $1 }
    | MINUS INTEGER { `Value Int }
    | MINUS FLOAT { `Value Float }

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

sql_type_flavor: T_INTEGER UNSIGNED? ZEROFILL? { Int }
               | binary { Blob }
               | NATIONAL? text VARYING? charset? collate? { Text }
               | ENUM sequence(TEXT) charset? collate? { Text }
               | T_FLOAT PRECISION? { Float }
               | T_BOOLEAN { Bool }
               | T_DATETIME | DATE | TIME | TIMESTAMP { Datetime }

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

