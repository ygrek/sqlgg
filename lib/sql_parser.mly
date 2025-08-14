/*
  Simple SQL parser
*/


%{
  open Sql
  open Sql.Type
  open Sql.Constraint
  open ExtLib

  module Dialect_feature = Parser_state.Dialect_feature

  (* preserve order *)
  let make_limit l =
    let param = function
      | _, `Const _ -> None
      | x, `Param { label=None; pos } -> Some (new_param { label = Some (match x with `Limit -> "limit" | `Offset -> "offset"); pos } (strict Int))
      | _, `Param id -> Some (new_param id (strict Int))
    in
    List.filter_map param l, List.mem (`Limit,`Const 1) l

  let poly ret parameters = Fun { kind = (F (Typ ret, List.map (fun _ -> Var 0) parameters)); parameters; is_over_clause = false }
%}

%token <int> INTEGER
%token <string> IDENT TEXT BLOB
%token <float> FLOAT
%token <Sql.param_id> PARAM
%token <Sql.shared_query_ref_id> SHARED_QUERY_REF
%token <int> LCURLY RCURLY
%token <string> INTERVAL_UNIT (* interval unit keyword or function *)
%token LPAREN RPAREN COMMA EOF DOT NULL
%token CONFLICT_ALGO
%token SELECT INSERT OR INTO CREATE UPDATE VIEW TABLE VALUES WHERE ASTERISK DISTINCT ALL ANY SOME
       LIMIT ORDER BY DESC ASC EQUAL DELETE FROM DEFAULT OFFSET SET STRAIGHT_JOIN JOIN LIKE_OP LIKE
       EXCL TILDE NOT BETWEEN AND XOR ESCAPE USING UNION EXCEPT INTERSECT AS TO
       CONCAT_OP LEFT RIGHT FULL INNER OUTER NATURAL CROSS REPLACE IN GROUP HAVING
       UNIQUE PRIMARY KEY FOREIGN AUTOINCREMENT ON CONFLICT DO TEMPORARY IF EXISTS
       PRECISION UNSIGNED ZEROFILL VARYING CHARSET NATIONAL ASCII UNICODE COLLATE BINARY CHARACTER
       DATETIME_FUNC DATE TIME TIMESTAMP ALTER RENAME ADD COLUMN CASCADE RESTRICT DROP
       GLOBAL LOCAL REFERENCES CHECK CONSTRAINT IGNORED AFTER INDEX FULLTEXT SPATIAL FIRST
       CASE WHEN THEN ELSE END CHANGE MODIFY DELAYED ENUM FOR SHARE MODE LOCK
       OF WITH NOWAIT ACTION NO IS INTERVAL SUBSTRING DIV MOD CONVERT LAG LEAD OVER
       FIRST_VALUE LAST_VALUE NTH_VALUE PARTITION ROWS RANGE UNBOUNDED PRECEDING FOLLOWING CURRENT ROW
       CAST GENERATED ALWAYS VIRTUAL STORED STATEMENT DOUBLECOLON QSTN TWO_QSTN INSTANT INPLACE COPY ALGORITHM RECURSIVE
       SHARED EXCLUSIVE NONE
%token FUNCTION PROCEDURE LANGUAGE RETURNS OUT INOUT BEGIN COMMENT
%token SECOND_MICROSECOND MINUTE_MICROSECOND MINUTE_SECOND
       HOUR_MICROSECOND HOUR_SECOND HOUR_MINUTE
       DAY_MICROSECOND DAY_SECOND DAY_MINUTE DAY_HOUR EXTRACT
       YEAR_MONTH FALSE TRUE DUPLICATE
%token NUM_DIV_OP NUM_EQ_OP NUM_CMP_OP PLUS MINUS NOT_DISTINCT_OP NUM_BIT_SHIFT NUM_BIT_OR NUM_BIT_AND
%token T_INTEGER T_BLOB T_TEXT T_FLOAT T_BOOLEAN T_DATETIME T_UUID T_DECIMAL T_JSON

(*
%left COMMA_JOIN
%left JOIN_JOIN
*)
(* FIXME precedence of COMMA and JOIN *)

(* https://dev.mysql.com/doc/refman/8.0/en/operator-precedence.html *)

%left OR CONCAT_OP
%left XOR
%left AND
%nonassoc NOT
%nonassoc BETWEEN CASE (* WHEN THEN ELSE *) (* never useful *)
%nonassoc EQUAL NUM_EQ_OP NOT_DISTINCT_OP IS LIKE LIKE_OP IN
%nonassoc NUM_CMP_OP
%left NUM_BIT_OR
%left NUM_BIT_AND
%left NUM_BIT_SHIFT
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP MOD DIV
(* ^ *)
%nonassoc UNARY_MINUS TILDE
%nonassoc EXCL
(* Warning: the precedence level assigned to BINARY is never useful. *)
(* %nonassoc BINARY COLLATE *)
%nonassoc INTERVAL

%type <Sql.expr> expr

%start <Sql.stmt> input

%%

input: statement EOF { $1 }

param: 
  | QSTN { { label=None; pos = ($startofs, $endofs) } }
  | PARAM  { $1 }

if_not_exists: IF NOT EXISTS { }
if_exists: IF EXISTS {}
temporary: either(GLOBAL,LOCAL)? TEMPORARY { }
assign: name=IDENT EQUAL e=expr { name, e }


cte_item: | cte_name=IDENT names=maybe_parenth(sequence(IDENT))? AS LPAREN stmt=select_stmt_plain RPAREN
            {
              let cols = Option.map (List.map (fun name -> make_attribute' name (depends Any))) names in
              { cte_name; cols; stmt = CteInline stmt }
            }
          | cte_name=IDENT names=maybe_parenth(sequence(IDENT))? AS shared_query_ref_id=SHARED_QUERY_REF
            {
              let cols = Option.map (List.map (fun name -> make_attribute' name (depends Any))) names in
              { cte_name; cols; stmt = CteSharedQuery shared_query_ref_id; }
            }
cte: is_recursive=cte_with cte_items=commas(cte_item) {{ cte_items; is_recursive }}

statement: CREATE ioption(temporary) TABLE ioption(if_not_exists) name=table_name schema=table_definition
              {
                Create (name,`Schema schema)
              }
         | CREATE either(TABLE,VIEW) name=table_name AS select=maybe_parenth(select_stmt)
              {
                Dialect_feature.set_create_table_as_select ($startofs, $endofs);
                Create (name,`Select select)
              }
         | ALTER TABLE name=table_name actions=commas(alter_action)
              {
                Alter (name,actions)
              }
         | RENAME TABLE l=separated_nonempty_list(COMMA, separated_pair(table_name,TO,table_name)) { Rename l }
         | DROP either(TABLE,VIEW) if_exists? name=table_name
              {
                Drop name
              }
         | CREATE UNIQUE? INDEX if_not_exists? name=IDENT ON table=table_name cols=sequence(index_column)
              {
                CreateIndex (name, table, cols)
              }
         | select_stmt { Select $1 }
         | insert_cmd target=table_name names=sequence(IDENT)? VALUES values=commas(sequence(set_column_expr))? ss=conflict_clause?
              {
                Insert { target; action=`Values (names, values); on_conflict_clause=ss; }
              }
         | insert_cmd target=table_name names=sequence(IDENT)? VALUES p=param ss=conflict_clause?
              {
                Insert { target; action=`Param (names, p); on_conflict_clause=ss; }
              }
         | insert_cmd target=table_name names=sequence(IDENT)? select=maybe_parenth(select_stmt) ss=conflict_clause?
              {
                Insert { target; action=`Select (names, select); on_conflict_clause=ss; }
              }
         | insert_cmd target=table_name SET set=commas(set_column)? ss=conflict_clause?
              {
                Insert { target; action=`Set set; on_conflict_clause=ss; }
              }
         | update_cmd table=table_name SET ss=commas(set_column) w=where? o=loption(order) lim=loption(limit)
              {
                Update (table,ss,w,o,lim)
              }
         /* http://dev.mysql.com/doc/refman/5.1/en/update.html multi-table syntax */
         | update_cmd tables=commas(table_list) SET ss=commas(set_column) w=where?
              {
                UpdateMulti (tables,ss,w)
              }
         | DELETE FROM table=table_name w=where?
              {
                Delete (table,w)
              }
         /* https://dev.mysql.com/doc/refman/5.7/en/delete.html multi-table syntax */
         | DELETE targets=commas(table_name) FROM tables=table_list w=where?
              {
                DeleteMulti (targets, tables, w)
              }
         | SET kv=assign
              {
                Set ([kv], None)
              }
         | SET STATEMENT vars=separated_nonempty_list(COMMA, assign) FOR stmt=statement { Set (vars, Some stmt) }
         | CREATE or_replace? FUNCTION name=table_name params=sequence(func_parameter)
           RETURNS ret=sql_type
           routine_extra?
           AS? routine_body
           routine_extra?
              {
                Function.add (List.length params) (Ret (depends ret)) name.tn; (* FIXME store function namespace *)
                CreateRoutine (name, Some ret, params)
              }
         | CREATE or_replace? PROCEDURE name=table_name params=sequence(proc_parameter)
           routine_extra?
           AS? routine_body
           routine_extra?
              {
                Function.add (List.length params) (Ret (depends Any)) name.tn; (* FIXME void *)
                CreateRoutine (name, None, params)
              }   

parameter_default_: DEFAULT | EQUAL { }
parameter_default: parameter_default_ e=expr { e }
func_parameter: n=IDENT AS? t=sql_type e=parameter_default? { (n,t,e) }
parameter_mode: IN | OUT | INOUT { }
proc_parameter: parameter_mode? p=func_parameter { p }

or_replace: OR REPLACE { }

routine_body: TEXT | compound_stmt { }
compound_stmt: BEGIN statement+ END { } (* mysql *)

routine_extra: LANGUAGE IDENT { }
             | COMMENT TEXT { }

%inline table_name: name=IDENT { Sql.make_table_name name }
                  | db=IDENT DOT name=IDENT { Sql.make_table_name ~db name }
index_prefix: LPAREN n=INTEGER RPAREN { n }
index_column: name=IDENT index_prefix? collate? order_type? { name }

table_definition: t=sequence_(column_def1) ignore_after(RPAREN) 
                      { 
                        List.fold_right
                          (fun x (attrs, constraints) -> match x with
                          | `Attr a -> a::attrs, constraints
                          | `Constraint c -> attrs, c::constraints
                          | `Index _ -> attrs, constraints)
                          t ([], [])
                      }
                | LIKE name=maybe_parenth(table_name) { Tables.get name |> snd |> fun attrs -> (attrs, []) } (* mysql *)

(* ignoring everything after given token with a "lexer hack" (NB one look-ahead token) *)
ignore_after(X): parser_state_ignore X IGNORED* parser_state_normal { }

parser_state_ignore: { Parser_state.mode_ignore () }
parser_state_normal: { Parser_state.mode_normal () }
parser_state_ident: { Parser_state.mode_ident () }

cte_with: WITH { false } | WITH RECURSIVE { true }

select_stmt: cte=cte? select_complete=select_stmt_plain
              {
                { select_complete; cte; }
              }

select_stmt_plain: core=select_core other=list(pair(compound_op,select_core)) o=loption(order) lim=limit_t? select_row_locking?
              {
                { select = (core, other); order=o; limit=lim; }
              }

select_core: SELECT select_type? r=commas(column1) f=from?  w=where?  g=loption(group) h=having?
              {
                { columns=r; from=f; where=w; group=g; having=h; }
              }

table_list: src=source joins=join_source* { (src,joins) }

anyorder(X,Y): x=X y=Y | y=Y x=X { x,y }
inner_join: either(CROSS,INNER)? { Schema.Join.Inner }
left_join: anyorder(LEFT,OUTER?) { Schema.Join.Left }
right_join: anyorder(RIGHT,OUTER?) { Schema.Join.Right }
full_join: anyorder(FULL,OUTER?) { Schema.Join.Full }
straight_join: STRAIGHT_JOIN { Schema.Join.Inner }
natural(join): j=anyorder(NATURAL,join) JOIN src=source { src, snd j, Schema.Join.Natural }
cond(join): j=join JOIN src=source c=join_cond { Dialect_feature.set_join_source src ($startofs, $endofs); src, j, c }
straight_cond(join): j=join src=source c=join_cond { src, j, c }

join_source: COMMA src=source c=join_cond { src, Schema.Join.Inner, c }
           | j=natural(left_join)
           | j=natural(right_join)
           | j=natural(full_join)
           | j=natural(inner_join)
           | j=cond(left_join)
           | j=cond(right_join)
           | j=cond(full_join)
           | j=cond(inner_join) { j }
           | j=straight_cond(straight_join) { j }

join_cond: ON e=expr { On e }
         | USING l=sequence(IDENT) { Using l }
         | (* *) { Default }

source1: table_name { `Table $1 }
       | LPAREN s=select_stmt RPAREN { `Select s }
       | LPAREN s=table_list RPAREN { `Nested s }
       | LPAREN s=values_stmt RPAREN { `ValueRows s }

source: src=source1 alias=maybe_as_with_detupled? { 
  src, 
  Option.map (fun (tbl, cols) -> 
    let column_aliases = Option.map (List.map (fun name -> make_attribute' name (depends Any))) cols in
    { table_name = Sql.make_table_name tbl; column_aliases; }
  ) alias 
}

insert_cmd: INSERT DELAYED? OR? conflict_algo INTO | INSERT INTO | REPLACE INTO { }
update_cmd: UPDATE | UPDATE OR conflict_algo { }
conflict_algo: CONFLICT_ALGO | REPLACE { }

conflict_clause: 
  | ON DUPLICATE KEY UPDATE ss=commas(set_column) 
    { Dialect_feature.set_on_duplicate_key ($startofs, $endofs); On_duplicate, ss }
  | ON CONFLICT LPAREN attrs=separated_nonempty_list(COMMA, attr_name) RPAREN DO UPDATE SET ss=commas(set_column) 
    { Dialect_feature.set_on_conflict ($startofs, $endofs); On_conflict attrs, ss }

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
            | p=param { `Param p }

limit_t: LIMIT lim=int_or_param { make_limit [`Limit,lim] }
       | LIMIT ofs=int_or_param COMMA lim=int_or_param { make_limit [`Offset,ofs; `Limit,lim] }
       | LIMIT lim=int_or_param OFFSET ofs=int_or_param { make_limit [`Limit,lim; `Offset,ofs] }

limit: limit_t { fst $1 }

order: ORDER BY l=commas(pair(expr,order_type?)) { l }
order_type:
          | DESC | ASC { `Fixed }
          | param { `Param $1 }

from: FROM t=table_list { t }
where: WHERE e=expr { e }
group: GROUP BY l=expr_list { l }
having: HAVING e=expr { e }

column1:
       | table_name DOT ASTERISK { Sql.AllOf $1 }
       | ASTERISK { Sql.All }
       | e=expr m=maybe_as { Sql.Expr (e,m) }

maybe_as: AS? name=IDENT { Some name }
        | { None }

maybe_as_with_detupled: AS? name=IDENT names=sequence(IDENT)? { name, names }

maybe_parenth(X): x=X | LPAREN x=X RPAREN { x }

alter_action: ADD COLUMN? col=maybe_parenth(column_def) pos=alter_pos { `Add (col,pos) }
            | ADD index_type IDENT? sequence(IDENT) { `None }
            | ADD pair(CONSTRAINT,IDENT?)? table_constraint_1 index_options { `None }
            | RENAME either(TO,AS)? new_name=table_name { `RenameTable new_name }
            | RENAME COLUMN old_name=IDENT TO new_name=IDENT { `RenameColumn (old_name, new_name) }
            | RENAME index_or_key old_name=IDENT TO new_name=IDENT { `RenameIndex (old_name, new_name) }
            | DROP INDEX IDENT { `None }
            | DROP PRIMARY KEY { `None }
            | DROP COLUMN? col=IDENT drop_behavior? { `Drop col } (* FIXME behavior? *)
            | DROP FOREIGN KEY IDENT { `None }
            | DROP CHECK IDENT { `None }
            | CHANGE COLUMN? old_name=IDENT column=column_def pos=alter_pos { `Change (old_name,column,pos) }
            | MODIFY COLUMN? column=column_def pos=alter_pos { `Change (column.name,column,pos) }
            | SET IDENT IDENT { `None }
            | ALGORITHM EQUAL algorithm { `None }
            | LOCK EQUAL lock { `None }
            | either(DEFAULT,pair(CONVERT,TO))? charset collate? { `None }
index_or_key: INDEX | KEY { }
index_type: index_or_key | UNIQUE index_or_key? | either(FULLTEXT,SPATIAL) index_or_key? | PRIMARY KEY { }
alter_pos: AFTER col=IDENT { `After col }
         | FIRST { `First }
         | { `Default }
drop_behavior: CASCADE | RESTRICT { }

column_def: name=IDENT t=sql_type? extra=column_def_extra*
  {
    let rule_start_pos_cnum = $startpos.Lexing.pos_cnum in
    let meta = List.concat @@ Parser_state.Stmt_metadata.find_all rule_start_pos_cnum in
    let extra = Constraints.of_list @@ List.filter_map identity extra in
    make_attribute name t extra ~meta
  }

column_def1: c=column_def { `Attr c }
           | pair(CONSTRAINT,IDENT?)? l=table_constraint_1 index_options { `Constraint l }
           | index_or_key l=table_index { `Index l }
           | either(FULLTEXT,SPATIAL) index_or_key? l=table_index { `Index l }

key_part: n=IDENT delimited(LPAREN,INTEGER,RPAREN)? either(ASC,DESC)? { n }
index_options: list(IDENT)? { }

table_index: IDENT? l=sequence(key_part) index_options { l }

(* FIXME check columns *)
table_constraint_1:
      | PRIMARY KEY l=sequence(key_part) { `Primary l }
      | UNIQUE index_or_key? IDENT? l=sequence(key_part) { `Unique l }
      | FOREIGN KEY IDENT? sequence(IDENT) REFERENCES IDENT sequence(IDENT)?
        reference_action_clause*
          { `Ignore }
      | CHECK LPAREN expr RPAREN { `Ignore }

reference_action_clause:
  ON either(DELETE, UPDATE) reference_action { }

reference_action:
  RESTRICT | CASCADE | SET NULL | NO ACTION | SET DEFAULT { }

on_conflict: ON CONFLICT algo=conflict_algo { algo }
column_def_extra: PRIMARY? KEY { Some PrimaryKey }
                | NOT NULL { Some NotNull }
                | NULL { Some Null }
                | UNIQUE KEY? { Some Unique }
                | AUTOINCREMENT { Some Autoincrement }
                | DEFAULT default_value { Some WithDefault }
                | on_conflict { None }
                | CHECK LPAREN expr RPAREN { None }
                | COLLATE IDENT { None }
                | pair(GENERATED,ALWAYS)? AS LPAREN expr RPAREN either(VIRTUAL,STORED)? { None } (* FIXME params and typing ignored *)

default_value: e=single_literal_value 
             | e=datetime_value { e } (* sub expr ? *)
             | LPAREN e=expr RPAREN { e }

set_column: 
  | name=attr_name EQUAL e=set_column_expr { name, e }

set_column_expr:
  | e=expr { RegularExpr e }
  | LCURLY e=expr RCURLY TWO_QSTN { (WithDefaultParam (e, (($startofs, $endofs), ($startofs + 1, $endofs - 3)))) }
  | DEFAULT { AssignDefault }

anyall: ANY | ALL | SOME { }

mnot(X): NOT x = X | x = X { x }

attr_name: cname=IDENT { { cname; tname=None} }
         | table=table_name DOT cname=IDENT { {cname; tname=Some table} } (* FIXME database identifier *)

distinct_from: DISTINCT FROM { }

like_expr: e1=expr mnot(like) e2=expr %prec LIKE { Fun { kind = (fixed Bool [Text; Text]); parameters = [e1;e2]; is_over_clause = false } }

expr:
      e1=expr numeric_bin_op e2=expr %prec PLUS { Fun { kind = (Ret (depends Any)); parameters = [e1;e2]; is_over_clause = false } } (* TODO default Int *)
    | MOD LPAREN e1=expr COMMA e2=expr RPAREN { Fun { kind = (Ret (depends Any)); parameters = [e1;e2]; is_over_clause = false } } (* mysql special *)
    | e1=expr NUM_DIV_OP e2=expr %prec PLUS { Fun { kind = (Ret (depends Float)); parameters = [e1;e2]; is_over_clause = false } }
    | e1=expr DIV e2=expr %prec PLUS { Fun { kind = (Ret (depends Int)); parameters = [e1;e2]; is_over_clause = false } }
    | e1=expr boolean_bin_op e2=expr %prec AND { Fun { kind = (fixed Bool [Bool;Bool]); parameters = [e1;e2]; is_over_clause = false } }
    | e1=expr comp_op=comparison_op anyall? e2=expr %prec EQUAL { Fun { kind = Comparison comp_op; parameters = [e1; e2]; is_over_clause = false } }
    | e1=expr NOT_DISTINCT_OP anyall? e2=expr %prec EQUAL { poly (depends Bool) [e1;e2] }
    | e1=expr CONCAT_OP e2=expr { Fun { kind = (fixed Text [Text;Text]); parameters = [e1;e2]; is_over_clause = false } }
    | e=like_expr esc=escape?
      {
        match esc with
        | None -> e
        | Some esc -> Fun { kind = (fixed Bool [Bool; Text]); parameters = [e;esc]; is_over_clause = false }
      }
    | f=unary_op e=expr { f e }
    | MINUS e=expr %prec UNARY_MINUS { e }
    | INTERVAL e=expr interval_unit { Fun { kind = (fixed Datetime [Int]); parameters = [e]; is_over_clause = false } }
    | LPAREN e=expr RPAREN { e }
    | a=attr_name collate? { Column a }
    | VALUES LPAREN n=IDENT RPAREN { Inserted n }
    | v=literal_value | v=datetime_value { v }
    | INTERVAL_UNIT { Value (strict Datetime) }
    | e1=expr mnot(IN) l=sequence(expr) { poly (depends Bool) (e1::l) }
    | e1=expr mnot(IN) LPAREN select=select_stmt RPAREN { poly (depends Bool) [e1; SelectExpr (select, `AsValue)] }
    | e1=expr IN table=table_name { Tables.check table; e1 }
    | e1=expr k=in_or_not_in p=param
      {
        let e = poly (depends Bool) [ e1; Inparam (new_param p (depends Any), Meta.empty()) ] in
        InChoice ({ label = p.label; pos = ($startofs, $endofs) }, k, e )
      }
    | LPAREN exprs=commas(expr) RPAREN k=in_or_not_in p=param
      {
        InTupleList({exprs; param_id = p; kind = k; pos = ($startofs, $endofs); })
      }
    | LPAREN select=select_stmt RPAREN { SelectExpr (select, `AsValue) }
    | p=param t=preceded(DOUBLECOLON, manual_type)? { Param (new_param { p with pos=($startofs, $endofs) } (Option.default (depends Any) t), Meta.empty())  }
    | LCURLY e=expr RCURLY QSTN { OptionActions ({ choice=e; pos=(($startofs, $endofs), ($startofs + 1, $endofs - 2)); kind = BoolChoices}) }
    | p=param parser_state_ident LCURLY l=choices c2=RCURLY { let { label; pos=(p1,_p2) } = p in Choices ({ label; pos = (p1,c2+1)},l) }
    | SUBSTRING LPAREN s=expr FROM p=expr FOR n=expr RPAREN
    | SUBSTRING LPAREN s=expr COMMA p=expr COMMA n=expr RPAREN { Fun { kind = (Function.lookup "substring" 3); parameters = [s;p;n]; is_over_clause = false } }
    | SUBSTRING LPAREN s=expr either(FROM,COMMA) p=expr RPAREN { Fun { kind = (Function.lookup "substring" 2); parameters = [s;p]; is_over_clause = false } }
    | DATE LPAREN e=expr RPAREN { Fun { kind = (Function.lookup "date" 1); parameters = [e]; is_over_clause = false } }
    | TIME LPAREN e=expr RPAREN { Fun { kind = (Function.lookup "time" 1); parameters = [e]; is_over_clause = false } }
    | f=INTERVAL_UNIT LPAREN e=expr RPAREN { Fun { kind = Function.lookup f 1; parameters = [e]; is_over_clause = false } }
    | EXTRACT LPAREN interval_unit FROM e=expr RPAREN { Fun { kind = Function.lookup "extract" 1; parameters = [e]; is_over_clause = false } }
    | DEFAULT LPAREN a=attr_name RPAREN { Fun { kind = Type.identity; parameters = [Column a]; is_over_clause = false } }
    | CONVERT LPAREN e=expr USING IDENT RPAREN { e }
    | CONVERT LPAREN e=expr COMMA t=sql_type RPAREN
    | CAST LPAREN e=expr AS t=sql_type RPAREN { Fun { kind = (Ret (depends t)); parameters = [e]; is_over_clause = false } }
    | f=table_name LPAREN p=func_params RPAREN { Fun { kind = (Function.lookup f.tn (List.length p)); parameters = p; is_over_clause = false } }
    | e=expr IS NOT? NULL { poly (strict Bool) [e] }
    | e1=expr IS NOT? distinct_from? e2=expr { poly (strict Bool) [e1;e2] }
    | e=expr mnot(BETWEEN) a=expr AND b=expr { poly (depends Bool) [e;a;b] }
    | mnot(EXISTS) LPAREN select=select_stmt RPAREN { Fun { kind = (F (Typ (strict Bool), [Typ (depends Any)])); parameters = [SelectExpr (select,`Exists)]; is_over_clause = false } }
    | CASE initial_expr=expr? branches_list=nonempty_list(case_branch) else_expr=preceded(ELSE,expr)? END
      {
        let case_record = {
          Sql.case = initial_expr; 
          Sql.branches = branches_list;
          Sql.else_ = else_expr;
        } in
        Sql.Case case_record
      }
    | IF LPAREN e1=expr COMMA e2=expr COMMA e3=expr RPAREN { Fun { kind = (F (Var 0, [Typ (depends Bool);Var 0;Var 0])); parameters = [e1;e2;e3]; is_over_clause = false } }
    | e=window_function OVER window_spec { e }
    | f=table_name LPAREN p=func_params RPAREN OVER window_spec 
        { Fun { kind = (Function.lookup_agg f.tn (List.length p)); parameters = p; is_over_clause = true } }

values_stmt1: 
  | VALUES expr_list=commas(preceded(ROW, delimited(LPAREN, expr_list, RPAREN))) { RowExprList expr_list }
  | VALUES id=PARAM DOUBLECOLON types=sequence(manual_type) { RowParam { id={ id with pos=($startofs, $endofs) } ; types; values_start_pos = $startofs  } }

values_stmt: 
  | kind=values_stmt1 row_order=loption(order) row_limit=limit_t? {{ row_constructor_list = kind; row_order; row_limit;}}
  

(* https://dev.mysql.com/doc/refman/8.0/en/window-functions-usage.html *)
window_function:
  | either(FIRST_VALUE,LAST_VALUE) LPAREN e=expr RPAREN { e }
  | NTH_VALUE LPAREN e=expr COMMA INTEGER RPAREN { e }
  | either(LAG,LEAD) LPAREN e=expr pair(COMMA, pair(MINUS?,INTEGER))? RPAREN { e }

window_spec: LPAREN e=partition? o=order? frame? RPAREN (* TODO order parameters? *) { (Option.may (fun o -> make_partition_by (List.map fst o )) o); e }
partition: PARTITION BY e=expr_list { make_partition_by e} (* TODO check no params *)

frame: either(ROWS,RANGE) either(frame_border, frame_between) { }

frame_between: BETWEEN frame_border AND frame_border { }

frame_border:
  | CURRENT ROW
  | UNBOUNDED PRECEDING
  | UNBOUNDED FOLLOWING
  | expr PRECEDING
  | expr FOLLOWING { }

in_or_not_in: IN { `In } | NOT IN { `NotIn }
case_branch: WHEN w=expr THEN t=expr
             { { Sql.when_ = w; Sql.then_ = t } }
like: LIKE | LIKE_OP { }

choice_body: c1=LCURLY e=expr c2=RCURLY { (c1,Some e,c2) }
choice: parser_state_normal label=IDENT? e=choice_body? { let (c1,e,c2) = Option.default (0,None,0) e in ({ label; pos = (c1+1,c2) },e) }
choices: separated_nonempty_list(pair(parser_state_ident,NUM_BIT_OR),choice) { $1 }

datetime_value: | DATETIME_FUNC | DATETIME_FUNC LPAREN INTEGER? RPAREN { Value (strict Datetime) }

strict_value:
    | TEXT { StringLiteral $1 }
    | BLOB collate? { Blob }
    | INTEGER { Int }
    | FLOAT { Float }
    | TRUE
    | FALSE { Bool }
    | DATE TEXT
    | TIME TEXT
    | TIMESTAMP TEXT { Datetime }

literal_value:
    | strict_value { Value (strict $1) }
    | NULL { Value (nullable Any) } (* he he *)

single_literal_value:
    | literal_value { $1 }
    | MINUS INTEGER { Value (strict Int) }
    | MINUS FLOAT { Value (strict Float) }

expr_list: l=commas(expr) { l }
func_params: DISTINCT? l=expr_list { l }
           | ASTERISK { [] }
           | (* *) { [] }
escape: ESCAPE expr { $2 }
numeric_bin_op: PLUS | MINUS | ASTERISK | MOD | NUM_BIT_OR | NUM_BIT_AND | NUM_BIT_SHIFT { }
comparison_op: 
    | EQUAL { Comp_equal }
    | NUM_CMP_OP { Comp_num_cmp }
    | NUM_EQ_OP { 
      (* it would be nice to go into num_eq_op, 
         and consider == as equal as well. but for now
         we conservatively return `Comp_num_eq *)  
      Comp_num_eq 
    }

boolean_bin_op: AND | OR | XOR { }

unary_op: EXCL { 
          (* Some SQLs use ! as negation, some don't. play it safe and negate it,
             since negation is currently only used to verify cardinality constraints *)
          (fun id -> Fun { kind = Negation; parameters = [id]; is_over_clause = false }) } 
        | TILDE { (fun id -> id) }
        | NOT { (fun id -> Fun { kind = Negation; parameters = [id]; is_over_clause = false }) }

interval_unit: INTERVAL_UNIT
             | SECOND_MICROSECOND | MINUTE_MICROSECOND | MINUTE_SECOND
             | HOUR_MICROSECOND | HOUR_SECOND | HOUR_MINUTE
             | DAY_MICROSECOND | DAY_SECOND | DAY_MINUTE | DAY_HOUR
             | YEAR_MONTH { Value (strict Datetime) }

sql_type_flavor: T_INTEGER UNSIGNED? ZEROFILL? { Int }
               | T_DECIMAL { Decimal }
               | binary { Blob }
               | NATIONAL? text VARYING? charset? collate? { Text }
               | ENUM ctors=sequence(TEXT) charset? collate? { make_enum_kind ctors }
               | T_FLOAT PRECISION? { Float }
               | T_BOOLEAN { Bool }
               | T_DATETIME | DATE | TIME | TIMESTAMP { Datetime }
               | T_UUID { Blob }
               | T_JSON { Json }

binary: T_BLOB | BINARY | BINARY VARYING { }
text: T_TEXT | T_TEXT LPAREN INTEGER RPAREN | CHARACTER { }

%inline either(X,Y): X | Y { }
%inline commas(X): l=separated_nonempty_list(COMMA,X) { l }
(* (x1,x2,...,xn) *)
%inline sequence_(X): LPAREN l=commas(X) { l }
%inline sequence(X): l=sequence_(X) RPAREN { l }

charset: CHARSET either(IDENT,BINARY) | CHARACTER SET either(IDENT,BINARY) | ASCII | UNICODE { }
collate: COLLATE c=IDENT { Dialect_feature.set_collation c ($startofs, $endofs) }

sql_type: t=sql_type_flavor
        | t=sql_type_flavor LPAREN INTEGER RPAREN UNSIGNED?
        | t=sql_type_flavor LPAREN INTEGER COMMA INTEGER RPAREN
        { t }

compound_op:
  | UNION { `Union }
  | UNION ALL { `Union_all }
  | EXCEPT { `Except }
  | INTERSECT { `Intersect }

strict_type:
    | T_TEXT     { Text }
    | T_JSON     { Json }
    | T_BLOB     { Blob }
    | T_INTEGER  { Int }
    | T_FLOAT    { Float }
    | T_BOOLEAN  { Bool }
    | T_DATETIME { Datetime }

manual_type:
    | strict_type      { strict   $1 }
    | strict_type NULL { nullable $1 }

algorithm:
 | INPLACE { }
 | COPY { }
 | INSTANT { }

lock:
 | NONE {}
 | EXCLUSIVE {}
 | DEFAULT {}
 | SHARED {}
