/*
  Simple SQL parser
*/


%{
  open Sql
  open Sql.Type
  open Sql.Constraint
  open ExtLib

  (* preserve order *)
  let make_limit l =
    let param = function
      | _, `Const _ -> None
      | x, `Param { value=None; pos } -> Some (make_param ~id:{ value = Some (match x with `Limit -> "limit" | `Offset -> "offset"); pos } ~typ:(Source_type.strict Int))
      | _, `Param id -> Some (make_param ~id ~typ:(Source_type.strict Int))
    in
    List.filter_map param l, List.mem (`Limit,`Const 1) l

  let call name parameters = fn name (Function.lookup name (List.length parameters)) parameters

  let arith name ret e1 e2 = fn name (Arith (Source_type.depends ret)) [e1; e2]
%}

%token <int> INTEGER
%token <string> IDENT TEXT BLOB
%token <string> TYPE
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
       UNIQUE PRIMARY KEY FOREIGN AUTOINCREMENT ON CONFLICT DO NOTHING TEMPORARY IF EXISTS
       PRECISION SIGNED UNSIGNED ZEROFILL VARYING CHARSET NATIONAL ASCII UNICODE COLLATE BINARY CHARACTER
       DATETIME_FUNC DATE TIME TIMESTAMP ALTER RENAME ADD COLUMN CASCADE RESTRICT DROP
       GLOBAL LOCAL REFERENCES CHECK CONSTRAINT IGNORED AFTER INDEX FULLTEXT SPATIAL FIRST
       CASE WHEN THEN ELSE END CHANGE MODIFY DELAYED ENUM FOR SHARE MODE LOCK
       OF WITH NOWAIT ACTION NO IS INTERVAL SUBSTRING DIV MOD CONVERT LAG LEAD OVER
       FIRST_VALUE LAST_VALUE NTH_VALUE PARTITION ROWS RANGE UNBOUNDED PRECEDING FOLLOWING CURRENT ROW
       CAST GENERATED ALWAYS VIRTUAL STORED STATEMENT DOUBLECOLON QSTN TWO_QSTN INSTANT INPLACE COPY ALGORITHM RECURSIVE
       SHARED EXCLUSIVE NONE
       TTL TTL_ENABLE REMOVE CACHE NOCACHE
%token FUNCTION PROCEDURE LANGUAGE RETURNS OUT INOUT BEGIN COMMENT
%token SECOND_MICROSECOND MINUTE_MICROSECOND MINUTE_SECOND
       HOUR_MICROSECOND HOUR_SECOND HOUR_MINUTE
       DAY_MICROSECOND DAY_SECOND DAY_MINUTE DAY_HOUR EXTRACT
       YEAR_MONTH FALSE TRUE DUPLICATE
%token GROUP_CONCAT SEPARATOR
%token JSON_ARRAYAGG
%token NUM_DIV_OP NUM_EQ_OP NUM_CMP_OP PLUS MINUS NOT_DISTINCT_OP NUM_BIT_SHIFT NUM_BIT_OR NUM_BIT_AND
%token TEXT_CMP_OP TEXT_DIST_OP
%token JSON_EXTRACT_OP JSON_UNQUOTE_EXTRACT_OP
%token <Sql.int_size option> T_INTEGER
%token T_TEXT T_TINYTEXT T_MEDIUMTEXT T_LONGTEXT T_CHAR T_VARCHAR T_VARCHAR2
%token T_BLOB T_TINYBLOB T_MEDIUMBLOB T_LONGBLOB T_VARBINARY
%token T_FLOAT T_DOUBLE T_BOOLEAN T_DATETIME T_UUID T_DECIMAL T_JSON

(*
%left COMMA_JOIN
%left JOIN_JOIN
*)
(* FIXME precedence of COMMA and JOIN *)

%nonassoc LOWEST
%nonassoc COLLATE KEY ON WITH NULL

(* https://dev.mysql.com/doc/refman/8.0/en/operator-precedence.html *)

%left OR CONCAT_OP
%left XOR
%left AND
%nonassoc NOT
%nonassoc BETWEEN
%nonassoc EQUAL NUM_EQ_OP NOT_DISTINCT_OP IS LIKE LIKE_OP IN
%nonassoc NUM_CMP_OP TEXT_CMP_OP TEXT_DIST_OP
%left NUM_BIT_OR
%left NUM_BIT_AND
%left NUM_BIT_SHIFT
%left PLUS MINUS
%left ASTERISK NUM_DIV_OP MOD DIV
%left JSON_EXTRACT_OP JSON_UNQUOTE_EXTRACT_OP
(* ^ *)
%nonassoc UNARY_MINUS TILDE
%nonassoc EXCL
(* Warning: the precedence level assigned to BINARY is never useful. *)
(* %nonassoc BINARY COLLATE *)
%nonassoc ESCAPE

%type <Sql.expr> expr

%start <Sql.stmt> input

%%

input: statement EOF { $1 }

param: 
  | QSTN { { value=None; pos = ($startofs, $endofs) } }
  | PARAM  { $1 }

if_not_exists: IF NOT EXISTS { }
if_exists: IF EXISTS {}
temporary: either(GLOBAL,LOCAL)? TEMPORARY { }
assign: name=ident EQUAL e=expr { name, e }


cte_item: | cte_name=located(ident) names=maybe_parenth(sequence(ident))? AS LPAREN stmt=select_stmt_plain RPAREN
            {
              let cols = Option.map (List.map (fun name -> make_attribute' name (depends Any) )) names in
              { cte_name; cols; stmt = CteInline stmt }
            }
          | cte_name=located(ident) names=maybe_parenth(sequence(ident))? AS shared_query_ref_id=SHARED_QUERY_REF
            {
              let cols = Option.map (List.map (fun name -> make_attribute' name (depends Any) )) names in
              { cte_name; cols; stmt = CteSharedQuery shared_query_ref_id; }
            }
cte: is_recursive=cte_with cte_items=commas(cte_item) {{ cte_items; is_recursive }}

statement: CREATE ioption(temporary) TABLE ioption(if_not_exists) name=located(table_name) schema=table_definition
              {
                Create (name, Schema schema)
              }
         | CREATE either(TABLE,VIEW) name=located(table_name) AS select=maybe_parenth(located(select_stmt))
              {
                Create (name, Select select)
              }
         | ALTER TABLE name=table_name actions=commas(alter_action_or_ignored)
              {
                Alter (name, List.filter_map (fun x -> x) actions)
              }
         | RENAME TABLE l=separated_nonempty_list(COMMA, separated_pair(table_name,TO,table_name)) { Rename l }
         | DROP either(TABLE,VIEW) if_exists? name=table_name
              {
                Drop name
              }
         | CREATE u=boption(UNIQUE) INDEX if_not_exists? name=ident ON table=table_name cols=sequence(index_column)
              {
                let ci_kind = if u then Sql.Unique_idx else Sql.Plain_idx in
                CreateIndex { ci_name = name; ci_table = table; ci_cols = cols; ci_kind }
              }
         | select_stmt { Select $1 }
         | insert_action_kind=insert_cmd target=table_name names=insert_columns VALUES values=commas(sequence(set_column_expr))? ss=located(conflict_clause)?
              {
                Insert { insert_action_kind; target; action=`Values (names, values); on_conflict_clause=ss; }
              }
         | insert_action_kind=insert_cmd target=table_name names=insert_columns VALUES p=param ss=located(conflict_clause)?
              {
                Insert { insert_action_kind; target; action=`Param (names, p); on_conflict_clause=ss; }
              }
         | insert_action_kind=insert_cmd target=table_name names=insert_columns select=maybe_parenth(select_stmt) ss=located(conflict_clause)?
              {
                Insert { insert_action_kind; target; action=`Select (names, select); on_conflict_clause=ss; }
              }
         | insert_action_kind=insert_cmd target=table_name SET set=commas(set_column)? ss=located(conflict_clause)?
              {
                Insert { insert_action_kind; target; action=`Set set; on_conflict_clause=ss; }
              }
         /* http://dev.mysql.com/doc/refman/5.1/en/update.html multi-table syntax */
         | update_cmd tables=table_list SET ss=commas(set_column) w=where? o=loption(order) lim=loption(limit)
              {
                match tables with
                | (`Table table, None), [] -> Update (table,ss,w,o,lim)
                | _ -> UpdateMulti (tables,ss,w,o,lim)
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
           RETURNS ret=located_sql_type
           routine_extra?
           AS? routine_body
           routine_extra?
              {
                Function.add (List.length params) (Ret { Source_type.t = ret.value.collated; nullability = Type.Depends }) name.tn; (* FIXME store function namespace *)
                CreateRoutine (name, Some ret, params)
              }
         | CREATE or_replace? PROCEDURE name=table_name params=sequence(proc_parameter)
           routine_extra?
           AS? routine_body
           routine_extra?
              {
                Function.add (List.length params) (Ret (Source_type.depends Any)) name.tn; (* FIXME void *)
                CreateRoutine (name, None, params)
              }   
         | CREATE TYPE name=ident AS ENUM LPAREN ctors=commas(TEXT) RPAREN
              { CreateType (name, TypeEnum ctors) }
         | DROP TYPE ie=boption(if_exists) name=ident
              { DropType (name, ie) }

parameter_default_: DEFAULT | EQUAL { }
parameter_default: parameter_default_ e=expr { e }
func_parameter: n=ident AS? t=located_sql_type e=parameter_default? { (n,t,e) }
parameter_mode: IN | OUT | INOUT { }
proc_parameter: parameter_mode? p=func_parameter { p }

or_replace: OR REPLACE { }

routine_body: TEXT | compound_stmt { }
compound_stmt: BEGIN statement+ END { } (* mysql *)

routine_extra: LANGUAGE IDENT { }
             | COMMENT TEXT { }

(* cf. ColId / unreserved_keyword in PostgreSQL's gram.y (TYPE_P is unreserved there too):
   https://github.com/postgres/postgres/blob/REL_18_0/src/backend/parser/gram.y#L17632 *)
ident: x=IDENT | x=TYPE { x }

table_ident: x=ident { x }
qual_ident: x=ident { x }
func_ident: x=ident { x }

%inline table_name: name=table_ident { Sql.make_table_name name }
                  | db=table_ident DOT name=ident { Sql.make_table_name ~db name }
%inline qualifier: name=qual_ident { Sql.make_table_name name }
                 | db=qual_ident DOT name=ident { Sql.make_table_name ~db name }
%inline func_name: name=func_ident { Sql.make_table_name name }
                 | db=qual_ident DOT name=ident { Sql.make_table_name ~db name }
index_prefix: LPAREN n=INTEGER RPAREN { n }
index_column: name=ident index_prefix? c=collate? order_type? { make_collated ?collation:c ~collated:name ()}

table_definition: t=sequence_(column_def1) ignore_after(RPAREN) 
                      { 
                        List.fold_right
                          (fun x { schema; constraints; indexes } -> match x with
                          | `Attr a -> { schema = a::schema; constraints; indexes }
                          | `Constraint c -> { schema; constraints = c::constraints; indexes }
                          | `Index i -> { schema; constraints; indexes = i::indexes })
                          t { schema = []; constraints = []; indexes = [] }
                      }
                | LIKE name=maybe_parenth(table_name) { Tables.get name |> snd |> fun attrs -> { schema = List.map Alter_action_attr.from_attr attrs; constraints = []; indexes = [] } } (* mysql *)

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

select_stmt_plain: core=select_core other=list(pair(compound_op,select_core)) o=loption(order) lim=limit_t? select_row_locking=located(select_row_locking)?
              {
                let core = { core with source_pos = Some ($startofs, $endofs) } in
                { select = (core, other); order=o; limit=lim; select_row_locking; }
              }

select_core: SELECT select_type? r=commas(column1) f=from?  w=where?  g=loption(group) h=having?
              {
                { source_pos = Some ($startofs, $endofs);
                  columns=r; from=f; where=w; group=g; having=h; }
              }

table_list: src=source joins=located(join_source)* { (src,joins) }

inner_join: either(CROSS,INNER)? { Schema.Join.Inner }
inner_join_kw: either(CROSS,INNER) { Schema.Join.Inner }
outer_join: LEFT OUTER? | OUTER LEFT { Schema.Join.Left }
          | RIGHT OUTER? | OUTER RIGHT { Schema.Join.Right }
          | FULL OUTER? | OUTER FULL { Schema.Join.Full }
straight_join: STRAIGHT_JOIN { Schema.Join.Straight }
natural_join: NATURAL j=located(inner_join) | j=located(inner_join_kw) NATURAL { j }
            | NATURAL j=located(outer_join) | j=located(outer_join) NATURAL { j }
natural(join): j=join JOIN src=source { src, j, Schema.Join.Natural }
cond(join): j=join JOIN src=source c=join_cond { src, j, c }
straight_cond(join): j=join src=source c=join_cond { src, j, c }

join_source: COMMA src=source c=join_cond { src, make_located ~value:Schema.Join.Inner ~pos:($startofs, $endofs), c }
           | j=natural(natural_join)
           | j=cond(located(outer_join))
           | j=cond(located(inner_join)) { j }
           | j=straight_cond(located(straight_join)) { j }
join_cond: ON e=expr { On e }
         | USING l=sequence(ident) { Using l }
         | %prec LOWEST { Default }

source1: table_name { `Table $1 }
       | LPAREN s=select_stmt RPAREN { `Select s }
       | LPAREN s=table_list RPAREN { `Nested s }
       | LPAREN s=values_stmt RPAREN { `ValueRows s }

source: src=source1 alias=source_alias? {
  ( src, 
    Option.map (fun (tbl, cols) -> 
      let column_aliases = Option.map (List.map (fun name -> make_attribute' name (depends Any) )) cols in
      { table_name = { tbl with value = Sql.make_table_name tbl.value }; column_aliases }
    ) alias
  )
}

insert_cmd:  INSERT DELAYED? OR? conflict_algo INTO { Insert_into }
           | INSERT INTO { Insert_into }
           | REPLACE INTO { Replace_into ($startofs, $endofs) }
update_cmd: UPDATE | UPDATE OR conflict_algo { }
conflict_algo: CONFLICT_ALGO | REPLACE { }

on_conflict_action:
  | UPDATE SET ss=commas(set_column) { Do_update ss }
  | NOTHING { Do_nothing }

conflict_clause: 
  | ON DUPLICATE KEY UPDATE ss=commas(set_column)
    { On_duplicate { assignments = ss; }; }
  | ON CONFLICT LPAREN attrs=separated_nonempty_list(COMMA, attr_name) RPAREN DO action=on_conflict_action
    { On_conflict { action; attrs; }; }

select_type: DISTINCT | ALL { }

select_row_locking:
    for_update_or_share+
      { For_update }
  | LOCK IN SHARE MODE
      { For_share }

for_update_or_share:
  FOR either(UPDATE, SHARE) update_or_share_of? NOWAIT? with_lock { }

update_or_share_of: OF commas(ident) { }

with_lock: %prec LOWEST { } | WITH LOCK { }

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
       | c=located(column1_kind) { c }

column1_kind:
       | t=qualifier DOT ASTERISK { Sql.AllOf t }
       | ASTERISK { Sql.All }
       | c=pair(located(expr), maybe_as) { let (e, m) = c in Sql.Expr (e, m) }

maybe_as: AS? name=ident { Some name }
        | { None }

source_alias: AS? name=located(ident) names=sequence(ident)? { name, names }

maybe_parenth(X): x=X | LPAREN x=X RPAREN { x }

alter_column_pg_spec:
  | TYPE t=located_sql_type { Alter_column_pg.Set_type t }
  | SET NOT NULL { Alter_column_pg.Set_not_null }
  | DROP NOT NULL { Alter_column_pg.Drop_not_null }
  | SET DEFAULT default_value { Alter_column_pg.Set_default }
  | DROP DEFAULT { Alter_column_pg.Drop_default }

alter_action: ADD COLUMN? col=maybe_parenth(column_def) pos=alter_pos { `Add (col,pos) }
            | ADD PRIMARY KEY cols=sequence(ident) { `AddPrimaryKey cols }
            | ADD k=index_type name=ident? cols=sequence(ident) { `AddIndex { add_idx_name = name; add_idx_kind = k; add_idx_cols = cols } }
            | ADD CONSTRAINT name=ident? table_constraint_1 index_options { `AddConstraint name }
            | RENAME either(TO,AS)? new_name=table_name { `RenameTable new_name }
            | RENAME COLUMN old_name=ident TO new_name=ident { `RenameColumn (old_name, new_name) }
            | RENAME index_or_key old_name=ident TO new_name=ident { `RenameIndex (old_name, new_name) }
            | DROP INDEX name=ident { `DropIndex name }
            | DROP PRIMARY KEY { `DropPrimaryKey }
            | DROP COLUMN? col=ident drop_behavior? { `Drop col } (* FIXME behavior? *)
            | DROP FOREIGN KEY name=ident { `DropConstraint name }
            | DROP CHECK name=ident { `DropConstraint name }
            | CHANGE COLUMN? old_name=ident column=column_def pos=alter_pos { `Change (old_name,column,pos) }
            | MODIFY COLUMN? column=column_def pos=alter_pos { `Change (column.Alter_action_attr.name.value,column,pos) }
            | ALTER COLUMN? col=ident spec=located(alter_column_pg_spec) { `AlterColumnPG (col, spec) }
            | opts=ttl_option+ { `TtlOptions (opts, ($startofs, $endofs)) }
            | REMOVE TTL { `RemoveTtl ($startofs, $endofs) }
            | CACHE { `Cache ($startofs, $endofs) }
            | NOCACHE { `NoCache ($startofs, $endofs) }
            | either(DEFAULT,pair(CONVERT,TO))? cs=charset c=collate? { `Default_or_convert_to (cs, c) }

(* clauses sqlgg parses but does not act on: kept out of the action list *)
alter_action_or_ignored: a=alter_action { Some a }
            | SET IDENT IDENT { None }
            | ALGORITHM EQUAL algorithm { None }
            | LOCK EQUAL lock { None }

ttl_option: TTL EQUAL col=ident PLUS INTERVAL n=INTEGER unit=INTERVAL_UNIT
              { `TtlSet (col, n, unit) }
          | TTL_ENABLE EQUAL v=TEXT { `TtlEnable v }
index_or_key: INDEX | KEY { }
index_type:
  | index_or_key { Sql.Plain_idx }
  | UNIQUE index_or_key? { Sql.Unique_idx }
  | FULLTEXT index_or_key? { Sql.Fulltext_idx }
  | SPATIAL index_or_key? { Sql.Spatial_idx }
alter_pos: AFTER col=ident { `After col }
         | FIRST { `First }
         | { `Default }
drop_behavior: CASCADE | RESTRICT { }

column_def: name=located(ident) sql_kind=located_sql_type? extra=located(column_def_extra)*
  {
    let rule_start_pos_cnum = $startpos.Lexing.pos_cnum in
    let meta = List.concat @@ Parser_state.Stmt_metadata.find_all rule_start_pos_cnum in
    let extra = List.filter_map (fun { value; pos } -> Option.map (fun v -> { value = v; pos }) value) extra in
    { Alter_action_attr.name = name; meta; kind = sql_kind; extra; }
  }

inline_idx_kind:
  | index_or_key           { Sql.Regular_idx }
  | FULLTEXT index_or_key? { Sql.Fulltext }
  | SPATIAL index_or_key?  { Sql.Spatial }

column_def1: c=column_def { `Attr c }
           | pair(CONSTRAINT,ident?)? l=table_constraint_1 index_options { `Constraint l }
           | kind=inline_idx_kind t=table_index { let (idx_name, idx_cols) = t in `Index (make_located ~value:{ idx_kind = kind; idx_name; idx_cols; idx_unique = false } ~pos:($startofs, $endofs)) }

int_arg: LPAREN n=INTEGER RPAREN { n }

key_part: n=ident int_arg? either(ASC,DESC)? { n }

index_options: IDENT* { }

table_index: name=ident? l=sequence(key_part) index_options { (name, l) }

(* FIXME check columns *)
table_constraint_1:
      | PRIMARY KEY l=sequence(key_part) { `Primary l }
      | UNIQUE index_or_key? name=ident? l=sequence(key_part) { `Unique (name, l) }
      | FOREIGN KEY ident? cols=sequence(ident) REFERENCES t=table_name refs=sequence(ident)?
        reference_action_clause*
          { `Foreign (cols, t, Stdlib.Option.value ~default:[] refs) }
      | CHECK LPAREN expr RPAREN { `Ignore }

reference_action_clause:
  ON either(DELETE, UPDATE) reference_action { }

reference_action:
  RESTRICT | CASCADE | SET NULL | NO ACTION | SET DEFAULT { }

on_conflict: ON CONFLICT algo=conflict_algo { algo }
column_def_extra: PRIMARY? KEY { Some (Alter_action_attr.Syntax_constraint PrimaryKey) }
                | NOT NULL { Some (Alter_action_attr.Syntax_constraint NotNull) }
                | NULL { Some (Alter_action_attr.Syntax_constraint Null) }
                | UNIQUE %prec LOWEST
                | UNIQUE KEY { Some (Alter_action_attr.Syntax_constraint Unique) }
                | AUTOINCREMENT { Some (Alter_action_attr.Syntax_constraint Autoincrement) }
                | DEFAULT def=default_value {
                    let pos = ($startofs(def), $endofs(def)) in
                    Some (Alter_action_attr.Default {
                      expr = make_located ~value:def ~pos;
                      sql = Parser_state.extract_source pos;
                    })
                  }
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

anyall: ANY | SOME { `Any }
      | ALL { `All }

mnot(X): NOT x = X | x = X { x }

attr_name: cname=ident { { cname; tname=None; cpos = ($startofs, $endofs) } }
         | table=qualifier DOT cname=ident { {cname; tname=Some table; cpos = ($startofs, $endofs) } } (* FIXME database identifier *)

is_not: %prec LOWEST { } | NOT %prec LOWEST { }
distinct_from: { } | DISTINCT FROM { }

insert_column: ident { $1 }
%inline insert_columns: { None } | l=sequence(insert_column) { Some l }

operators(E):
      e1=E numeric_bin_op e2=E %prec PLUS { arith "numeric_bin_op" Any e1 e2 } (* TODO default Int *)
    | e1=E NUM_DIV_OP e2=E %prec PLUS { arith "num_div" Float e1 e2 }
    | e1=E TEXT_DIST_OP e2=E { arith "text_dist" Float e1 e2 }
    | e1=E DIV e2=E %prec PLUS { arith "div" Int e1 e2 }
    | e1=E op=comparison_op q=anyall? e2=E %prec EQUAL
      { let kind = Stdlib.Option.fold
          ~none:(Comparison op)
          ~some:(fun quantifier -> Quantified_comparison { op; quantifier })
          q
        in
        fn "comparison" kind [e1;e2] }
    | e1=E CONCAT_OP e2=E { fn "concat" (fixed Text [Text;Text]) [e1;e2] }
    | e1=E JSON_EXTRACT_OP e2=E { call "json_extract" [e1;e2] }
    | e1=E JSON_UNQUOTE_EXTRACT_OP e2=E { call "json_unquote" [call "json_extract" [e1;e2]] }
    | EXCL e=E %prec EXCL
      (* Some SQLs use ! as negation, some don't. play it safe and negate it,
         since negation is currently only used to verify cardinality constraints *)
      { fn "excl" Negation [e] }
    | TILDE e=E %prec TILDE { e }
    | MINUS e=E %prec UNARY_MINUS { e }
    | INTERVAL e=E interval_unit { fn "interval" (fixed Datetime [Int]) [e] }

b_expr: e=b_expr_ { Sql.with_pos ($startofs, $endofs) e }

expr: e=expr_ { Sql.with_pos ($startofs, $endofs) e }

c_expr: e=c_expr_ { Sql.with_pos ($startofs, $endofs) e }

b_expr_:
      e=c_expr | e=operators(b_expr) { e }

expr_:
      e=c_expr | e=operators(expr) { e }
    | e1=expr AND e2=expr { fn "boolean_bin_op" (Logical And) [e1;e2] }
    | e1=expr XOR e2=expr { fn "boolean_bin_op" (Logical Xor) [e1;e2] }
    | e1=expr OR e2=expr { fn "boolean_bin_op" (Logical Or) [e1;e2] }
    | NOT e=expr %prec NOT { fn "not" Negation [e] }
    | e1=expr mnot(like) e2=expr %prec LIKE { fn "like" Like [e1;e2] }
    | e1=expr mnot(like) e2=expr ESCAPE esc=expr { fn "like_escape" Like_escape [fn "like" Like [e1;e2]; esc] }
    | e1=expr mnot(IN) l=sequence(expr) { fn "in" Membership (e1::l) }
    | e1=expr mnot(IN) LPAREN select=select_stmt RPAREN { fn "in_select" Membership [e1; SelectExpr (select, `AsValue)] }
    | e1=expr IN table=table_name { Tables.check table; e1 }
    | e1=expr k=in_or_not_in p=param
      {
        let arg = Inparam (make_param ~id:p ~typ:(Source_type.depends Any), Meta.empty()) in
        let e = fn "in_param" Membership [e1; arg] in
        InChoice (make_located ~value:p.value ~pos:($startofs, $endofs), k, e )
      }
    | LPAREN e=expr COMMA es=commas(expr) RPAREN k=in_or_not_in p=param
      {
        InTupleList(make_located ~value:{ exprs = e :: es; param_id = p; kind_in_tuple_list = k; } ~pos:($startofs, $endofs))
      }
    | e=expr IS NOT NULL { fn "is_not_null" (Comparison Is_not_null) [e] }
    | e=expr IS NULL { fn "is_null" (Comparison Is_null) [e] }
    | e1=expr IS is_not distinct_from e2=expr %prec IS { fn "is_distinct" (Comparison Not_distinct_op) [e1;e2] }
    | e=expr mnot(BETWEEN) a=b_expr AND b=expr { fn "between" Range [e;a;b] }

c_expr_:
      MOD LPAREN e1=expr COMMA e2=expr RPAREN { arith "mod" Any e1 e2 } (* mysql special *)
    | LPAREN e=expr RPAREN { e }
    | a=attr_name c=collate? { column ?collation:c a }
    | VALUES LPAREN n=ident RPAREN { Of_values n }
    | v=literal_value | v=datetime_value { v }
    | INTERVAL_UNIT { Value (make_collated ~collated:(strict Datetime) ()) }
    | LPAREN select=select_stmt RPAREN { SelectExpr (select, `AsValue) }
    | p=param t=preceded(DOUBLECOLON, manual_type)? { Param (make_param ~id:{ p with pos=($startofs, $endofs) } ~typ:(Stdlib.Option.value ~default:(Source_type.depends Any) t), Meta.empty())  }
    | LCURLY e=expr RCURLY QSTN { OptionActions ({ choice=e; pos=(($startofs, $endofs), ($startofs + 1, $endofs - 2)); kind = BoolChoices}) }
    | p=param parser_state_ident LCURLY l=choices c2=RCURLY { let { value; pos=(p1,_p2) } = p in Choices ({ value; pos = (p1,c2+1)},l) }
    | SUBSTRING LPAREN s=expr FROM p=expr FOR n=expr RPAREN
    | SUBSTRING LPAREN s=expr COMMA p=expr COMMA n=expr RPAREN { call "substring" [s;p;n] }
    | SUBSTRING LPAREN s=expr either(FROM,COMMA) p=expr RPAREN { call "substring" [s;p] }
    | REPLACE LPAREN s=expr COMMA from=expr COMMA to_=expr RPAREN { call "replace" [s;from;to_] }
    | DATE LPAREN e=expr RPAREN { call "date" [e] }
    | TIME LPAREN e=expr RPAREN { call "time" [e] }
    | f=INTERVAL_UNIT LPAREN e=expr RPAREN { call f [e] }
    | EXTRACT LPAREN interval_unit FROM e=expr RPAREN { call "extract" [e] }
    | DEFAULT LPAREN a=attr_name RPAREN { fn "default" fun_identity [Column (make_collated ~collated:a ())] }
    | CONVERT LPAREN e=expr USING IDENT RPAREN { e }
    | CONVERT LPAREN e=expr COMMA f=cast_as RPAREN { f e }
    | GROUP_CONCAT LPAREN p=func_params order=loption(order) preceded(SEPARATOR, TEXT)? RPAREN
      { fn "group_concat" (Agg (With_order { with_order_kind = Group_concat; order })) p }
    | JSON_ARRAYAGG LPAREN p=func_params order=loption(order) limit_t? RPAREN
      { fn "json_arrayagg" (Agg (With_order { with_order_kind = Json_arrayagg; order })) p }
    | CAST LPAREN e=expr AS f=cast_as RPAREN { f e }
    | f=func_name LPAREN p=func_params RPAREN { call f.tn p }
    | EXISTS LPAREN select=select_stmt RPAREN { fn "exists" (F (Typ (strict Bool), [Typ (depends Any)])) [SelectExpr (select,`Exists)] }
    | CASE initial_expr=expr? branches_list=nonempty_list(case_branch) else_expr=preceded(ELSE,expr)? END
      {
        let case_record = {
          Sql.case = initial_expr; 
          Sql.branches = branches_list;
          Sql.else_ = else_expr;
        } in
        Sql.Case case_record
      }
    | IF LPAREN e1=expr COMMA e2=expr COMMA e3=expr RPAREN { fn "if" (F (Var 0, [Typ (depends Bool); Var 0; Var 0])) [e1;e2;e3] }
    | w=window_function OVER spec=window_spec { w spec }
    | f=func_name LPAREN p=func_params RPAREN OVER w=window_spec
        { fn ~over:w f.tn (Function.lookup_agg f.tn (List.length p)) p }
values_stmt1: 
  | VALUES expr_list=commas(preceded(ROW, delimited(LPAREN, expr_list, RPAREN))) { RowExprList expr_list }
  | VALUES id=PARAM DOUBLECOLON types=sequence(manual_type) { RowParam { id={ id with pos=($startofs, $endofs) } ; types; values_start_pos = $startofs  } }

values_stmt: 
  | kind=values_stmt1 row_order=loption(order) row_limit=limit_t? {{ row_constructor_list = kind; row_order; row_limit;}}
  

(* https://dev.mysql.com/doc/refman/8.0/en/window-functions-usage.html *)
lag_or_lead: LAG { "lag" } | LEAD { "lead" }

window_function:
  | FIRST_VALUE LPAREN e=expr RPAREN { fun over -> fn ~over "first_value" (Agg Self) [e] }
  | LAST_VALUE LPAREN e=expr RPAREN { fun over -> fn ~over "last_value" (Agg Self) [e] }
  | NTH_VALUE LPAREN e=expr COMMA INTEGER RPAREN
    { fun _ -> fn ~over:{ frame_has_a_row = false } "nth_value" (Agg Self) [e] }
  | f=lag_or_lead LPAREN e=expr offset=pair(COMMA, pair(MINUS?,INTEGER))? RPAREN
    {
      match offset with
      | Some (_, (_, 0)) -> (fun _ -> e)
      | None | Some _ -> (fun _ -> fn ~over:{ frame_has_a_row = false } f (Agg Self) [e])
    }

frame:
  | either(ROWS,RANGE) start=frame_border { start, `Current }
  | either(ROWS,RANGE) BETWEEN start=frame_border AND stop=frame_border { start, stop }

frame_border:
  | CURRENT ROW { `Current }
  | UNBOUNDED PRECEDING { `Before }
  | UNBOUNDED FOLLOWING { `After }
  | expr PRECEDING { `Before }
  | expr FOLLOWING { `After }

window_spec: LPAREN partition? o=order? f=frame? RPAREN (* TODO order parameters? *)
  {
    Stdlib.Option.iter (fun o -> make_partition_by (List.map fst o)) o;
    { frame_has_a_row = match f with
      | None -> true
      | Some (`After, _) | Some (_, `Before) -> false
      | Some ((`Before | `Current), (`Current | `After)) -> true }
  }

partition: PARTITION BY e=expr_list { make_partition_by e} (* TODO check no params *)

in_or_not_in: IN { `In } | NOT IN { `NotIn }
case_branch: WHEN w=expr THEN t=expr
             { { Sql.when_ = w; Sql.then_ = t } }
like: LIKE | LIKE_OP { }

choice_body: c1=LCURLY e=expr c2=RCURLY { (c1,Some e,c2) }
choice: parser_state_normal value=IDENT? e=choice_body?
  {
    let (c1, body, c2) = Stdlib.Option.value ~default:(0, None, 0) e in
    { ctor = { value; pos = (c1 + 1, c2) }; ctor_pos = ($startofs(value), $endofs(value)); body }
  }
choices: separated_nonempty_list(pair(parser_state_ident,NUM_BIT_OR),choice) { $1 }

datetime_value: | DATETIME_FUNC | DATETIME_FUNC LPAREN INTEGER? RPAREN { Value { collated=(strict Datetime); collation=None; } }

literal_value:
    | TEXT c=collate_opt { Value { collated=(strict (StringLiteral $1)); collation=c; } }
    | BLOB c=collate_opt { Value { collated=(strict Blob); collation=c; } }
    | INTEGER         { Value { collated=(strict Int);  collation=None; } }
    | TRUE
    | FALSE           { Value { collated=(strict Bool); collation=None; } }
    | DATE TEXT
    | TIME TEXT
    | TIMESTAMP TEXT  { Value { collated=(strict Datetime); collation=None; } }
    | FLOAT           { Value { collated=(strict (FloatingLiteral $1)); collation=None; } }
    | NULL            { Value { collated=(nullable Any); collation=None; } } (* he he *)

single_literal_value:
    | literal_value { $1 }
    | MINUS INTEGER { Value { collated=(strict Int); collation=None; } }
    | MINUS FLOAT   { Value { collated=(strict (FloatingLiteral $2)); collation=None; } }

expr_list: l=commas(expr) { l }
func_params: DISTINCT? l=expr_list { l }
           | ASTERISK { [] }
           | (* *) { [] }
numeric_bin_op: PLUS | MINUS | ASTERISK | MOD | NUM_BIT_OR | NUM_BIT_AND | NUM_BIT_SHIFT { }
comparison_op: 
    | EQUAL { Comp_equal }
    | NUM_CMP_OP { Comp_num_cmp }
    | TEXT_CMP_OP { Comp_text_cmp }
    | NOT_DISTINCT_OP { Not_distinct_op }
    | NUM_EQ_OP { 
      (* it would be nice to go into num_eq_op, 
         and consider == as equal as well. but for now
         we conservatively return `Comp_num_eq *)  
      Comp_num_eq 
    }

interval_unit: INTERVAL_UNIT
             | SECOND_MICROSECOND | MINUTE_MICROSECOND | MINUTE_SECOND
             | HOUR_MICROSECOND | HOUR_SECOND | HOUR_MINUTE
             | DAY_MICROSECOND | DAY_SECOND | DAY_MINUTE | DAY_HOUR
             | YEAR_MONTH { Value (make_collated ~collated:(strict Datetime) ()) }

int_type:
  | s=T_INTEGER n=int_arg? u=boption(UNSIGNED) {
      Sql.Source_type.Int {
        size = s;
        sign = if u then Sql.Unsigned else Sql.Signed;
        display_width = n;
      }
    }

decimal_type:
                 | T_DECIMAL p=option(delimited(LPAREN, pair(INTEGER, option(preceded(COMMA, INTEGER))), RPAREN)) {
                      match p with
                      | Some (precision, scale) -> Decimal { precision = Some precision; scale }
                      | None -> Decimal { precision = None; scale = None}
                  }

(* expr_sql_type_flavor returns Type.kind for use in CAST *)
expr_sql_type_flavor:
                 | binary { Blob }
                 | NATIONAL? text VARYING? charset? { Text }
                 | T_BOOLEAN { Bool }
                 | T_DATETIME | DATE | TIME | TIMESTAMP { Datetime }
                 | T_UUID { Blob }
                 | T_JSON { Json }

%inline lob_size(PLAIN, TINY, MEDIUM, LONG):
  | PLAIN  { None }
  | TINY   { Some Sql.Tiny }
  | MEDIUM { Some Sql.Medium }
  | LONG   { Some Sql.Long }

%inline text_plain: s=lob_size(T_TEXT, T_TINYTEXT, T_MEDIUMTEXT, T_LONGTEXT) { Source_type.PlainText s }
%inline blob_plain: s=lob_size(T_BLOB, T_TINYBLOB, T_MEDIUMBLOB, T_LONGBLOB) { Source_type.PlainBlob s }

%inline text_var:
  | T_CHAR n=int_arg?     { Source_type.Char n }
  | T_VARCHAR n=int_arg?  { Source_type.Varchar n }
  | T_VARCHAR2 n=int_arg? { Source_type.Varchar2 n }

(* sql_type_flavor returns Source_type.kind *)
sql_type_flavor:
  | t=int_type ZEROFILL? { t }
  | T_VARBINARY n=int_arg?                              { Source_type.Blob (Varbinary n) }
  | NATIONAL? f=text_var VARYING? charset?              { Source_type.Text f }
  | t=decimal_type { Source_type.Infer t }
  | t=sql_type_flavor_plain type_args? { t }

sql_type_flavor_plain:
  | T_FLOAT { Source_type.Float Single }
  | T_DOUBLE PRECISION? { Source_type.Float Double }
  | f=blob_plain                                        { Source_type.Blob f }
  | NATIONAL? f=text_plain charset?                     { Source_type.Text f }
  | t=expr_sql_type_flavor { Source_type.Infer t }
  | ENUM ctors=sequence(TEXT) charset? { Source_type.Infer (make_enum_kind ctors) }
  | name=ident { Source_type.Infer (User_types.get name) }

type_args: LPAREN INTEGER RPAREN UNSIGNED? | LPAREN INTEGER COMMA INTEGER RPAREN { }

binary: BINARY | BINARY VARYING { }
text: CHARACTER { }

cast_as:
    | t=cast_sql_type { (fun e -> fn "cast" (Ret (Source_type.depends t)) [e]) }
    | UNSIGNED { (fun e -> fn "cast_unsigned" (Ret (Source_type.depends UInt64)) [e]) }
    | SIGNED { (fun e -> fn "cast_signed" (Ret (Source_type.depends Int)) [e]) }

%inline either(X,Y): X | Y { }
%inline commas(X): l=separated_nonempty_list(COMMA,X) { l }
(* (x1,x2,...,xn) *)
%inline sequence_(X): LPAREN l=commas(X) { l }
%inline sequence(X): l=sequence_(X) RPAREN { l }

%inline charset_kw: CHARSET {} | CHARACTER SET {}
charset: charset_kw c=IDENT { Named c }
       | charset_kw BINARY { Binary }
       | charset_kw? ASCII { Ascii }
       | charset_kw? UNICODE { Unicode }
collate: COLLATE c=IDENT { make_located ~value:c ~pos:($startofs, $endofs) }
collate_opt: %prec LOWEST { None } | c=collate { Some c }

sql_type: t=sql_type_flavor c=collate_opt { make_collated ?collation:c ~collated:t () }

located_sql_type: t=sql_type { make_located ~value:t ~pos:($startofs, $endofs) }

cast_sql_type:
        | t=decimal_type { t }
        | t=expr_sql_type_flavor { t }
        | t=expr_sql_type_flavor LPAREN INTEGER RPAREN { t }
        | t=expr_sql_type_flavor LPAREN INTEGER COMMA INTEGER RPAREN { t }
        | T_FLOAT { Float }
        | T_DOUBLE PRECISION? { Float }
        | blob_plain | T_VARBINARY int_arg? { Blob }
        | text_plain | text_var { Text }

compound_op:
  | UNION { `Union }
  | UNION ALL { `Union_all }
  | EXCEPT { `Except }
  | INTERSECT { `Intersect }

(* manual_type returns Source_type.t for parameter type annotations *)
manual_type:
    | f=text_plain           { { Source_type.t = Text f; nullability = Type.Strict } }
    | f=text_var             { { Source_type.t = Text f; nullability = Type.Strict } }
    | T_JSON                 { Source_type.strict Json }
    | f=blob_plain           { { Source_type.t = Blob f; nullability = Type.Strict } }
    | T_VARBINARY n=int_arg? { { Source_type.t = Blob (Varbinary n); nullability = Type.Strict } }
    | t=int_type             { { Source_type.t; nullability = Type.Strict } }
    | T_FLOAT                { { Source_type.t = Float Single; nullability = Type.Strict } }
    | T_DOUBLE               { { Source_type.t = Float Double; nullability = Type.Strict } }
    | T_BOOLEAN              { Source_type.strict Bool }
    | T_DATETIME             { Source_type.strict Datetime }
    | f=text_plain NULL           { { Source_type.t = Text f; nullability = Type.Nullable } }
    | f=text_var NULL             { { Source_type.t = Text f; nullability = Type.Nullable } }
    | T_JSON NULL                 { Source_type.nullable Json }
    | f=blob_plain NULL           { { Source_type.t = Blob f; nullability = Type.Nullable } }
    | T_VARBINARY n=int_arg? NULL { { Source_type.t = Blob (Varbinary n); nullability = Type.Nullable } }
    | t=int_type NULL        { { Source_type.t; nullability = Type.Nullable } }
    | T_FLOAT NULL           { { Source_type.t = Float Single; nullability = Type.Nullable } }
    | T_DOUBLE NULL          { { Source_type.t = Float Double; nullability = Type.Nullable } }
    | T_BOOLEAN NULL         { Source_type.nullable Bool }
    | T_DATETIME NULL        { Source_type.nullable Datetime }

algorithm:
 | INPLACE { }
 | COPY { }
 | INSTANT { }

lock:
 | NONE {}
 | EXCLUSIVE {}
 | DEFAULT {}
 | SHARED {}

%inline located(X): X { make_located ~value:$1 ~pos:($startofs, $endofs) }
