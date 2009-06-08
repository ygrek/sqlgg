/*
  Simple SQL parser
*/


%{
  open Printf
  open Sql
  open Sql.Type
  open ListMore
  open Stmt
  open Syntax
  open Operators

  let def_param_name name (id,t) =
    let name =
    match id with
    | Next | Numbered _ -> name
    | Named x -> x
    in
    (Named name,t)

  let params_of select = List.map (fun x -> `Param x) (snd select)

  let select_value select =
    let (s,p) = select in
    if (List.length s <> 1) then
      raise (RA.Scheme.Error (s,"only one column allowed for SELECT operator in this expression"));
    params_of select

%}

%token <int> INTEGER
%token <string> IDENT TEXT BLOB
%token <float> FLOAT
%token <Stmt.param_id> PARAM
%token <Sql.Type.t option> FUNCTION
%token LPAREN RPAREN COMMA EOF DOT NULL
%token CONFLICT_ALGO
%token SELECT INSERT OR INTO CREATE UPDATE VIEW TABLE VALUES WHERE ASTERISK DISTINCT ALL ANY SOME
       LIMIT ORDER BY DESC ASC EQUAL DELETE FROM DEFAULT OFFSET SET JOIN LIKE_OP
       EXCL TILDE NOT TEST_NULL BETWEEN AND ESCAPE USING UNION EXCEPT INTERSECT AS
       CONCAT_OP JOIN_TYPE1 JOIN_TYPE2 NATURAL CROSS REPLACE IN GROUP HAVING
       UNIQUE PRIMARY KEY FOREIGN AUTOINCREMENT ON CONFLICT TEMPORARY IF EXISTS
       PRECISION UNSIGNED ZEROFILL VARYING CHARSET NATIONAL ASCII UNICODE COLLATE BINARY CHARACTER
       GLOBAL LOCAL VALUE REFERENCES CHECK CONSTRAINT
%token NUM_BINARY_OP PLUS MINUS COMPARISON_OP
%token T_INTEGER T_BLOB T_TEXT T_FLOAT T_BOOLEAN T_DATETIME

(*
%left COMMA_JOIN
%left JOIN_JOIN
*)
(* FIXME precedence of COMMA and JOIN *)

%left TEST_NULL
%left AND OR
%nonassoc EQUAL
%nonassoc NUM_BINARY_OP
%left PLUS MINUS
%left ASTERISK

%type <Syntax.expr> expr

%start <RA.Scheme.t * Stmt.params * Stmt.kind> input

%%

input: statement EOF { $1 }

if_not_exists: IF NOT EXISTS { }
temporary: either(GLOBAL,LOCAL)? TEMPORARY { }

statement: CREATE ioption(temporary) TABLE ioption(if_not_exists) name=IDENT
           table_def=sequence(column_def1)
              {
                let schema = List.filter_map (function `Attr a -> Some a | `Constraint _ -> None) table_def in
                let () = Tables.add (name,schema) in
                ([],[],Create name)
              }
        | CREATE either(TABLE,VIEW) name=IDENT AS select=select_stmt
              {
                let (s,p) = select in
                Tables.add (name,s);
                ([],p,Create name)
              }
         | select_stmt
              { let (s,p) = $1 in s,p,Select }
         | insert_cmd table=IDENT cols=sequence(IDENT)? VALUES (*sequence(expr)?*)
              {
                let s = Tables.get_schema table in
                let s = match cols with
                  | Some cols -> RA.Scheme.project cols s
                  | None -> s
                in
                let p = Syntax.schema_as_params s in
                [],p,Insert table
              }
         | update_cmd table=IDENT SET assignments=separated_nonempty_list(COMMA,set_column) w=where?
              {
                let t = Tables.get table in
                let p2 = get_params_opt [t] (snd t) w in
                let (cols,exprs) = List.split assignments in
                let _ = RA.Scheme.project cols (snd t) in (* validates columns *)
                let p1 = Syntax.get_params_l [t] (snd t) exprs in
                [], p1 @ p2, Update table
              }
         | DELETE FROM table=IDENT w=where?
              {
                let t = Tables.get table in
                let p = get_params_opt [t] (snd t) w in
                [], p, Delete table
              }

select_stmt: select_core list(preceded(compound_op,select_core)) o=loption(order) p4=loption(limit)
              {
                let (s1,p1,tbls) = $1 in
                let (s2l,p2l) = List.split (List.map (fun (s,p,_) -> s,p) $2) in
                (* ignoring tables in compound statements - they cannot be used in ORDER BY *)
                let schema = List.fold_left RA.Scheme.compound s1 s2l in
                let p3 = Syntax.get_params_l tbls schema o in
(*                 RA.Scheme.check_unique schema; *)
                schema,(p1@(List.flatten p2l)@p3@p4)
              }

select_core: SELECT select_type? r=separated_nonempty_list(COMMA,column1)
             FROM t=table_list
             w=where?
             g=loption(group)
             h=having?
              {
                let (tbls,p2,joined_schema) = Syntax.join t in
                let p1 = Syntax.params_of_columns tbls joined_schema r in
                let p3 = Syntax.get_params_opt tbls joined_schema w in
                let p4 = Syntax.get_params_l tbls joined_schema g in
                let p5 = Syntax.get_params_opt tbls joined_schema h in
                (Syntax.infer_schema r tbls joined_schema, p1 @ p2 @ p3 @ p4 @ p5, tbls)
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
       | LPAREN s=select_core RPAREN { let (s,p,_) = s in ("",s),p }

source: src=source1 alias=maybe_as
    {
      match alias with
      | Some name -> let ((n,s),p) = src in ((name,s),p)
      | None -> src
    }

insert_cmd: INSERT OR CONFLICT_ALGO INTO | INSERT INTO | REPLACE INTO { }

update_cmd: UPDATE {}
          | UPDATE OR CONFLICT_ALGO {} ;

select_type: DISTINCT | ALL { }

int_or_param: INTEGER { [] }
            | PARAM { [($1,Some Sql.Type.Int)] }

limit: LIMIT p=int_or_param { p }
     | LIMIT p1=int_or_param COMMA p2=int_or_param { p1 @ p2 } (* Named? *)
     | LIMIT p1=int_or_param OFFSET p2=int_or_param { p1 @ p2 }

order: ORDER BY l=separated_nonempty_list(COMMA,terminated(expr,order_type?)) { l }
order_type: DESC | ASC { }

where: WHERE e=expr { e }
group: GROUP BY l=separated_nonempty_list(COMMA,expr) { l }
having: HAVING e=expr { e }

column1:
       | IDENT DOT ASTERISK { Syntax.AllOf $1 }
       | ASTERISK { Syntax.All }
       | expr maybe_as { let e = $1 in Syntax.Expr (e,$2) }

maybe_as: AS? name=IDENT { Some name }
        | { None }

column_def1: name=IDENT t=sql_type? column_def_extra*
              { `Attr (RA.attr name (match t with Some x -> x | None -> Type.Int)) }
           | pair(CONSTRAINT,IDENT)? c=table_constraint_1 { `Constraint c }

on_conflict: ON CONFLICT CONFLICT_ALGO { $3 }
column_def_extra: PRIMARY KEY { Some Constraint.PrimaryKey }
                | NOT NULL { Some Constraint.NotNull }
                | NULL { None }
                | UNIQUE { Some Constraint.Unique }
                | AUTOINCREMENT { Some Constraint.Autoincrement }
                | on_conflict { None }
                | CHECK LPAREN expr RPAREN
                | DEFAULT INTEGER { None }

(* FIXME check columns *)
table_constraint_1:
      | either(UNIQUE,pair(PRIMARY,KEY)) sequence(IDENT) { [] }
      | UNIQUE LPAREN VALUE RPAREN { [] }
      | FOREIGN KEY cols=sequence(IDENT) REFERENCES table=IDENT fcols=sequence(IDENT)? { [] }
      | CHECK LPAREN e=expr RPAREN { [] }

set_column: name=IDENT EQUAL e=expr { name,e }

(* expr: expr1 { $1 >> Syntax.expr_to_string >> prerr_endline; $1 } *)

anyall: ANY | ALL | SOME { }

mnot(X): NOT x = X | x = X { x }

expr:
     expr numeric_bin_op expr %prec PLUS { `Func ((Some Int),[$1;$3]) }
    | expr boolean_bin_op expr %prec AND { `Func ((Some Bool),[$1;$3]) }
    | e1=expr comparison_op anyall? e2=expr %prec EQUAL { `Func ((Some Bool),[e1;e2]) }
    | expr CONCAT_OP expr { `Func ((Some Text),[$1;$3]) }
    | e1=expr mnot(LIKE_OP) e2=expr e3=escape?
      { `Func (None,(List.filter_valid [Some e1; Some e2; e3])) }
    | unary_op expr { $2 }
    | LPAREN expr RPAREN { $2 }
    | IDENT { `Column ($1,None) }
    | t=IDENT DOT c=IDENT
    | IDENT DOT t=IDENT DOT c=IDENT { `Column (c,Some t) }
    | INTEGER { `Value Int }
    | FLOAT { `Value Float }
    | e1=expr mnot(IN) l=sequence(expr) { `Func (None,e1::l) }
    | e1=expr mnot(IN) LPAREN select=select_stmt RPAREN
      {
        `Func (None,e1::select_value select)
      }
    | e1=expr IN table=IDENT { Tables.check(table); e1 }
    | LPAREN select=select_stmt RPAREN
      {
        `Func (None,select_value select)
      }
    | TEXT { `Value Text }
    | BLOB { `Value Blob }
    | PARAM { `Param ($1,None) }
    | FUNCTION LPAREN func_params RPAREN { `Func ($1,$3) }
    | expr TEST_NULL { $1 }
    | expr mnot(BETWEEN) expr AND expr { `Func ((Some Int),[$1;$3;$5]) }
    | mnot(EXISTS) LPAREN select=select_stmt RPAREN { `Func ((Some Bool),params_of select) }

expr_list: separated_nonempty_list(COMMA,expr) { $1 }
func_params: expr_list { $1 }
           | ASTERISK { [] }
           | (* *) { [] }
escape: ESCAPE expr { $2 }
numeric_bin_op: PLUS | MINUS | ASTERISK | NUM_BINARY_OP { }
comparison_op: EQUAL | COMPARISON_OP { }
boolean_bin_op: AND | OR { }

unary_op: EXCL { }
        | PLUS { }
        | MINUS { }
        | TILDE { }
        | NOT { }

sql_type_flavor: T_INTEGER UNSIGNED? ZEROFILL? { Type.Int }
               | binary { Type.Blob }
               | NATIONAL? text VARYING? charset? collate? { Type.Text }
               | T_FLOAT PRECISION? { Type.Float }
               | T_BOOLEAN { Type.Bool }
               | T_DATETIME { Type.Datetime }

binary: T_BLOB | BINARY | BINARY VARYING { }
text: T_TEXT | CHARACTER { }

%inline either(X,Y): X | Y { }
(* (x1,x2,...,xn) *)
sequence(X): LPAREN l=separated_nonempty_list(COMMA,X) RPAREN { l }

charset: CHARSET either(IDENT,BINARY) | CHARACTER SET either(IDENT,BINARY) | ASCII | UNICODE { }
collate: COLLATE IDENT { }

sql_type: t=sql_type_flavor
        | t=sql_type_flavor LPAREN INTEGER RPAREN
        | t=sql_type_flavor LPAREN INTEGER COMMA INTEGER RPAREN
        { t }

compound_op: UNION ALL? | EXCEPT | INTERSECT { }

maybe_join_type: JOIN_TYPE1? JOIN_TYPE2? { }
