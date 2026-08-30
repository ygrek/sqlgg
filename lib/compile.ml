open Stdlib

type outcome =
  | Executable of Syntax.result
  | Reusable of Parser.parse_result
  | Verbatim
  | Not_reusable

let statement ~(dynamic_select : Props.dynamic_select) (stmt : Statements.t) =
  let sql = stmt.text in 
  let props = stmt.props in
  let eval parse_result = Executable (Syntax.eval_parsed sql parse_result) in
  Syntax.Config.dynamic_select := (match dynamic_select with Off -> false | Only | Both -> true);
  Parser_state.Stmt_metadata.reset ();
  List.iter (fun (offset, meta) -> Parser_state.Stmt_metadata.add offset meta) stmt.metadata;
  match Props.include_ props with
  | Execute when Props.has Noparse props -> Verbatim
  | Execute -> eval (Parser.parse_stmt sql)
  | (Reuse | Reuse_and_execute) as include_ ->
    match Parser.parse_stmt sql with
    | { stmt = Sql.Select select; _ } as parse_result ->
      Shared_queries.add (Option.value ~default:"" (Props.name props)) (sql, select);
      begin match include_ with
      | Reuse -> Reusable parse_result
      | Execute | Reuse_and_execute -> eval parse_result
      end
    | _ -> Not_reusable

type state = {
  tables : Tables.stored_table list;
  types : (string, Sql.Type.kind) Hashtbl.t;
  queries : Shared_queries.t;
  functions : Sql.Function.registry;
}

let snapshot () =
  { tables = Tables.snapshot (); types = User_types.snapshot (); queries = Shared_queries.snapshot ();
    functions = Sql.Function.snapshot () }

let restore { tables; types; queries; functions } =
  Tables.restore tables;
  User_types.restore types;
  Shared_queries.restore queries;
  Sql.Function.restore functions

let reset =
  let initial = snapshot () in
  fun () ->
    restore initial;
    Parser_state.Stmt_metadata.reset ()
