open Stdlib

type outcome =
  | Executable of Syntax.result
  | Reusable of Parser.parse_result
  | Verbatim
  | Not_reusable

let statement ~dynamic_select stmt =
  let dynamic_select_enabled =
    match dynamic_select with Props.Off -> false | Only | Both -> true
  in
  Syntax.Config.dynamic_select := dynamic_select_enabled;
  Parser_state.Stmt_metadata.load stmt.Statements.metadata;
  match Props.include_ stmt.props with
  | Execute when Props.has Noparse stmt.props -> Verbatim
  | Execute ->
    Executable (Syntax.eval_parsed stmt.text (Parser.parse_stmt stmt.text))
  | (Reuse | Reuse_and_execute) as include_ ->
    match Parser.parse_stmt stmt.text with
    | { stmt = Sql.Select select; _ } as parse_result ->
      Shared_queries.add
        (Option.value ~default:"" (Props.name stmt.props))
        (stmt.text, select);
      begin match include_ with
      | Reuse -> Reusable parse_result
      | Execute | Reuse_and_execute ->
        Executable (Syntax.eval_parsed stmt.text parse_result)
      end
    | _ -> Not_reusable

type state = {
  tables : Tables.stored_table list;
  user_types : (string, Sql.Type.kind) Hashtbl.t;
  queries : Shared_queries.t;
  functions : Sql.Function.registry;
}

let snapshot () =
  { tables = Tables.snapshot (); user_types = User_types.snapshot (); queries = Shared_queries.snapshot ();
    functions = Sql.Function.snapshot () }

let restore { tables; user_types; queries; functions } =
  Tables.restore tables;
  User_types.restore user_types;
  Shared_queries.restore queries;
  Sql.Function.restore functions

let reset =
  let initial = snapshot () in
  fun () ->
    restore initial;
    Parser_state.Stmt_metadata.reset ()
