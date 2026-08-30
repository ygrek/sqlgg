open Sqlgg
open Printf
open Linol_lsp.Types

module Priority = struct
  type t =
    | Parameter
    | Cte
    | Qualified_column
    | Column
    | Table of { fits : bool; in_query : bool }
    | Source
    | Function
    | Keyword

  let order = function
    | Parameter | Cte | Qualified_column -> 0
    | Column | Table { fits = true; in_query = false } -> 1
    | Table { fits = false; in_query = false } | Source -> 2
    | Table { fits = true; in_query = true } | Function -> 3
    | Table { fits = false; in_query = true } -> 4
    | Keyword -> 9
end

type item = {
  label : string;
  detail : string;
  kind : CompletionItemKind.t;
  priority : Priority.t;
}

let at ?cache ~path text offset =
  let hole = "sqlgg__completion_hole" in
  let column_items ~priority (owner : Symbol.t) =
    Symbol.columns owner |> List.map (fun (attr : Sql.attr) ->
      { label = attr.name; detail = sprintf "%s — %s" (Sql.Type.show attr.domain) owner.name;
        kind = Field; priority })
  in
  let listing_items ~priority ~kind ~what =
    List.map (fun (symbol : Symbol.t) ->
      { label = symbol.name; kind; priority = priority symbol;
        detail = sprintf "%s — %d columns" what (List.length symbol.columns) })
  in
  let function_item name =
    { label = name; detail = "function"; kind = Function; priority = Priority.Function }
  in
  let offset = Pos.valid_offset text offset in
  let document = Document.analyze ?cache ~path text in
  let items = Document.items document in
  let item =
    List.find_map (fun (item : Document.item) ->
      if Pos.covers item.block.pos offset then Some item else None) items
  in
  let stmt = Option.map (fun (item : Document.item) -> item.block) item in
  let ((start, stop) as replace) =
    let range base (lexeme : Recover_parser.lexeme) =
      let ((start, stop) as pos) = Pos.shift base lexeme.pos in
      if not (Pos.covers pos offset) then None
      else
        match lexeme.token with
        | PARAM { value = Some _; _ } -> Some (start + 1, stop)
        | IDENT _ | TYPE _ -> Some pos
        | token when Sql_lexer.is_keyword token -> Some pos
        | _ -> None
    in
    Option.bind stmt (fun (stmt : Statements.t) ->
      List.find_map (range (fst stmt.pos)) (Recover_parser.tokens stmt.text))
    |> Option.value ~default:(offset, offset)
  in
  let opaque =
    Statements.lexemes text
    |> Seq.take_while (fun ((start, _), _) -> start <= offset)
    |> Seq.exists (fun (pos, lexeme) ->
      match lexeme with
      | `Literal | `Open_literal | `Comment | `Props _ | `Bad_props -> Pos.contains pos offset
      | `Text | `Blank | `Semicolon -> false)
  in
  if opaque then replace, []
  else
    let stmt : Statements.t =
      match stmt with
      | None -> { text = hole; props = []; pos = (start, start + String.length hole); metadata = []; comments = []; errors = [] }
      | Some stmt ->
        let base = fst stmt.pos in
        let (start, stop) = start - base, stop - base in
        let delta = String.length hole - (stop - start) in
        { stmt with
          text = String.sub stmt.text 0 start ^ hole ^ String.sub stmt.text stop (String.length stmt.text - stop);
          pos = (base, snd stmt.pos + delta);
          metadata = List.map (fun (offset, meta) -> (if offset >= stop then offset + delta else offset), meta) stmt.metadata }
    in
    let current = Document.check_at document stmt in
    let stmts =
      current :: List.filter_map (fun (item : Document.item) ->
        if Pos.covers item.stmt.pos offset then None else Some item.stmt) items
    in
    let base = fst stmt.pos and sql = stmt.text in
    let hole_start = start - base in
    let run = Recover_parser.run sql hole_start in
    let full = Recover_parser.run sql (String.length sql) in
    let hole_end = hole_start + String.length hole in
    let next =
      List.find_map (fun (lexeme : Recover_parser.lexeme) ->
        if fst lexeme.pos >= hole_end then Some lexeme.token else None)
        full.trace.seen
    in
    let slot = Recover_parser.slot ?next run in
    let replace =
      match slot with
      | Parameter sigil -> base + sigil, stop
      | Name _ | Column_of _ -> replace
    in
    let tables = Document.index document in
    let source_scope =
      Option.bind item (fun (item : Document.item) ->
        match Document.select_scope_at item.stmt offset with
        | Some _ as scope -> scope
        | None ->
          match item.stmt.scope.symbols with
          | [] -> None
          | _ :: _ -> Some item.stmt.scope)
    in
    let names =
      List.filter_map (fun (lexeme : Recover_parser.lexeme) ->
        Recover_parser.ident_name lexeme.token) full.trace.seen
    in
    let (recovery_sources, sources) =
      let trace_tables = List.filter_map (Symbol.find tables) full.trace.tables in
      let recovery_tables = if full.trace.recovery then List.filter_map (Symbol.find tables) names else [] in
      let alias_sources =
        full.trace.sources |> List.filter_map (fun (src, (alias : Sql.source_alias option)) ->
          match src, alias with
          | `Table (table : Sql.table_name), Some alias ->
            Option.map (Symbol.rename alias.table_name.value.tn) (Symbol.find tables table.tn)
          | `Table _, None | (`Select _ | `Nested _ | `ValueRows _), _ -> None)
      in
      let visible sources =
        List.filter (fun (symbol : Symbol.t) ->
          not (String.equal symbol.name hole)) sources
        |> Symbol.unique
      in
      let recovery_sources =
        visible
          (current.scope.symbols @ trace_tables @ recovery_tables @ alias_sources)
      in
      let sources =
        Option.fold ~none:recovery_sources
          ~some:(fun (scope : Document.scope) -> visible scope.symbols)
          source_scope
      in
      recovery_sources, sources
    in
    let functions = Sql.Function.names () in
    let role = function
      | Recover_parser.Table_name ->
        let has_column symbol name = Option.is_some (Symbol.find_column symbol name) in
        let names = List.filter (fun name -> List.exists (fun symbol -> has_column symbol name) tables) names in
        let priority (symbol : Symbol.t) =
          Priority.Table {
            fits = List.for_all (has_column symbol) names;
            in_query = Option.is_some (Symbol.find recovery_sources symbol.name);
          }
        in
        let ctes =
          sources
          |> List.filter (fun (symbol : Symbol.t) -> match symbol.kind with Cte -> true | Table | Local -> false)
          |> List.map (fun (symbol : Symbol.t) ->
            { label = symbol.name; detail = "CTE in this statement"; kind = Interface; priority = Priority.Cte })
        in
        ctes @ listing_items ~priority ~kind:Struct ~what:"table" tables
      | Column_name -> List.concat_map (column_items ~priority:Priority.Column) sources
      | Qualifier -> listing_items ~priority:(Fun.const Priority.Source) ~kind:Module ~what:"source" sources
      | Function_name ->
        functions |> List.filter (fun name -> not (Sql_lexer.Keywords.mem name Sql_lexer.keywords)) |> List.map function_item
    in
    let items =
      match slot with
      | Parameter _ ->
        let module Names = Hashtbl.Make (struct include String let hash = Hashtbl.hash end) in
        let seen = Names.create 64 in
        let fresh name = not (Names.mem seen name) && (Names.add seen name (); true) in
        List.to_seq stmts
        |> Seq.concat_map (fun stmt -> Params.all_nodes (Document.params stmt))
        |> Seq.filter_map (fun (node : Params.node) ->
          match node.kind with Var (id, _) -> id.value | Branch _ -> None)
        |> Seq.append (List.to_seq full.trace.seen
          |> Seq.filter_map (fun (lexeme : Recover_parser.lexeme) ->
          match lexeme.token with PARAM { value = Some name; _ } -> Some name | _ -> None))
        |> Seq.filter (fun name -> not (String.equal name hole) && fresh name)
        |> List.of_seq
        |> List.map (fun name ->
          { label = "@" ^ name; detail = "parameter"; kind = Variable; priority = Priority.Parameter })
      | Column_of q -> Option.fold ~none:[] ~some:(column_items ~priority:Priority.Qualified_column) (Symbol.find sources q)
      | Name roles ->
        let is_type = function Sql_tokens.TYPE _ -> true | _ -> false in
        let keywords =
          Sql_lexer.Keywords.to_seq Sql_lexer.keywords
          |> Seq.filter (fun (_, token) ->
            not (is_type token) && Recover_parser.accepts run token)
          |> Seq.map (fun (keyword, _) ->
            if List.exists (String.equal keyword) functions then function_item keyword
            else
              { label = String.uppercase_ascii keyword; detail = "keyword";
                kind = Keyword; priority = Priority.Keyword })
          |> List.of_seq
        in
        List.concat_map role roles @ keywords
        |> Prelude.unique_by (module String) (fun item -> item.label)
    in
    replace, items
