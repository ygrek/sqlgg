open Sqlgg
open Printf
open Linol_lsp.Types

module Rank = struct
  let exact = 0
  let column = 1
  let source = 2
  let function_ = 3
  let mismatched_table = 4
  let keyword = 9

  let table matches_columns in_query =
    match matches_columns, in_query with
    | true, false -> column
    | false, false -> source
    | true, true -> function_
    | false, true -> mismatched_table
end

type item = {
  label : string;
  detail : string;
  kind : CompletionItemKind.t;
  rank : int;
}

let make (document : Document.t) offset =
  let hole = "sqlgg__completion_hole" in
  let column_items ~rank (source : Symbol.t) =
    Symbol.columns source |> List.map (fun (attr : Sql.attr) ->
      { label = attr.name; detail = sprintf "%s — %s" (Sql.Type.show attr.domain) source.name;
        kind = Field; rank })
  in
  let listing_items ~rank ~kind ~kind_name =
    List.map (fun (symbol : Symbol.t) ->
      { label = symbol.name; kind; rank = rank symbol;
        detail = sprintf "%s — %d columns" kind_name (List.length symbol.columns) })
  in
  let function_item name =
    { label = name; detail = "function"; kind = Function;
      rank = Rank.function_ }
  in
  let text = document.text in
  let offset = Line_index.clamp_offset text offset in
  let current_statement = Document.find_statement document offset in
  let stmt =
    Option.map (fun (statement : Document.checked_statement) -> statement.block)
      current_statement
  in
  let ((start, stop) as replace) =
    let range base (lexeme : Recover_parser.lexeme) =
      let ((start, stop) as pos) = Sql.Pos.shift base lexeme.pos in
      if not (Sql.Pos.covers pos offset) then None
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
      | `Literal | `Open_literal | `Comment | `Props _ | `Bad_props ->
        Sql.Pos.contains pos offset
      | `Text | `Blank | `Semicolon -> false)
  in
  if opaque then replace, []
  else
    let stmt : Statements.t =
      match stmt with
      | None ->
        { text = hole;
          pos = (start, start + String.length hole);
          props = [];
          metadata = [];
          comments = [];
          errors = [] }
      | Some stmt ->
        let base = fst stmt.pos in
        let (start, stop) = start - base, stop - base in
        let delta = String.length hole - (stop - start) in
        { stmt with
          text = String.sub stmt.text 0 start ^ hole ^ String.sub stmt.text stop (String.length stmt.text - stop);
          pos = (base, snd stmt.pos + delta);
          metadata =
            List.map (fun (offset, meta) ->
              let adjusted_offset =
                if offset >= stop then offset + delta else offset
              in
              adjusted_offset, meta) stmt.metadata }
    in
    let current = Document.recheck document stmt in
    let other_statement (statement : Document.checked_statement) =
      if Sql.Pos.covers statement.stmt.pos offset
      then None
      else Some statement.stmt
    in
    let statements =
      Seq.cons current
        (Seq.filter_map other_statement (Array.to_seq document.Document.statements))
    in
    let base = fst stmt.pos in
    let hole_start = start - base in
    let run = Recover_parser.run stmt.text hole_start in
    let full = Recover_parser.run stmt.text (String.length stmt.text) in
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
    let find_symbol name =
      Symbol.Index.find_opt name document.Document.index
    in
    let tables =
      document.Document.index
      |> Symbol.Index.bindings
      |> List.map (fun (_, symbol) -> symbol)
    in
    let source_scope =
      Option.bind current_statement (fun (statement : Document.checked_statement) ->
        match Document.select_scope_opt statement.stmt offset with
        | Some _ as scope -> scope
        | None ->
          match Document.statement_scope statement.stmt with
          | [] -> None
          | _ :: _ as scope -> Some scope)
    in
    let names =
      List.filter_map (fun (lexeme : Recover_parser.lexeme) ->
        Recover_parser.ident_name lexeme.token) full.trace.seen
    in
    let (recovery_sources, sources) =
      let recovery_names =
        if full.trace.recovery then full.trace.tables @ names
        else full.trace.tables
      in
      let recovery_tables = List.filter_map find_symbol recovery_names in
      let alias_sources =
        full.trace.sources |> List.filter_map (fun (src, (alias : Sql.source_alias option)) ->
          match src, alias with
          | `Table (table : Sql.table_name), Some alias ->
            Option.map (fun (symbol : Symbol.t) ->
              { symbol with name = alias.table_name.value.tn; kind = Symbol.Alias table })
              (find_symbol table.tn)
          | `Table _, None | (`Select _ | `Nested _ | `ValueRows _), _ -> None)
      in
      let visible sources =
        List.filter (fun (symbol : Symbol.t) ->
          not (String.equal symbol.name hole)) sources
        |> Symbol.unique
      in
      let recovery_sources =
        visible
          (Document.statement_scope current @ recovery_tables @ alias_sources)
      in
      let sources =
        Option.fold ~none:recovery_sources ~some:visible source_scope
      in
      recovery_sources, sources
    in
    let functions = Sql.Function.names () in
    let role = function
      | Recover_parser.Table_name ->
        let has_column symbol name =
          Option.is_some (Symbol.find_column_opt symbol name)
        in
        let matching_names =
          List.filter (fun name ->
            List.exists (fun symbol -> has_column symbol name)
              tables)
            names
        in
        let rank (symbol : Symbol.t) =
          let matches_columns =
            List.for_all (has_column symbol) matching_names
          in
          let in_query =
            Option.is_some (Symbol.find_opt recovery_sources symbol.name)
          in
          Rank.table matches_columns in_query
        in
        let ctes =
          sources
          |> List.filter (fun (symbol : Symbol.t) ->
            match symbol.kind with Cte -> true | Table | Derived | Alias _ -> false)
          |> List.map (fun (symbol : Symbol.t) ->
            { label = symbol.name; detail = "CTE in this statement";
              kind = Interface; rank = Rank.exact })
        in
        ctes @ listing_items ~rank ~kind:Struct ~kind_name:"table" tables
      | Column_name ->
        List.concat_map (column_items ~rank:Rank.column) sources
      | Qualifier ->
        listing_items ~rank:(Fun.const Rank.source) ~kind:Module
          ~kind_name:"source" sources
      | Function_name ->
        functions |> List.filter (fun name -> not (Sql_lexer.Keywords.mem name Sql_lexer.keywords)) |> List.map function_item
    in
    let completions =
      match slot with
      | Parameter _ ->
        statements
        |> Seq.concat_map (fun stmt -> Params.all_nodes (Document.params stmt))
        |> Seq.filter_map (fun (node : Params.node) ->
          match node.kind with Var _ -> node.param.value | Branch _ -> None)
        |> Seq.append (List.to_seq full.trace.seen
          |> Seq.filter_map (fun (lexeme : Recover_parser.lexeme) ->
          match lexeme.token with PARAM { value = Some name; _ } -> Some name | _ -> None))
        |> Seq.filter (Fun.negate (String.equal hole))
        |> List.of_seq
        |> List.sort_uniq String.compare
        |> List.map (fun name ->
          { label = "@" ^ name; detail = "parameter"; kind = Variable;
            rank = Rank.exact })
      | Column_of q ->
        Option.fold ~none:[]
          ~some:(column_items ~rank:Rank.exact)
          (Symbol.find_opt sources q)
      | Name roles ->
        let keywords =
          Sql_lexer.Keywords.to_seq Sql_lexer.keywords
          |> Seq.filter (fun (_, token) ->
            Option.is_none (Recover_parser.ident_name token)
            && Recover_parser.accepts run token)
          |> Seq.map (fun (keyword, _) ->
            if List.exists (String.equal keyword) functions then function_item keyword
            else
              { label = String.uppercase_ascii keyword; detail = "keyword";
                kind = Keyword; rank = Rank.keyword })
          |> List.of_seq
        in
        List.concat_map role roles @ keywords
        |> Prelude.unique_by ~key:(fun completion -> completion.label)
    in
    replace, completions
