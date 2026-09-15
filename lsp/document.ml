open Sqlgg

type error = {
  pos : Sql.Pos.t;
  msg : string;
}

type checked = {
  kind : Stmt.kind;
  schema : Sql.schema;
  params : Params.node list;
  dialect_errors : error list;
  new_table : Symbol.t option;
}

type scope = {
  sources : Symbol.t list;
  ctes : Symbol.t list;
}

type analysis = {
  scope : scope;
  select_scopes : (scope * Sql.Pos.t) list;
  exprs : (Sql.Type.t * Sql.Pos.t) list;
  result_aliases : (Sql.attr * Sql.Pos.t) list;
}

type outcome =
  | Verbatim
  | Rejected of error
  | Checked of checked

type stmt = {
  pos : Sql.Pos.t;
  name : string option;
  props_errors : error list;
  tokens : Recover_parser.lexeme list Lazy.t;
  analysis : analysis;
  outcome : outcome;
}

let symbols scope = scope.sources @ scope.ctes

let errors stmt =
  stmt.props_errors
  @ match stmt.outcome with
    | Verbatim -> []
    | Rejected err -> [ err ]
    | Checked checked -> checked.dialect_errors

let params stmt =
  match stmt.outcome with
  | Checked checked -> checked.params
  | Verbatim | Rejected _ -> []

let exprs stmt = stmt.analysis.exprs
let result_aliases stmt = stmt.analysis.result_aliases

let select_scope stmt offset =
  let scopes = List.to_seq stmt.analysis.select_scopes in
  let innermost = Sql.Pos.find_innermost_by_opt Sql.Pos.covers offset scopes in
  Option.map fst innermost

let enclosing_scope stmt offset =
  Option.value (select_scope stmt offset) ~default:stmt.analysis.scope

let statement_scope stmt = symbols stmt.analysis.scope
let scope stmt offset = symbols (enclosing_scope stmt offset)
let sources stmt offset =
  let scope = enclosing_scope stmt offset in
  scope.sources
let select_scope_opt stmt offset = Option.map symbols (select_scope stmt offset)

type checked_statement = {
  block : Statements.t;
  stmt : stmt;
}

type t = {
  path : string;
  text : string;
  statements : checked_statement array;
  index : Symbol.t Symbol.Index.t;
  snapshot : Compile.state;
}

let find_statement t offset =
  Array.find_opt
    (fun (statement : checked_statement) -> Sql.Pos.covers statement.block.pos offset)
    t.statements

let find_reusable_opt t name =
  Seq.find_map (fun (statement : checked_statement) ->
    match statement.stmt.name, Props.include_ statement.block.props with
    | Some stmt_name, (Reuse | Reuse_and_execute) ->
      if String.equal stmt_name name
      then Some (statement.stmt, Symbol.loc ~file:t.path statement.stmt.pos)
      else None
    | None, _ | Some _, Execute -> None)
    (Array.to_seq t.statements)

let assoc_string name l =
  List.find_map (fun (key, x) -> if String.equal key name then Some x else None) l

let check ~file (stmt : Statements.t) =
  let tn (name : Sql.table_name) = name.tn in
  let props_errors = List.map (fun (pos, msg) -> { pos; msg }) stmt.errors in
  let base = fst stmt.pos in
  let rebase (start, stop) =
    Sql.Pos.shift base
      (Line_index.clamp_offset stmt.text start,
       Line_index.clamp_offset stmt.text stop)
  in
  let nonempty pos = if Sql.Pos.is_empty pos then None else Some pos in
  let error_pos pos = let (start, stop) = rebase pos in start, Int.max stop (start + 1) in
  let loc pos = Option.map (fun pos -> Symbol.loc ~file (rebase pos)) (nonempty pos) in
  let recover_scope exn =
    let of_table table : Sql.nested = (`Table table, None), [] in
    let cross : Sql.source list -> Sql.nested option = function
      | [] -> None
      | src :: rest ->
        let join src = Sql.dummy_loc (src, Sql.dummy_loc Sql.Schema.Join.Inner, Sql.Schema.Join.Default) in
        Some (src, List.map join rest)
    in
    let scope ?cte from =
      try
        let annotations = Syntax.scope_of ?cte from in
        annotations.src_tbls, annotations.cte_defs, annotations.table_aliases
      with
      | Out_of_memory as exn -> raise exn
      | _ -> [], [], []
    in
    let recovery_scope sql =
      let run = Recover_parser.run sql (String.length sql) in
      scope (cross run.trace.sources)
    in
    let from_stmt : Sql.stmt -> _ = function
      | Sql.Select { select_complete = { select = (core, _); _ }; cte }
      | Sql.Insert { action = `Select (_, { select_complete = { select = (core, _); _ }; cte }); _ } -> scope ?cte core.from
      | Sql.Update (table, _, _, _, _) | Sql.Delete (table, _) -> scope (Some (of_table table))
      | Sql.UpdateMulti (from, _, _, _, _) -> scope (Some from)
      | Sql.DeleteMulti (_, tables, _) -> scope (Some tables)
      | Sql.Insert { action = (`Set _ | `Values _ | `Param _); _ }
      | Sql.Create _ | Sql.Drop _ | Sql.Alter _ | Sql.Rename _ | Sql.CreateIndex _ | Sql.Set _
      | Sql.CreateRoutine _ | Sql.CreateType _ | Sql.DropType _
      | Sql.CreateExtension _ | Sql.DropExtension _ -> [], [], []
    in
    match exn with
    | Parser_utils.Error _ -> recovery_scope stmt.text
    | _ ->
      match Parser.parse_stmt stmt.text with
      | exception Parser_utils.Error _ -> recovery_scope stmt.text
      | { Parser.stmt; _ } -> from_stmt stmt
  in
  let new_table table_defs =
    List.find_map (fun ((name : Sql.table_name Sql.located), columns) ->
        let positions = List.map (fun (col : string Sql.located) -> col.value, col.pos) columns in
        let column (attr : Sql.attr) =
          Symbol.column ?loc:(Option.bind (assoc_string attr.name positions) loc) attr
        in
        Some (Symbol.make ~name:(tn name.value) ~kind:Table ?loc:(loc name.pos)
          (List.map column (Tables.get_schema name.value))))
      table_defs
  in
  let error_of_exn exn =
    let (pos, exn) =
      match exn with
      | Parser_utils.Error (Sql_lexer.Error (_, pos) as exn, _) -> nonempty pos, exn
      | Parser_utils.Error (exn, { pos; _ }) -> Some pos, exn
      | Prelude.At (pos, exn) -> nonempty pos, exn
      | exn -> None, exn
    in
    { pos =
        error_pos
          (Option.value ~default:(0, String.length stmt.text) pos);
      msg = Parser_utils.message_of_exn exn }
  in
  let dialect_errors (result : Syntax.result) =
    let dialect = !Dialect.selected in
    result.dialect_features |> List.filter_map (fun (ds : Dialect.dialect_support) ->
      let error msg = Some { pos = error_pos ds.pos; msg } in
      match Dialect.support ds dialect with
      | `Supported -> None
      | `Unknown -> error (Dialect.unknown_message ds dialect)
      | `Unsupported -> error (Dialect.unsupported_message ds dialect))
  in
  let dynamic_select = Option.value ~default:Props.Off (Props.dynamic_select stmt.props) in
  let no_annotations : Syntax.stmt_annotations =
    { src_tbls = []; cte_defs = []; table_aliases = []; table_defs = [];
      expr_types = []; result_aliases = []; select_scopes = [] }
  in
  let success (result : Syntax.result) =
    `Checked { kind = result.kind;
               schema = Sql.schema_of_columns result.schema;
               params = Params.of_vars ~base result.vars;
               dialect_errors = dialect_errors result;
               new_table = new_table result.annotations.table_defs },
    result.annotations
  in
  let compile () =
    match Compile.statement ~dynamic_select stmt with
    | Compile.Verbatim -> `Verbatim, no_annotations
    | Compile.Reusable parsed -> success (Syntax.eval_parsed stmt.text parsed)
    | Compile.Not_reusable ->
      `Rejected { pos = stmt.pos; msg = "include=reuse requires a SELECT statement" }, no_annotations
    | Compile.Executable result -> success result
  in
  let (compiled, annotations) =
    match compile () with
    | compiled -> compiled
    | exception (Out_of_memory as exn) -> raise exn
    | exception exn ->
      let (src_tbls, cte_defs, table_aliases) = recover_scope exn in
      `Rejected (error_of_exn exn), { no_annotations with src_tbls; cte_defs; table_aliases }
  in
  let { Syntax.src_tbls; cte_defs; table_aliases; expr_types;
        result_aliases; _ } = annotations in
  let ctes = List.map (fun ((cte : Sql.table Sql.located), columns) -> cte.value, columns) cte_defs in
  let find_cte_opt name =
    List.find_opt (fun ((table, _), _) -> String.equal (tn table) name) ctes
  in
  let definitions =
    List.map (fun ((cte : Sql.table Sql.located), _) -> tn (fst cte.value), cte.pos) cte_defs
    @ List.map (fun ({ alias; _ } : Sql.table_alias) ->
      tn alias.value, alias.pos) table_aliases
  in
  let make_scope src_tbls (table_aliases : Sql.table_alias list) cte_tables =
    let find_alias_opt name =
      List.find_opt
        (fun ({ alias; _ } : Sql.table_alias) -> String.equal (tn alias.value) name)
        table_aliases
    in
    let symbol (table, schema) =
      let name = tn table in
      let cte = find_cte_opt name in
      let (kind, definition) =
        match cte, find_alias_opt name with
        | Some _, _ -> Symbol.Cte, cte
        | None, Some { target = Some target; _ } -> Symbol.Alias target, find_cte_opt (tn target)
        | None, Some { target = None; _ } -> Symbol.Derived, None
        | None, None -> Symbol.Table, None
      in
      let columns =
        match definition with
        | Some (_, positions) when Int.equal (List.compare_lengths positions schema) 0 ->
          List.map2 (fun attr pos -> Symbol.column ?loc:(loc pos) attr) schema positions
        | Some _ | None -> List.map Symbol.column schema
      in
      Symbol.make ~name ~kind
        ?loc:(Option.bind (assoc_string name definitions) loc) columns
    in
    let sources = Symbol.unique (List.map symbol src_tbls) in
    let ctes =
      List.map symbol cte_tables
      |> List.filter (fun (cte : Symbol.t) ->
        Option.is_none (Symbol.find_opt sources cte.name))
      |> Symbol.unique
    in
    { sources; ctes }
  in
  let target_tables =
    match compiled with
    | `Checked { kind; _ } -> List.filter_map Tables.find (Stmt.dml_tables kind)
    | `Verbatim | `Rejected _ -> []
  in
  let scope =
    make_scope (target_tables @ src_tbls) table_aliases (List.map fst ctes)
  in
  let select_scopes =
    List.map (fun (select_scope : Syntax.select_scope) ->
      let scope =
        make_scope select_scope.src_tbls select_scope.table_aliases
          select_scope.cte_tables
      in
      scope, rebase select_scope.pos)
      annotations.select_scopes
  in
  let rebase_types types =
    List.filter_map (fun (expr : Sql.Type.t Sql.located) ->
      Option.map (fun pos -> expr.value, rebase pos) (nonempty expr.pos))
      types
  in
  let analysis =
    { scope; select_scopes; exprs = rebase_types expr_types;
      result_aliases =
        List.filter_map (fun (alias : Sql.attr Sql.located) ->
          Option.map (fun pos -> alias.value, rebase pos)
            (nonempty alias.pos))
          result_aliases }
  in
  let outcome =
    match compiled with
    | `Verbatim -> Verbatim
    | `Rejected err -> Rejected err
    | `Checked checked -> Checked checked
  in
  { pos = stmt.pos; name = Props.name stmt.props; props_errors;
    tokens = lazy (Recover_parser.tokens stmt.text); analysis; outcome }

module Cache = struct
  module Schema_key = struct
    type t = Dialect.t * string list [@@deriving eq, hash]
  end

  module Schemas = Hashtbl.Make (Schema_key)

  type project = { stamps : float list; project : Project.t }

  type schema = {
    stamps : float list;
    snapshot : Compile.state;
    index : Symbol.t Symbol.Index.t;
  }

  type document = { schema : schema; result : t }

  type t = {
    projects : (string, project) Hashtbl.t;
    schemas : schema Schemas.t;
    documents : (string, document) Hashtbl.t;
  }

  let create () = { projects = Hashtbl.create 4; schemas = Schemas.create 4; documents = Hashtbl.create 4 }

  let forget cache path = Hashtbl.remove cache.documents path
end

let analyze ?(cache = Cache.create ()) ~path text =
  let apply ~file index blocks =
    List.fold_left_map (fun index block ->
      let stmt = check ~file block in
      let statement = { block; stmt } in
      match stmt.outcome with
      | Checked { new_table = Some table; _ } ->
        Symbol.Index.add table.name table index, statement
      | Checked { new_table = None; _ } | Verbatim | Rejected _ ->
        index, statement) index blocks
  in
  let mtime file =
    try
      let stat = Unix.stat file in
      stat.st_mtime
    with Unix.Unix_error _ -> 0.
  in
  let { Project.schema_files; dialect; _ } =
    match Project.locate path with
    | None -> Project.default
    | Some config ->
      match Hashtbl.find_opt cache.projects config with
      | Some entry when List.equal Float.equal entry.stamps (List.map mtime entry.project.watch_paths) -> entry.project
      | Some _ | None ->
        let project = Project.load config in
        Hashtbl.replace cache.projects config { Cache.stamps = List.map mtime project.watch_paths; project };
        project
  in
  Dialect.set_selected dialect;
  let same_file a b =
    let id file =
      match Unix.stat file with
      | { st_dev; st_ino; _ } -> Some (st_dev, st_ino)
      | exception Unix.Unix_error _ -> None
    in
    match id a, id b with
    | Some (dev, ino), Some (dev', ino') -> Int.equal dev dev' && Int.equal ino ino'
    | Some _, None | None, Some _ | None, None -> String.equal a b
  in
  let files = List.filter (fun file -> not (same_file file path)) schema_files in
  let stamps = List.map mtime files in
  let schema =
    match Cache.Schemas.find_opt cache.schemas (dialect, files) with
    | Some entry when List.equal Float.equal entry.stamps stamps -> entry
    | Some _ | None ->
      Compile.reset ();
      let load index file =
        match In_channel.with_open_bin file In_channel.input_all with
        | exception Sys_error _ -> index
        | text ->
          let blocks = Statements.glue_downs (Statements.split text) in
          let (index, _) = apply ~file index blocks in
          index
      in
      let index = List.fold_left load Symbol.Index.empty files in
      let schema = { Cache.stamps; snapshot = Compile.snapshot (); index } in
      Cache.Schemas.replace cache.schemas (dialect, files) schema;
      schema
  in
  match Hashtbl.find_opt cache.documents path with
  | Some doc when String.equal doc.result.text text && doc.schema == schema -> doc.result
  | Some _ | None ->
    Compile.restore schema.snapshot;
    let blocks = Statements.split text in
    let (index, statements) = apply ~file:path schema.index blocks in
    let result =
      { path; text; statements = Array.of_list statements; index; snapshot = schema.snapshot }
    in
    Hashtbl.replace cache.documents path { schema; result };
    result

let recheck result (stmt : Statements.t) =
  Compile.restore result.snapshot;
  Array.to_seq result.statements
  |> Seq.take_while (fun statement -> snd statement.block.pos <= fst stmt.pos)
  |> Seq.iter (fun statement ->
    ignore (check ~file:result.path statement.block));
  check ~file:result.path stmt

