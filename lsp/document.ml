open Sqlgg

module Project = struct
  type t = {
    schema_files : string list;
    dialect : Dialect.t;
    watch_paths : string list;
  }

  let default = { schema_files = []; dialect = Dialect.MySQL; watch_paths = [] }

  type config = {
    schema : string list [@default []];
    dialect : Dialect.t [@default default.dialect];
  } [@@deriving yojson { strict = false }]

  let locate path =
    let rec find_config dir =
      let config = Filename.concat dir "sqlgg.json" in
      if Sys.file_exists config then Some config
      else
        let parent = Filename.dirname dir in
        if String.equal parent dir then None else find_config parent
    in
    find_config (Filename.dirname path)

  let load config_path =
    let is_glob path = String.exists (function '*' | '?' -> true | _ -> false) (Filename.basename path) in
    let expand path =
      let dir = Filename.dirname path and base = Filename.basename path in
      let matches name =
        let rec loop i j =
          if i = String.length base then j = String.length name
          else
            match base.[i] with
            | '*' -> loop (i + 1) j || (j < String.length name && loop i (j + 1))
            | '?' -> j < String.length name && loop (i + 1) (j + 1)
            | c -> j < String.length name && Char.equal name.[j] c && loop (i + 1) (j + 1)
        in
        loop 0 0
      in
      if not (is_glob path) then
        if Sys.file_exists path then [ path ] else []
      else
        match Sys.readdir dir with
        | exception Sys_error _ -> []
        | entries ->
          Array.to_list entries
          |> List.filter matches
          |> List.sort String.compare
          |> List.map (Filename.concat dir)
    in
    let { schema; dialect } =
      match config_of_yojson (Yojson.Safe.from_file config_path) with
      | Ok config -> config
      | Error _ | exception (Yojson.Json_error _ | Sys_error _) -> { schema = []; dialect = default.dialect }
    in
    let root = Filename.dirname config_path in
    let patterns = List.map (fun path -> if Filename.is_relative path then Filename.concat root path else path) schema in
    { schema_files = List.concat_map expand patterns; dialect;
      watch_paths = config_path :: List.map Filename.dirname (List.filter is_glob patterns) }
end

type error = {
  pos : Pos.t;
  msg : string;
}

type success = {
  kind : Stmt.kind;
  schema : Sql.schema;
  params : Params.node list;
  dialect_errors : error list;
  new_table : Symbol.t option;
}

type outcome =
  | Skip
  | Error of error
  | Ok of success

type scope = {
  symbols : Symbol.t list;
  aliases : Sql.table_alias list;
}

type stmt = {
  pos : Pos.t;
  name : string option;
  scope : scope;
  select_scopes : (scope * Pos.t) list;
  exprs : (Sql.Type.t * Pos.t) list;
  outcome : outcome;
}

let errors stmt = match stmt.outcome with Skip -> [] | Error err -> [ err ] | Ok c -> c.dialect_errors
let params stmt = match stmt.outcome with Ok c -> c.params | Skip | Error _ -> []
let select_scope_at stmt offset =
  Pos.find_innermost_covering offset (List.to_seq stmt.select_scopes)
  |> Option.map fst

let scope_at stmt offset =
  select_scope_at stmt offset |> Option.value ~default:stmt.scope

type item = {
  block : Statements.t;
  stmt : stmt;
}

type t = {
  path : string;
  items : item list;
  index : Symbol.t list;
  snapshot : Compile.state;
}

let items t = t.items
let index t = t.index

let find_reusable t name =
  List.find_map (fun (item : item) ->
    match item.stmt.name, Props.include_ item.block.props with
    | Some stmt_name, (Reuse | Reuse_and_execute) ->
      if String.equal stmt_name name
      then Some (item.stmt, Symbol.loc ~file:t.path item.stmt.pos)
      else None
    | None, _ | Some _, Execute -> None)
    t.items

let check ~file (stmt : Statements.t) =
  let tn (name : Sql.table_name) = name.tn in
  match stmt.errors with
  | (pos, msg) :: _ ->
    { pos; name = None; scope = { symbols = []; aliases = [] }; select_scopes = [];
      exprs = []; outcome = Error { pos; msg } }
  | [] ->
  let sql = stmt.text in
  let base = fst stmt.pos in
  let rebase (start, stop) = Pos.shift base (Pos.valid_offset sql start, Pos.valid_offset sql stop) in
  let nonempty pos = if Pos.is_empty pos then None else Some pos in
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
      let sources = run.trace.sources in
      scope (cross sources)
    in
    let from_stmt : Sql.stmt -> _ = function
      | Sql.Select { select_complete = { select = (core, _); _ }; cte }
      | Sql.Insert { action = `Select (_, { select_complete = { select = (core, _); _ }; cte }); _ } -> scope ?cte core.from
      | Sql.Update (table, _, _, _, _) | Sql.Delete (table, _) -> scope (Some (of_table table))
      | Sql.UpdateMulti (from, _, _, _, _) -> scope (Some from)
      | Sql.DeleteMulti (_, tables, _) -> scope (Some tables)
      | Sql.Insert { action = (`Set _ | `Values _ | `Param _); _ }
      | Sql.Create _ | Sql.Drop _ | Sql.Alter _ | Sql.Rename _ | Sql.CreateIndex _ | Sql.Set _
      | Sql.CreateRoutine _ | Sql.CreateType _ | Sql.DropType _ -> [], [], []
    in
    match exn with
    | Parser_utils.Error _ -> recovery_scope sql
    | _ ->
      match Parser.parse_stmt sql with
      | exception Parser_utils.Error _ -> recovery_scope sql
      | { Parser.stmt; _ } -> from_stmt stmt
  in
  let new_table table_defs =
    List.find_map (fun ((name : Sql.table_name Sql.located), columns) ->
        let positions = List.map (fun (col : string Sql.located) -> col.value, col.pos) columns in
        let column (attr : Sql.attr) =
          Symbol.column ?loc:(Option.bind (Prelude.assoc_string attr.name positions) loc) attr
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
    { pos = error_pos (Option.value ~default:(0, String.length sql) pos); msg = Parser_utils.message_of_exn exn }
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
      expr_types = []; select_scopes = [] }
  in
  let success (result : Syntax.result) =
    let annotations = result.annotations in
    Ok { kind = result.kind;
         schema = Sql.schema_of_columns result.schema;
         params = Params.of_vars ~base result.vars;
         dialect_errors = dialect_errors result;
         new_table = new_table annotations.table_defs },
    annotations
  in
  let (outcome, annotations) =
    match Compile.statement ~dynamic_select stmt with
    | Compile.Verbatim -> Skip, no_annotations
    | Compile.Reusable parsed -> success (Syntax.eval_parsed sql parsed)
    | Compile.Not_reusable ->
      Error { pos = stmt.pos; msg = "include=reuse requires a SELECT statement" }, no_annotations
    | Compile.Executable result -> success result
    | exception (Out_of_memory as exn) -> raise exn
    | exception exn ->
      let (src_tbls, cte_defs, table_aliases) = recover_scope exn in
      Error (error_of_exn exn), { no_annotations with src_tbls; cte_defs; table_aliases }
  in
  let { Syntax.src_tbls; cte_defs; table_aliases; expr_types; _ } = annotations in
  let ctes = List.map (fun ((cte : Sql.table Sql.located), columns) -> cte.value, columns) cte_defs in
  let find_cte name = List.find_opt (fun ((table, _), _) -> String.equal (tn table) name) ctes in
  let definitions =
    List.map (fun ((cte : Sql.table Sql.located), _) -> tn (fst cte.value), cte.pos) cte_defs
    @ List.map (fun ({ alias; _ } : Sql.table_alias) ->
      tn alias.value, alias.pos) table_aliases
  in
  let make_scope src_tbls (table_aliases : Sql.table_alias list) cte_tables =
    let aliases =
      table_aliases
      |> Prelude.unique_by (module String)
        (fun ({ alias; _ } : Sql.table_alias) -> tn alias.value)
    in
    let symbol (table, schema) =
      let name = tn table in
      let cte = find_cte name in
      let definition =
        match cte with
        | Some _ -> cte
        | None ->
          Option.bind (Sql.find_table_alias aliases name)
            (fun table -> find_cte (tn table))
      in
      let columns =
        match definition with
        | Some (_, positions) when List.compare_lengths positions schema = 0 ->
          List.map2 (fun attr pos -> Symbol.column ?loc:(loc pos) attr) schema positions
        | Some _ | None -> List.map Symbol.column schema
      in
      Symbol.make ~name ~kind:(if Option.is_some cte then Cte else Local)
        ?loc:(Option.bind (Prelude.assoc_string name definitions) loc) columns
    in
    { symbols = Symbol.unique (List.map symbol (src_tbls @ cte_tables)); aliases }
  in
  let target_tables =
    match outcome with
    | Ok { kind; _ } -> List.map Tables.get (Stmt.dml_tables kind)
    | Skip | Error _ -> []
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
  { pos = stmt.pos; outcome; scope; select_scopes;
    name = Props.name stmt.props;
    exprs = List.filter_map (fun (expr : Sql.Type.t Sql.located) ->
      Option.map (fun pos -> expr.value, rebase pos) (nonempty expr.pos)) expr_types }

module Cache = struct
  module Schema_key = struct
    type t = Dialect.t * string list [@@deriving eq, hash]
  end

  module Schemas = Hashtbl.Make (Schema_key)

  type project = { stamps : float list; project : Project.t }

  type schema = { stamps : float list; snapshot : Compile.state; index : Symbol.t list }

  type document = { text : string; schema : schema; result : t }

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
      let item = { block; stmt } in
      match stmt.outcome with
      | Ok { new_table = Some table; _ } ->
        table :: List.filter (fun (symbol : Symbol.t) ->
          not (String.equal symbol.name table.name)) index,
        item
      | Ok { new_table = None; _ } | Skip | Error _ -> index, item) index blocks
  in
  let mtime file = try (Unix.stat file).st_mtime with Unix.Unix_error _ -> 0. in
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
  let files = List.filter (Fun.negate (String.equal path)) schema_files in
  let stamps = List.map mtime files in
  let schema =
    match Cache.Schemas.find_opt cache.schemas (dialect, files) with
    | Some entry when List.equal Float.equal entry.stamps stamps -> entry
    | Some _ | None ->
      Compile.reset ();
      let load index file =
        match In_channel.with_open_bin file In_channel.input_all with
        | exception Sys_error _ -> index
        | text -> fst (apply ~file index (Statements.glue_downs (Statements.split text)))
      in
      let index = List.fold_left load [] files in
      let schema = { Cache.stamps; snapshot = Compile.snapshot (); index } in
      Cache.Schemas.replace cache.schemas (dialect, files) schema;
      schema
  in
  match Hashtbl.find_opt cache.documents path with
  | Some doc when String.equal doc.text text && doc.schema == schema -> doc.result
  | Some _ | None ->
    Compile.restore schema.snapshot;
    let blocks = Statements.split text in
    let (index, items) = apply ~file:path schema.index blocks in
    let result = { path; items; index; snapshot = schema.snapshot } in
    Hashtbl.replace cache.documents path { text; schema; result };
    result

let check_at result (stmt : Statements.t) =
  Compile.restore result.snapshot;
  List.to_seq result.items
  |> Seq.take_while (fun item -> snd item.block.pos <= fst stmt.pos)
  |> Seq.iter (fun item -> ignore (check ~file:result.path item.block));
  check ~file:result.path stmt

