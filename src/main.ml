open ExtLib
open Sqlgg

(** Exception with preserved backtrace *)
exception With_backtrace of exn * Printexc.raw_backtrace

let () = Printexc.(register_printer begin function
  | With_backtrace (exn,bt) when raw_backtrace_length bt = 0 -> Some (to_string exn)
  | With_backtrace (exn,bt) -> Some (Printf.sprintf "%s\nBacktrace:\n%s" (to_string exn) (raw_backtrace_to_string bt))
  | _ -> None
end)

let check_dialect sql dialect_features =
  let open Dialect in
  let dialect = dialect_of_configuration !Sqlgg_config.dialect in
  let position_info ({ pos = (first, last); _ }: dialect_support) = String.slice ~first ~last sql in
  let with_position message position =
    if String.starts_with position ~prefix:"\n" then Printf.sprintf "%s at%s" message position
    else Printf.sprintf "%s at %s" message position
  in
  dialect_features |> List.iter (fun ds ->
    let skip = List.mem ds.feature !Sqlgg_config.no_check_features in
    match support ds dialect, skip with
    | `Supported, _ -> ()
    | `Unsupported, true ->
        Error.warn "%s\n"
          (with_position
             (unsupported_message ~alternatives:false ds dialect ^ ", proceeding anyway")
             (position_info ds))
    | `Unsupported, false ->
        Error.log "%s" (with_position (unsupported_message ds dialect) (position_info ds))
    | `Unknown, true ->
        (match ds.feature with
        | Collation ->
            Error.warn "Assuming custom collation implementation for %s\n"
              (show dialect)
        | _ ->
            Error.warn "%s\n" (with_position (unknown_message ds dialect) (position_info ds)))
    | `Unknown, false ->
        Error.log
          "Unknown dialect support for feature %s with dialect %s at %s, use -no-check=all to skip all checks or -no-check=%s to skip this feature check"
          (show_feature ds.feature) (show dialect) (position_info ds) (feature_to_string ds.feature))

let check_params_mode kind =
  Option.may (Error.log "%s")
    (Gen.params_mode_error ~params_mode:!Gen.params_mode kind)

let compile ~dynamic_select (stmt : Statements.t) =
  let error_tail_length = 32 in
  if Sqlgg_config.debug1 () then Printf.eprintf "------\n%s\n%!" stmt.text;
  try
    Some (Compile.statement ~semantics:(Sqlgg_config.semantics ()) ~dynamic_select stmt)
  with
  | Parser_utils.Error (exn, { pos = (_, stop); token; tail }) ->
    let msg = Parser_utils.message_of_exn exn in
    Error.log "==> %s" stmt.text;
    if Int.equal stop (String.length stmt.text) && String.equal token "" then
      Error.log "Error: %s" msg
    else begin
      let { Line_index.line; character } =
        Line_index.position (Line_index.make stmt.text) stop
      in
      Error.log "Position %u:%u Tokens: %s%s\nError: %s"
        (line + 1) character token
        (String.sub tail 0 (Int.min error_tail_length (String.length tail)))
        msg
    end;
    None
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Error.log "Failed %s: %s"
      (Stdlib.Option.value ~default:"" (Props.name stmt.props)) stmt.text;
    let exn =
      match exn with
      | Prelude.At ((p1, p2), exn) ->
        Error.log "At : %s" (String.sub stmt.text p1 (p2 - p1));
        exn
      | _ -> exn
    in
    raise @@ With_backtrace (exn, bt)

let executable (stmt : Statements.t) =
  Stdlib.Option.bind
    (compile
       ~dynamic_select:
         (Props.resolve_dynamic_select ~default_enabled:!Sqlgg_config.dynamic_select stmt.props)
       stmt)
    (function
      | Compile.Executable syntax ->
        check_dialect syntax.typed.sql syntax.dialect_features;
        check_params_mode syntax.typed.kind;
        Some (Query.of_result stmt.props syntax)
      | Compile.Verbatim | Compile.Reusable _ | Compile.Not_reusable -> None)

let parse_one (stmt : Statements.t) =
  let mode =
    Props.resolve_dynamic_select ~default_enabled:!Sqlgg_config.dynamic_select stmt.props
  in
  match compile ~dynamic_select:mode stmt with
  | None -> []
  | Some Compile.Verbatim -> [ Query.verbatim ~props:stmt.props stmt.text ]
  | Some Compile.Not_reusable ->
    if Sqlgg_config.debug1 () then Error.log "Cannot use shared with non-select statement";
    []
  | Some (Compile.Reusable { dialect_features; _ }) ->
    check_dialect stmt.text dialect_features;
    []
  | Some (Compile.Executable syntax) ->
    check_params_mode syntax.typed.kind;
    let query = Query.of_result stmt.props syntax in
    let queries =
      match
        List.exists (function Sql.DynamicSelect _ -> true | _ -> false) query.vars,
        Props.include_ stmt.props,
        mode
      with
      | false, _, _
      | true, (Reuse | Reuse_and_execute), _
      | true, Execute, (Off | Only) -> [ query ]
      | true, Execute, Both ->
        let props =
          Props.Dynamic_select Off
          :: List.filter_map
               (function
                 | Props.Name name -> Some (Props.Name (name ^ "_static"))
                 | Props.Dynamic_select _ -> None
                 | prop -> Some prop)
               stmt.props
        in
        match compile ~dynamic_select:Props.Off { stmt with props } with
        | Some (Compile.Executable static) -> [ query; Query.of_result props static ]
        | None | Some (Compile.Verbatim | Compile.Reusable _ | Compile.Not_reusable) -> [ query ]
    in
    check_dialect stmt.text syntax.dialect_features;
    queries

let prepare_statements text =
  Statements.split text
  |> List.filter (fun (stmt : Statements.t) ->
    match stmt.errors with
    | [] -> true
    | ((start, stop), msg) :: _ -> Error.log "%s at %d-%d" msg start stop; false)
  |> List.map Statements.strip_comments

let src_of_statement ~file (stmt : Statements.t) =
  { Query.file = Stdlib.Option.value stmt.src.file ~default:file; line = stmt.src.line }

let get_statements ~file ch =
  let stamp =
    match file with
    | None -> fun _ stmts -> stmts
    | Some file ->
      let file_prop = Props.File (Filename.basename file) in
      fun stmt stmts ->
        let src = Some (src_of_statement ~file stmt) in
        List.map (fun (q : Query.t) -> { q with props = file_prop :: q.props; src }) stmts
  in
  prepare_statements (In_channel.input_all ch)
  |> List.concat_map (fun (stmt : Statements.t) ->
    match parse_one stmt with
    | [] -> []
    | first :: _ as stmts ->
      if not (Sql.Schema.is_unique (Sql.schema_of_columns first.schema)) then
        Printf.eprintf "Warning: this SQL statement will produce rowset with duplicate column names:\n%s\n" stmt.text;
      stamp stmt stmts)

let replay_statement stmt =
  let (_ : Query.t option) = executable stmt in
  ()

let replay_sql sql = List.iter replay_statement (prepare_statements sql)

type migration_revert =
  | Irreversible
  | Revert of Gen_migrations.step
  | Missing_down

type migration_block = {
  stmt : Statements.t;
  apply_src : Query.src;
  revert : migration_revert;
}

let migration_blocks ~file text =
  Statements.pair_downs (prepare_statements text)
  |> List.map (fun ((up : Statements.t), down) ->
    let apply_src = src_of_statement ~file up in
    let revert =
      match down, Props.down up.props with
      | Some (down : Statements.t), _ ->
        Revert { sql = [ down.text ]; src = Some (src_of_statement ~file down) }
      | None, _ when Props.has Props.Irreversible up.props -> Irreversible
      | None, Some down -> Revert { sql = [ down ]; src = Some apply_src }
      | None, None -> Missing_down
    in
    { stmt = up; apply_src; revert })

let migration_of_block ~id ({ stmt = block; apply_src; revert } : migration_block) =
  Stdlib.Option.bind (executable block) (fun (stmt : Query.t) ->
    let apply_sql = String.trim block.text in
    let migration revert =
      let props =
        match Props.name stmt.props, id with
        | Some _, (Some _ | None) | None, None -> stmt.props
        | None, Some id ->
          (match Sqlgg_migrations.Migration_id.name id with
           | Some _ ->
             Props.Name (Sqlgg_migrations.Migration_id.to_string id) :: stmt.props
           | None -> stmt.props)
      in
      Some
        { Gen_migrations.props;
          kind = stmt.kind;
          apply = { sql = [ apply_sql ]; src = Some apply_src };
          revert }
    in
    match revert with
    | Irreversible -> migration None
    | Revert step -> migration (Some step)
    | Missing_down -> Error.log "migration has no `down`:\n%s" apply_sql; None)

let with_channel filename f =
  match try Some (open_in filename) with _ -> None with
  | None -> Error.log "cannot open file : %s" filename; f None
  | Some ch -> Std.finally (fun () -> close_in_noerr ch) f (Some ch)
