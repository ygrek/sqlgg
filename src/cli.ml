(** command-line *)

open Printf
open ExtLib
open Sqlgg

module Cxx = Gen.Make(Gen_cxx)
module Caml = Gen.Make(Gen_caml.Generator)
module Caml_io = Gen.Make(Gen_caml.Generator_io)
module Xml_gen = Gen.Make(Gen_xml)
module Java = Gen.Make(Gen_java)
module CSharp = Gen.Make(Gen_csharp)

(*
  common usecase:
     sqlgg [-gen none] ddl.sql -gen cxx dml.sql
*)

type lang = Cxx | Caml | Caml_io | Xml | Json | Java | CSharp

type output = Lang of lang | Sql_ddl

exception Cli_fatal of string
exception Cli_errors_found
exception Cli_bad_input of string

let fatal fmt = ksprintf (fun s -> raise (Cli_fatal s)) fmt
let bad_input fmt = ksprintf (fun s -> raise (Cli_bad_input s)) fmt

let process_json module_name queries =
  let sqlgg_version =
    match !Sqlgg_config.gen_header with
    | Some (`Full | `Without_timestamp) -> Some Sqlgg_config.version
    | Some `Static | None -> None
  in
  Query.document_to_json
    { sqlgg_version; module_name; dialect = !Dialect.selected; params = !Gen.params_mode;
      queries; tables = Tables.snapshot () }
  |> Yojson.Basic.to_channel ~suf:"\n" stdout

let process_stmts = function
  | Cxx -> Cxx.process
  | Caml -> Caml.process
  | Caml_io -> Caml_io.process
  | Xml -> Xml_gen.process
  | Json -> process_json
  | Java -> Java.process
  | CSharp -> CSharp.process

type recorded_blocks =
  | Function_list_blocks of
      (Sqlgg_migrations.Migration_id.t option * Statements.t) list
  | Typed_plan_blocks of {
      baseline : Sqlgg_migrations.Migration_id.t option;
      blocks : (Sqlgg_migrations.Migration_id.t * Statements.t) list;
    }

let process_migrations lang name migration_output =
  let unsupported lang = failwith (sprintf "migrations not supported for %s" lang) in
  match lang with
  | Caml | Caml_io -> Gen_caml.generate_migrations name migration_output
  | Xml -> Gen_xml.generate_migrations name (Gen_caml.output_migrations migration_output)
  | Json -> unsupported "JSON"
  | Cxx -> unsupported "C++"
  | Java -> unsupported "Java"
  | CSharp -> unsupported "C#"

let params_selection = ref Gen.From_dialect

let select_params selection =
  params_selection := selection;
  Gen.params_mode := Gen.resolve_params_mode ~dialect:None selection

let all_categories = String.concat " " @@ List.map Stmt.show_category Stmt.all_categories

let filter_category cat =
  match !Sqlgg_config.include_category with
  | `All -> true
  | `None -> false
  | `Only l -> List.mem cat l
  | `Except l -> not (List.mem cat l)

let stamp_source filename stmts =
  List.map (fun (stmt : Query.t) -> { stmt with props = Props.File filename :: stmt.props }) stmts

(* parse always runs for its schema-registration side effects, even in -gen none *)
let read_statements = function
  | "-" -> Main.get_statements stdin
  | filename ->
    Main.with_channel filename (Option.map_default Main.get_statements [])
    |> stamp_source (Filename.basename filename)

let filter_stmts ~output stmts =
  match output with
  | None -> []
  | Some _ -> List.filter (fun (stmt : Query.t) -> filter_category (Stmt.category_of_stmt_kind stmt.kind)) stmts

let generate ~output ~name results =
  match output with
  | None | Some Sql_ddl -> ()
  | Some (Lang l) ->
    process_stmts l name (List.mapi Query.named (List.concat results))

let merge_blocks gen ext =
  let numbered, rest =
    List.fold_right
      (fun (block : Statements.t) (numbered, rest) ->
        match Props.id block.props with
        | None -> (numbered, block :: rest)
        | Some raw ->
          (match Sqlgg_migrations.Migration_id.parse raw with
           | Ok id -> ((id, block) :: numbered, rest)
           | Error (`Invalid _) ->
             fatal "bad migration block id %S (expected integer or <timestamp>_<name>)" raw
           | Error (`Too_long _ as error) ->
             fatal "%s" (Sqlgg_migrations.Migration_id.error_to_string error)))
      (gen @ ext) ([], [])
  in
  let seen = Hashtbl.create 16 in
  numbered |> List.iter (fun (id, _) ->
    let open Sqlgg_migrations.Migration_id in
    let text = to_string id in
    if Hashtbl.mem seen text then
      fatal "duplicate migration id %S" text;
    Hashtbl.add seen text ());
  List.map (fun (id, block) -> Some id, block)
    (List.stable_sort
       (fun (a, _) (b, _) ->
         Sqlgg_migrations.Migration_id.compare_order a b)
       numbered)
  @ List.map (fun block -> None, block) rest

let now_stamp () =
  let t = Unix.localtime (Unix.time ()) in
  List.fold_left
    (fun acc x -> Int64.add (Int64.mul acc 100L) (Int64.of_int x))
    (Int64.of_int (t.Unix.tm_year + 1900))
    [t.Unix.tm_mon + 1; t.Unix.tm_mday; t.Unix.tm_hour; t.Unix.tm_min; t.Unix.tm_sec]

let next_base now existing =
  let ts = match now with Some n -> n | None -> now_stamp () in
  List.fold_left
    (fun latest id ->
      match Sqlgg_migrations.Migration_id.next_position id with
      | Ok next -> if Int64.compare latest next >= 0 then latest else next
      | Error `Position_overflow ->
          fatal
            "migration id %s has no following ordinal"
            (Sqlgg_migrations.Migration_id.to_string id))
    ts
    existing

let migration_name_limit ~max_id_length ordinal =
  match Migration_name.limit ~max_id_length ordinal with
  | Ok limit -> limit
  | Error `Ordinal_does_not_fit ->
    fatal
      "-max-migration-id-length %d cannot hold migration ordinal %Ld"
      max_id_length
      ordinal

let assign_ids base migs =
  let entries =
    List.map (fun (mig : Gen_migrations.migration) ->
      match Sqlgg_migrations.Migration_id.make ~ordinal:base ~name:(Props.name mig.props) with
      | Ok id -> id, mig
      | Error error -> fatal "%s" (Sqlgg_migrations.Migration_id.error_to_string error)) migs
  in
  let seen = Hashtbl.create 16 in
  entries |> List.iter (fun (id, _) ->
    let id = Sqlgg_migrations.Migration_id.to_string id in
    if Hashtbl.mem seen id then
      fatal "two migrations get the same id %S. increase -max-migration-id-length" id;
    Hashtbl.add seen id ());
  entries

let entries_to_sql entries =
  entries
  |> List.map (fun (id, (m : Gen_migrations.migration)) ->
    let id = Sqlgg_migrations.Migration_id.to_string id in
    let single label = function
      | [s] -> String.trim s
      | _ ->
        fatal
          "-gen sql: %s of migration %s is multi-statement and cannot be emitted as \
           a single SQL entry; write this step by hand in the extends file" label id
    in
    let up = single "up" m.apply in
    match m.revert with
    | None -> sprintf "-- [sqlgg] generated\n-- [sqlgg] irreversible\n-- [sqlgg] id=%s\n%s;" id up
    | Some revert -> sprintf "-- [sqlgg] generated\n-- [sqlgg] id=%s\n%s;\n%s;" id up (single "down" revert))
  |> String.concat "\n\n"

let read_file_opt path =
  if Sys.file_exists path then
    match Std.input_file path with
    | exception Sys_error msg -> fatal "cannot read %s: %s" path msg
    | s -> let s = String.trim s in if s = "" then None else Some s
  else
    None

let program = Filename.basename Sys.executable_name

let usage_msg =
  String.concat "\n" [
    sprintf "SQL Guided (code) Generator ver. %s" Sqlgg_config.version;
    sprintf "Usage: %s <options> <file.sql> [<file2.sql> ...]" program;
    sprintf "       %s ir <options> [<file.sql> ...]" program;
    sprintf "       %s migrations generate --help" program;
    "Options are:";
  ]

type group = { title : string; opts : (Arg.key * Arg.spec * Arg.doc) list }

let render groups =
  let option (key, _spec, doc) = if doc = "" then None else Some (sprintf "  %s %s" key doc) in
  let group g = sprintf " %s:\n%s" g.title (String.concat "\n" (List.filter_map option g.opts)) in
  String.concat "\n\n" (List.map group groups)

let show_version () = print_endline Sqlgg_config.version

let print_json_schema schema () =
  Yojson.Basic.pretty_to_channel stdout (Jsonkit.Jsonschema.make schema);
  print_newline ();
  exit 0

let select_dialect d =
  Dialect.set_selected d;
  Gen.params_mode :=
    Gen.resolve_params_mode ~dialect:(Some d) !params_selection

let enum_values to_string values =
  String.concat "|" (List.map to_string values)

let alter_lock_values =
  enum_values Sql.alter_lock_to_string Sql.all_of_alter_lock

let alter_algorithm_values =
  enum_values Sql.alter_algorithm_to_string Sql.all_of_alter_algorithm

let parse_alter_option kind values of_string s =
  match of_string s with
  | Some option -> option
  | None -> fatal "unknown ALTER TABLE %s %S (expected %s)" kind s values

let abort_on_errors () = if !Error.errors then raise Cli_errors_found

let fail_on_errors msg =
  if !Error.errors then begin
    Error.logs msg;
    raise Cli_errors_found
  end

type schema_source = From_file of string | From_block of Statements.t

let to_file_sources files = List.map (fun f -> From_file f) files

let read_blocks f = Main.with_channel f (Stdlib.Option.fold ~none:[] ~some:Main.raw_blocks)

let replay_sources sources =
  Compile.reset ();
  let replay origin block =
    try Main.replay_statement block
    with Main.With_backtrace (exn, _) -> fatal "%s: %s" origin (Parser_utils.message_of_exn exn)
  in
  List.iter (function
    | From_file f -> List.iter (replay f) (read_blocks f)
    | From_block block ->
      replay
        (Stdlib.Option.fold ~none:"migration without id" ~some:(sprintf "migration %s")
           (Props.id block.props))
        block) sources

let schema_of_sources sources =
  replay_sources sources;
  abort_on_errors ();
  Tables.snapshot ()

let load_schema files = schema_of_sources (to_file_sources files)

let diff_schema ~naming ~alter_options ~new_tables ~from_ ~to_ =
  let migs =
    try Schema_diff.generate ~naming ~alter_options ~new_tables ~from_ ~to_
    with
    | Gen_migrations.Migration_error msg ->
      fatal "cannot generate migration (write this step manually):\n%s" msg
    | Schema_diff.New_tables_not_created (first, rest) ->
      fatal "the target schema has tables that neither the initial schema nor the migrations create: %s\n\
             Add them to the initial schema, or pass -ddl-as-migration (--ddl-as-migration for \
             sqlgg migrations) to generate CREATE TABLE migrations."
        (String.concat ", "
           (List.sort (List.map Gen_migrations.quote_table_name (first :: rest))))
  in
  begin
    try Schema_diff.replay_migrations ~replay:Main.replay_sql ~from_ ~to_ migs
    with Schema_diff.Verification_failed _ as e ->
      fatal "migration verification failed (write this step manually):\n%s" (Printexc.to_string e)
  end;
  abort_on_errors ();
  migs

type gen_args = {
  output : output option;
  name : string;
  inputs : Query.t list list;
}

type delta_args = {
  name : string;
  target_files : string list;
  now : int64 option;
  max_id_length : int;
  new_tables : Schema_diff.new_tables;
  alter_options : Sql.alter_option list;
}

type diff_args = {
  delta : delta_args;
  base_files : string list;
  output : output option;
}

type migrate_args = {
  delta : delta_args;
  gen_lang : lang;
  initial_files : string list;
  migrations_file : string option;
  extends_file : string option;
}

type materialize_args = { base_files : string list }

type config =
  | Generate of gen_args
  | Diff of diff_args
  | Migrate of migrate_args
  | Materialize_schema of materialize_args

let parse_args () =
  let output = ref None in
  let name = ref "sqlgg" in
  let inputs = ref [] in
  let input_files = ref [] in
  let mode = ref None in
  let base_files = ref [] in
  let target_files = ref [] in
  let initial_files = ref [] in
  let migrations_file = ref None in
  let extends_file = ref None in
  let now = ref None in
  let max_id_length = ref Sqlgg_migrations.Migration_id.max_length in
  let new_tables = ref Schema_diff.Created_by_schema in
  let alter_lock = ref None in
  let alter_algorithm = ref None in
  let files : (string, [ `Open of Query.t list | `Positional ]) Hashtbl.t = Hashtbl.create 4 in
  let canonical = function
    | "-" -> "-"
    | f -> (try Unix.realpath f with Unix.Unix_error _ -> fatal "cannot open file : %s" f)
  in
  let open_input f =
    let key = canonical f in
    if not (Hashtbl.mem files key) then Hashtbl.replace files key (`Open (read_statements f))
  in
  let work s =
    let key = canonical s in
    let stmts =
      match Hashtbl.find_opt files key with
      | Some (`Open stmts) -> (* was used with -open before so we reuse *) stmts
      | Some `Positional | None -> read_statements s
    in
    Hashtbl.replace files key `Positional;
    inputs := filter_stmts ~output:!output stmts :: !inputs
  in
  let select_mode requested =
    match !mode, requested with
    | None, requested -> mode := Some requested
    | Some `Diff, `Diff | Some `Migrate, `Migrate -> ()
    | Some `Diff, `Migrate | Some `Migrate, `Diff ->
      fatal "-migrate and -diff are mutually exclusive"
  in
  let apply_setting (setting : Config_file.setting) =
    match setting with
    | Dialect dialect -> select_dialect dialect
    | No_check features -> Sqlgg_config.set_no_check_features features
    | Write_notnull_null Allow -> Sqlgg_config.allow_write_notnull_null true
    | Write_notnull_null Reject -> Sqlgg_config.allow_write_notnull_null false
    | Header header -> Sqlgg_config.gen_header := header
    | Gen Cxx -> output := Some (Lang Cxx)
    | Gen Caml -> output := Some (Lang Caml)
    | Gen Caml_io -> output := Some (Lang Caml_io)
    | Gen Java -> output := Some (Lang Java)
    | Gen Xml -> output := Some (Lang Xml)
    | Gen Json -> output := Some (Lang Json)
    | Gen Csharp -> output := Some (Lang CSharp)
    | Gen Sql -> output := Some Sql_ddl
    | Gen No_output -> output := None
    | Name module_name -> name := module_name
    | Params selection -> select_params selection
    | Category category -> Sqlgg_config.include_category := category
    | Dynamic_select Both -> Sqlgg_config.dynamic_select := true
    | Dynamic_select Off -> Sqlgg_config.dynamic_select := false
    | Diff_mode -> select_mode `Diff
    | Migrate_mode -> select_mode `Migrate
    | Base file -> base_files := file :: !base_files
    | Target file -> target_files := file :: !target_files
    | Initial file -> initial_files := file :: !initial_files
    | Migrations_file file -> migrations_file := Some file
    | Extends file -> extends_file := Some file
    | Max_migration_id_length length -> max_id_length := length
    | New_tables created -> new_tables := created
    | Alter_algorithm algorithm -> alter_algorithm := Some algorithm
    | Alter_lock lock -> alter_lock := Some lock
    | Open file -> open_input file
    | Input file ->
      input_files := file :: !input_files;
      work file
    | Stdin ->
      input_files := "-" :: !input_files;
      work "-"
  in
  let apply_config file =
    match Config_file.settings file with
    | Error (`Config_file error) -> bad_input "%s" (Config_file.error_to_string error)
    | Ok settings -> List.iter apply_setting settings
  in
  let command_line_actions = ref [] in
  let defer action = command_line_actions := action :: !command_line_actions in
  let defer_setting setting = defer (fun () -> apply_setting setting) in
  let config_file = ref None in
  let select_config file =
    match !config_file with
    | None -> config_file := Some file
    | Some _ -> bad_input "-config may be given only once"
  in
  let parse_gen raw =
    let canonical =
      match String.lowercase_ascii raw with
      | "c++" | "cpp" -> "cxx"
      | "ocaml" | "ml" -> "caml"
      | "c#" | "cs" -> "csharp"
      | "ddl" -> "sql"
      | canonical -> canonical
    in
    match Config_file.gen_of_string canonical with
    | Some gen -> gen
    | None -> failwith (sprintf "Unknown output language: %s" raw)
  in
  let parse_category raw =
    let category name =
      match Stmt.category_of_string name with
      | Some (DDL | DQL | DML | DCL | TCL as category) -> category
      | Some OTHER | None -> failwith @@ sprintf "bad category %S" name
    in
    let normalized = String.lowercase_ascii raw in
    match normalized with
    | "all" -> `All
    | "none" -> `None
    | "" -> failwith "bad category \"\""
    | _ when normalized.[0] = '-' ->
      `Except
        (List.map category
           (String.nsplit (String.slice ~first:1 normalized) ","))
    | _ -> `Only (List.map category (String.nsplit normalized ","))
  in
  let parse_no_check = function
    | "all" -> Dialect.all_of_feature
    | raw ->
      String.nsplit raw ","
      |> List.map (fun name ->
        match Dialect.feature_of_string name with
        | Some feature -> feature
        | None -> failwith (sprintf "Unknown feature: %s" name))
  in
  let groups =
  [
    { title = "Configuration"; opts =
    [
      "-config", Arg.String select_config, "<file> Load defaults from a JSON configuration file. Command-line options override it";
      "-config-json-schema", Arg.Unit (print_json_schema Config_file.document_jsonschema),
        " Print the JSON Schema of the -config file and exit";
    ] };

    { title = "Code generation"; opts =
    [
      "-gen", Arg.String (fun raw -> defer_setting (Gen (parse_gen raw))), "cxx|caml|caml_io|java|xml|json|csharp|sql|none Set output language (default: none)";
      "-name", Arg.String (fun module_name -> defer_setting (Name module_name)), "<identifier> Set output module name (default: sqlgg)";
      "-params", Arg.String (fun raw ->
        match Gen.params_mode_of_string raw with
        | Ok selection -> defer_setting (Params selection)
        | Error message -> failwith message),
        "named|unnamed|oracle|postgresql|none Output query parameters substitution (default: auto-detected from dialect, can be overridden)";
      "-category", Arg.String (fun raw -> defer_setting (Category (parse_category raw))), sprintf "{all|none|[-]<category>{,<category>}+} Only generate code for these specific query categories (possible values: %s)" all_categories;
      "-open", Arg.String (fun file -> defer_setting (Open file)), "<file> Make definitions (schema, reusable queries) from <file> available without generating code for it (unless the file is also given as an input)";
      "-dynamic-select", Arg.Unit (fun () -> defer_setting (Dynamic_select Both)),
        " Generate static and dynamic version for every SELECT (dynamic allows to pick columns per call)";
      "-", Arg.Unit (fun () -> defer_setting Stdin), " Read sql from stdin";
      "-json-schema", Arg.Unit (print_json_schema Query.document_jsonschema),
        " Print the JSON Schema of -gen json output and exit";
    ] };

    { title = "Schema and migrations"; opts =
    [
      "-diff", Arg.Unit (fun () -> defer_setting Diff_mode), " Generate up/down migrations from the diff of -base schema against -target schema";
      "-migrate", Arg.Unit (fun () -> defer_setting Migrate_mode), " All-in-one: diff initial+migrations against target, append the up/down delta to the migrations file and (re)generate code";
      "-base", Arg.String (fun file -> defer_setting (Base file)), "<file> Baseline schema / prior up migrations (repeatable)";
      "-target", Arg.String (fun file -> defer_setting (Target file)), "<file> Target schema (repeatable)";
      "-initial", Arg.String (fun file -> defer_setting (Initial file)), "<file> Baseline DDL for -migrate (repeatable)";
      "-migrations-file", Arg.String (fun file -> defer_setting (Migrations_file file)), "<file> Generated migrations SQL for -migrate (read and appended in place; required only when there is a new delta to record)";
      "-extends", Arg.String (fun file -> defer_setting (Extends file)), "<file> Hand-written migrations SQL, merged with the generated ones by `id` (read-only)";
      "-now", Arg.String (fun raw ->
        match Int64.of_string_opt raw with
        | Some n when Int64.compare n 0L >= 0 ->
          defer (fun () -> now := Some n)
        | Some _ | None -> fatal "bad migration timestamp %S" raw),
        "<YYYYMMDDHHMMSS> Pin the migration id timestamp (default: current clock); ids are <timestamp>_<descriptive_name>";
      "-max-migration-id-length", Arg.Int (fun length -> defer_setting (Max_migration_id_length length)),
        sprintf "<N> Limit generated migration ids to N bytes (default and maximum: %d)"
          Sqlgg_migrations.Migration_id.max_length;
      "-ddl-as-migration", Arg.Unit (fun () -> defer_setting (New_tables Schema_diff.Created_by_migrations)), " Write new tables as CREATE TABLE migrations instead of plain schema DDL";
      "-alter-algorithm", Arg.String (fun raw ->
        defer_setting
          (Alter_algorithm
             (parse_alter_option "algorithm" alter_algorithm_values
                Sql.alter_algorithm_of_string raw))),
        sprintf "%s Add ALGORITHM policy to generated ALTER TABLE statements (MySQL and TiDB only)"
          alter_algorithm_values;
      "-alter-lock", Arg.String (fun raw ->
        defer_setting
          (Alter_lock
             (parse_alter_option "lock" alter_lock_values
                Sql.alter_lock_of_string raw))),
        sprintf "%s Add LOCK policy to generated ALTER TABLE statements (MySQL only)"
          alter_lock_values;
    ] };

    { title = "Dialect and checks"; opts =
    [
      "-dialect", Arg.String (fun raw ->
        match Dialect.of_string raw with
        | Some dialect -> defer_setting (Dialect dialect)
        | None -> failwith (sprintf "Unknown dialect: %s" raw)),
        sprintf "%s Set SQL dialect. Queries can only use its features"
          (enum_values Dialect.to_string Dialect.all);
      "-no-check", Arg.String (fun raw -> defer_setting (No_check (parse_no_check raw))),
        sprintf "{all|<feature>{,<feature>}+} Disable dialect feature checks (possible features: %s)"
          (enum_values Dialect.feature_to_string Dialect.all_of_feature);
      "-allow-write-notnull-null", Arg.Unit (fun () -> defer_setting (Write_notnull_null Allow)), " Accept writing a nullable value into a NOT NULL column, instead of failing (MySQL, TiDB and SQLite only)";
    ] };

    { title = "Generated header"; opts =
    [
      "-no-header", Arg.Unit (fun () -> defer_setting (Header None)),
        " Do not put version header in generated output";
      "-no-header-timestamp", Arg.Unit (fun () -> defer_setting (Header (Some `Without_timestamp))),
        " Do not put timestamp in version header in generated output";
      "-static-header", Arg.Unit (fun () -> defer_setting (Header (Some `Static))), " Only output short static header without version/timestamp";
    ] };

    { title = "Diagnostics"; opts =
    [
      "-show-tables", Arg.Unit (fun () -> defer Tables.print_all), " Show all current tables";
      "-show-table", Arg.String (fun name -> defer (fun () -> Tables.print1 name)), "<name> Show specified table";
      "-debug", Arg.Int (fun level -> defer (fun () -> Sqlgg_config.set_debug_level level)), "<N> Set debug level";
      "-version", Arg.Unit (fun () -> defer show_version), " Show version";
      "-test", Arg.Unit (fun () -> defer Test.run), " Run unit tests";
    ] };
  ]
  in
  let groups =
    let rec regroup aligned = function
      | [] -> []
      | g :: rest ->
        let opts, tail = List.split_nth (List.length g.opts) aligned in
        { g with opts } :: regroup tail rest
    in
    regroup (Arg.align (List.concat_map (fun g -> g.opts) groups)) groups
  in
  let show_help () = print_endline usage_msg; print_endline (render groups); exit 0 in
  let args = List.concat_map (fun g -> g.opts) groups @ [
    "-help", Arg.Unit show_help, " Display this list of options";
    "--help", Arg.Unit show_help, "";
    "--", Arg.Rest (fun file -> defer_setting (Input file)), "";
    "-options-md", Arg.Unit (fun () -> printf "```text\n%s\n```\n" (render groups); exit 0), "";
  ]
  in
  Arg.parse args (fun file -> defer_setting (Input file)) usage_msg;
  if Array.length Sys.argv = 1 then show_help ();
  Stdlib.Option.iter apply_config !config_file;
  List.iter (fun action -> action ()) (List.rev !command_line_actions);
  if !max_id_length > Sqlgg_migrations.Migration_id.max_length then
    fatal "-max-migration-id-length %d is above %d, the width of the migration id column"
      !max_id_length Sqlgg_migrations.Migration_id.max_length;
  begin match !alter_lock, !Dialect.selected with
  | Some _, (Dialect.PostgreSQL | Dialect.SQLite | Dialect.TiDB) ->
    fatal "-alter-lock is only supported for dialect mysql"
  | None, _ | Some _, Dialect.MySQL -> ()
  end;
  begin match !alter_algorithm, !Dialect.selected with
  | Some _, (Dialect.PostgreSQL | Dialect.SQLite) ->
    fatal "-alter-algorithm is only supported for dialects mysql and tidb"
  | None, _ | Some _, (Dialect.MySQL | Dialect.TiDB) -> ()
  end;
  let alter_options =
    Stdlib.Option.to_list
      (Option.map (fun algorithm -> Sql.Alter_algorithm algorithm) !alter_algorithm)
    @ Stdlib.Option.to_list
        (Option.map (fun lock -> Sql.Alter_lock lock) !alter_lock)
  in
  let delta =
    { name = !name;
      target_files = List.rev !target_files;
      now = !now;
      max_id_length = !max_id_length;
      new_tables = !new_tables;
      alter_options }
  in
  let has_open =
    Hashtbl.fold
      (fun _ state found ->
        found || match state with `Open _ -> true | `Positional -> false)
      files false
  in
  if has_open && Stdlib.Option.is_some !mode then
    fatal "-open is not supported with -diff/-migrate";
  if !input_files <> [] && Stdlib.Option.is_some !mode then
    fatal "input files are not supported with -diff/-migrate";
  match !mode with
  | Some `Migrate ->
    let gen_lang =
      match !output with
      | Some (Lang l) -> l
      | Some Sql_ddl -> fatal "-migrate emits migration code, not SQL; use -diff -gen sql for SQL output"
      | None -> fatal "-migrate requires -gen <lang>"
    in
    Migrate {
      delta;
      gen_lang;
      initial_files = List.rev !initial_files;
      migrations_file = !migrations_file;
      extends_file = !extends_file;
    }
  | Some `Diff ->
    Diff { delta; base_files = List.rev !base_files; output = !output }
  | None ->
    if !output = Some Sql_ddl then
      Materialize_schema { base_files = List.rev !base_files }
    else
      Generate {
        output = !output;
        name = !name;
        inputs = List.rev !inputs;
      }

let run_migrate
    ~recorded_blocks
    ({ delta = { name; target_files; now; max_id_length;
                 new_tables; alter_options };
       gen_lang; initial_files; migrations_file; extends_file } : migrate_args) =
  let initial = to_file_sources initial_files in
  let ext = Option.map_default read_blocks [] extends_file in
  let read_recorded () =
    recorded_blocks
      (merge_blocks
         (match migrations_file with
          | Some f when Sys.file_exists f -> read_blocks f
          | Some _ | None -> [])
         ext)
  in
  let before = read_recorded () in
  let taken_ids, before_blocks =
    match before with
    | Function_list_blocks blocks ->
        List.filter_map fst blocks, List.map snd blocks
    | Typed_plan_blocks { baseline; blocks } ->
        Stdlib.Option.to_list baseline @ List.map fst blocks, List.map snd blocks
  in
  let current =
    schema_of_sources (initial @ List.map (fun block -> From_block block) before_blocks)
  in
  let target = load_schema target_files in
  let regenerate recorded =
    replay_sources initial;
    let migration_output =
      match recorded with
      | Function_list_blocks blocks ->
        Gen_caml.Function_list
          (List.filter_map
             (fun (id, block) -> Main.migration_of_block ~id block)
             blocks)
      | Typed_plan_blocks { baseline; blocks } ->
        Gen_caml.Typed_plan
          {
            baseline =
              Option.map Sqlgg_migrations.Migration_id.to_string baseline;
            migrations =
              List.filter_map
                (fun (id, block) ->
                  Option.map
                    (fun migration ->
                      Sqlgg_migrations.Migration_id.to_string id, migration)
                    (Main.migration_of_block ~id:(Some id) block))
                blocks;
          }
    in
    abort_on_errors ();
    process_migrations gen_lang name migration_output
  in
  let base = next_base now taken_ids in
  let naming = migration_name_limit ~max_id_length base in
  match diff_schema ~naming ~alter_options ~new_tables ~from_:current ~to_:target with
  | [] ->
    regenerate before;
    (match before_blocks with
     | [] -> prerr_endline "nothing to do"
     | _ :: _ ->
       eprintf "nothing new to migrate; regenerated code from %d recorded migration(s)\n"
         (List.length before_blocks))
  | migs ->
    let migrations_file =
      match migrations_file with
      | Some f -> f
      | None -> fatal "%d new migration(s) to record: pass -migrations-file <file>" (List.length migs)
    in
    let entries = assign_ids base migs in
    let sql = entries_to_sql entries in
    let text =
      Option.map_default (fun existing -> existing ^ "\n\n" ^ sql) sql
        (read_file_opt migrations_file)
    in
    Std.output_file ~filename:migrations_file ~text:(text ^ "\n");
    regenerate (read_recorded ());
    eprintf "appended %d migration(s) to %s (id %s)\n"
      (List.length migs) migrations_file
      (String.concat ", "
         (List.map (fun (id, _) -> Sqlgg_migrations.Migration_id.to_string id) entries))

let run_materialize_schema ({ base_files } : materialize_args) =
  let state =
    match base_files with
    | [] -> Tables.snapshot ()
    | files -> load_schema files
  in
  let ddl = Schema_diff.dump state in
  begin 
    try Schema_diff.verify_ddl ~replay:Main.replay_sql state ddl
    with Schema_diff.Verification_failed _ as e ->
      fatal "schema materialization failed verification:\n%s" (Printexc.to_string e)
  end;
  print_endline ddl

let run_diff ({ delta = { name; target_files; now; max_id_length;
                          new_tables; alter_options };
                base_files; output } : diff_args) =
  let from_ = load_schema base_files in
  let to_   = load_schema target_files in
  let base = next_base now [] in
  let naming = migration_name_limit ~max_id_length base in
  let migs = diff_schema ~naming ~alter_options ~new_tables ~from_ ~to_ in
  Tables.restore from_;
  match output with
  | None -> ()
  | Some Sql_ddl ->
    if migs <> [] then print_endline (entries_to_sql (assign_ids base migs))
  | Some (Lang l) ->
    process_migrations l name
      (Gen_caml.Function_list
         (List.map
            (fun (id, (mig : Gen_migrations.migration)) ->
              {
                mig with
                props =
                  Props.Name
                    (Sqlgg_migrations.Migration_id.to_string id)
                  :: mig.props;
              })
            (assign_ids base migs)))

let run_generate ({ output; name; inputs } : gen_args) =
  match inputs with
  | [] -> ()
  | results ->
    fail_on_errors "Errors encountered, no code generated";
    generate ~output ~name results

let run_ir () =
  let read_path = function
    | "-" -> In_channel.input_all stdin
    | path ->
      try In_channel.with_open_bin path In_channel.input_all
      with Sys_error message -> bad_input "cannot read %s: %s" path message
  in
  let schema = ref None in
  let plain_sql = ref false in
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.current := 1;
  Arg.parse
    (Arg.align [
      "-schema",      Arg.String (fun f -> schema := Some f),              "<file> Also check statements against this schema";
      "-plain-sql",   Arg.Set plain_sql,                                   " Reject sqlgg extensions";
      "-json-schema", Arg.Unit (print_json_schema Ir.document_jsonschema), " Print the JSON Schema of the document and exit";
      "-",            Arg.Unit (fun () -> add_file "-"),                   " Read SQL from stdin";
    ])
    add_file
    (String.concat "\n" [
      sprintf "Usage: %s ir <options> [<file.sql> ...]" program;
      "Print the AST of every statement as JSON. Exit 1 if any is invalid, 2 if input is unreadable.";
      "Options are:";
    ]);
  let files = match List.rev !files with [] -> [ "-" ] | files -> files in
  let statements_of_sql =
    match !schema with
    | None -> Ir.parse ~allow_extensions:(not !plain_sql)
    | Some path ->
      match Analysis.context_of_schema_sql (read_path path) with
      | Ok context -> Ir.analyze ~allow_extensions:(not !plain_sql) ~context
      | Error (_, first, rest) ->
        bad_input "schema %s: %s" path
          (String.concat "; " (List.map (fun d -> d.Analysis.message) (first :: rest)))
  in
  let statements = List.concat_map (fun path -> statements_of_sql (read_path path)) files in
  Yojson.Basic.to_channel ~suf:"\n" stdout (Ir.document_to_json { statements });
  if List.exists (fun (statement : Ir.statement) ->
    match statement.resolution with Invalid _ -> true | Parsed _ | Analyzed _ -> false) statements
  then raise Cli_errors_found

let run_migrations () =
  match
    Migrations_command.parse
      (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  with
  | `Exit code -> exit code
  | `Generate
      { dialect; schema_file; initial_file; baseline;
        migrations_file; manual_file; module_name; now; new_tables;
        destination } ->
    select_dialect
      (match dialect with
       | `MySQL -> Dialect.MySQL
       | `TiDB -> Dialect.TiDB);
    Sqlgg_config.gen_header := Some `Static;
    let generate () =
      run_migrate
        ~recorded_blocks:(fun blocks ->
          Typed_plan_blocks
            {
              baseline;
              blocks =
                List.map
                  (fun (id, block) ->
                    match id, baseline with
                    | None, (Some _ | None) ->
                      fatal "migration has no id: add -- [sqlgg] id=<timestamp>_<name>"
                    | Some id, Some cutoff
                      when Sqlgg_migrations.Migration_id.compare_order
                             id
                             cutoff
                           <= 0 ->
                      fatal "migration %s is not after --baseline %s"
                        (Sqlgg_migrations.Migration_id.to_string id)
                        (Sqlgg_migrations.Migration_id.to_string cutoff)
                    | Some id, (Some _ | None) -> id, block)
                  blocks;
            })
        {
          delta =
            {
              name = Config_file.Migrations.Module_name.to_string module_name;
              target_files = [ schema_file ];
              now;
              max_id_length = Sqlgg_migrations.Migration_id.max_length;
              new_tables;
              alter_options = [];
            };
          gen_lang = Caml_io;
          initial_files = Stdlib.Option.to_list initial_file;
          migrations_file = Some migrations_file;
          extends_file = manual_file;
        }
    in
    match destination with
    | Migrations_command.Stdout -> generate ()
    | Migrations_command.File filename ->
      let generated = Gen.capture generate in
      try Std.output_file ~filename ~text:generated
      with Sys_error message -> bad_input "cannot write %s: %s" filename message

let main () =
  match Array.to_list Sys.argv with
  | _ :: "ir" :: _ -> run_ir ()
  | _ :: "migrations" :: _ -> run_migrations ()
  | _ ->
    match parse_args () with
    | Generate c -> run_generate c
    | Diff c -> run_diff c
    | Migrate c ->
        run_migrate
          ~recorded_blocks:(fun blocks ->
            Function_list_blocks blocks)
          c
    | Materialize_schema c -> run_materialize_schema c

let () =
  exit @@
  match main () with
  | () -> 0
  | exception Cli_errors_found -> 1
  | exception Cli_fatal msg -> Error.logs msg; 1
  | exception Cli_bad_input msg -> Error.logs msg; 2
