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
    { sqlgg_version; module_name; dialect = Dialect.dialect_of_configuration !Sqlgg_config.dialect; params = !Gen.params_mode;
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
      (Sqlgg_migrations.Migration_id.t option * Main.migration_block) list
  | Typed_plan_blocks of {
      baseline : Sqlgg_migrations.Migration_id.t option;
      blocks : (Sqlgg_migrations.Migration_id.t * Main.migration_block) list;
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

let set_params_mode s =
  match Gen.params_mode_of_string s with
  | Ok selection ->
    params_selection := selection;
    Gen.params_mode := Gen.resolve_params_mode ~dialect:None selection
  | Error msg -> failwith msg

let all_categories = String.concat " " @@ List.map Stmt.show_category Stmt.all_categories
let category_of_string s =
  match List.find (fun cat -> String.equal (String.lowercase_ascii @@ Stmt.show_category cat) s) Stmt.all_categories with
  | exception _ -> failwith @@ sprintf "bad category %S" s
  | x -> x
let set_category s =
  let s = String.lowercase_ascii s in
  Sqlgg_config.include_category :=
    match s with
    | "all" -> `All
    | "none" -> `None
    | "" -> failwith "bad category \"\""
    | _ when s.[0] = '-' -> `Except (List.map category_of_string @@ String.nsplit (String.slice ~first:1 s) ",")
    | _ -> `Only (List.map category_of_string @@ String.nsplit s ",")
let filter_category cat =
  match !Sqlgg_config.include_category with
  | `All -> true
  | `None -> false
  | `Only l -> List.mem cat l
  | `Except l -> not (List.mem cat l)

let parse_output s =
  match (String.lowercase_ascii s) with
  | "cxx" | "c++" | "cpp" -> Some (Lang Cxx)
  | "caml" | "ocaml" | "ml" -> Some (Lang Caml)
  | "caml_io" -> Some (Lang Caml_io)
  | "xml" -> Some (Lang Xml)
  | "json" -> Some (Lang Json)
  | "java" -> Some (Lang Java)
  | "csharp" | "c#" | "cs" -> Some (Lang CSharp)
  | "sql" | "ddl" -> Some Sql_ddl
  | "none" -> None
  | _ -> failwith (sprintf "Unknown output language: %s" s)

(* parse always runs for its schema-registration side effects, even in -gen none *)
let read_statements = function
  | "-" -> Main.get_statements ~file:None stdin
  | filename ->
    Main.with_channel filename
      (Option.map_default (Main.get_statements ~file:(Some filename)) [])

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
      (fun (block : Main.migration_block) (numbered, rest) ->
        match Props.id block.stmt.props with
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
         Sqlgg_migrations.Migration_id.compare_position a b)
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
    let up = single "up" m.apply.sql in
    match m.revert with
    | None -> sprintf "-- [sqlgg] generated\n-- [sqlgg] irreversible\n-- [sqlgg] id=%s\n%s;" id up
    | Some revert -> sprintf "-- [sqlgg] generated\n-- [sqlgg] id=%s\n%s;\n%s;" id up (single "down" revert.sql))
  |> String.concat "\n\n"

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

let select_dialect configuration =
  Sqlgg_config.configure_dialect configuration;
  Gen.params_mode :=
    Gen.resolve_params_mode
      ~dialect:(Some (Dialect.dialect_of_configuration !Sqlgg_config.dialect))
      !params_selection

let set_dialect specification =
  match Dialect.parse_configuration specification with
  | Ok configuration -> select_dialect configuration
  | Error (Dialect.Unknown_dialect dialect_name) ->
    failwith (sprintf "Unknown dialect: %s" dialect_name)
  | Error (Dialect.Settings_not_supported row_store) ->
    fatal "dialect %s does not accept settings"
      (Dialect.to_string (Dialect.row_store_dialect row_store))
  | Error (Dialect.Invalid_clickhouse_setting
      (Sql_semantics.ClickHouse.Setting.Malformed_setting raw_setting)) ->
    fatal "malformed ClickHouse setting %S (expected name=value)" raw_setting
  | Error (Dialect.Invalid_clickhouse_setting
      (Sql_semantics.ClickHouse.Setting.Unknown_setting setting)) ->
    fatal "unknown ClickHouse setting %S" setting
  | Error (Dialect.Invalid_clickhouse_setting
      (Sql_semantics.ClickHouse.Setting.Invalid_setting_value { setting; value })) ->
    fatal "invalid value %S for ClickHouse setting %s (expected 0 or 1)"
      value (Sql_semantics.ClickHouse.Setting.name_to_string setting)

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

let parse_alter_lock =
  parse_alter_option "lock" alter_lock_values Sql.alter_lock_of_string

let parse_alter_algorithm =
  parse_alter_option "algorithm" alter_algorithm_values
    Sql.alter_algorithm_of_string

let set_no_check = function
  | "all" -> Sqlgg_config.set_no_check_features Dialect.all_of_feature
  | s ->
    String.nsplit s "," |> List.map (fun name ->
      match Dialect.feature_of_string name with
      | Some f -> f
      | None -> failwith (sprintf "Unknown feature: %s" name))
    |> Sqlgg_config.set_no_check_features

let abort_on_errors () = if !Error.errors then raise Cli_errors_found

let fail_on_errors msg =
  if !Error.errors then begin
    Error.logs msg;
    raise Cli_errors_found
  end

type schema_source = From_file of string | From_block of Statements.t

let to_file_sources files = List.map (fun f -> From_file f) files

let read_blocks f =
  Main.with_channel f
    (Stdlib.Option.fold ~none:[] ~some:(fun ch ->
       Main.migration_blocks ~file:f (In_channel.input_all ch)))

let replay_sources sources =
  Compile.reset ();
  let replay origin block =
    try Main.replay_statement block
    with Main.With_backtrace (exn, _) -> fatal "%s: %s" origin (Parser_utils.message_of_exn exn)
  in
  List.iter (function
    | From_file f ->
      List.iter (fun (block : Main.migration_block) -> replay f block.stmt) (read_blocks f)
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
  migration_log : Migrations_command.migration_log option;
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
  let diff_mode = ref false in
  let base_files = ref [] in
  let target_files = ref [] in
  let migrate_mode = ref false in
  let initial_files = ref [] in
  let migrations_file = ref None in
  let migrations_output = ref None in
  let extends_file = ref None in
  let now = ref None in
  let max_id_length = ref Sqlgg_migrations.Migration_id.max_length in
  let new_tables = ref Schema_diff.Created_by_schema in
  let alter_lock = ref None in
  let alter_algorithm = ref None in
  let emit_line_directives = ref false in
  let line_directives_file = ref None in
  let input_files = ref [] in
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
    input_files := s :: !input_files;
    inputs := filter_stmts ~output:!output stmts :: !inputs
  in
  let groups =
  [
    { title = "Code generation"; opts =
    [
      "-gen", Arg.String (fun s -> output := parse_output s), "cxx|caml|caml_io|java|xml|json|csharp|sql|none Set output language (default: none)";
      "-name", Arg.String (fun x -> name := x), "<identifier> Set output module name (default: sqlgg)";
      "-params", Arg.String set_params_mode, "named|unnamed|oracle|postgresql|clickhouse|none Output query parameters substitution (default: auto-detected from dialect, can be overridden)";
      "-category", Arg.String set_category, sprintf "{all|none|[-]<category>{,<category>}+} Only generate code for these specific query categories (possible values: %s)" all_categories;
      "-open", Arg.String open_input, "<file> Make definitions (schema, reusable queries) from <file> available without generating code for it (unless the file is also given as an input)";
      "-dynamic-select", Arg.Set Sqlgg_config.dynamic_select,
        " Generate static and dynamic version for every SELECT (dynamic allows to pick columns per call)";
      "-line-directives", Arg.Set emit_line_directives,
        " Emit `# <line> \"<file.sql>\"` directives so that OCaml and merlin report errors in a generated query function on the line the query starts at (caml/caml_io only)";
      "-line-directives-file", Arg.String (fun s -> line_directives_file := Some s),
        "<file> Name of the generated file, used by -line-directives to hand numbering back to it outside of query functions (default: <input.sql>.ml for a single input file; required with several input files and with -migrate)";
      "-", Arg.Unit (fun () -> work "-"), " Read sql from stdin";
      "-json-schema", Arg.Unit (print_json_schema Query.document_jsonschema),
        " Print the JSON Schema of -gen json output and exit";
    ] };

    { title = "Schema and migrations"; opts =
    [
      "-diff", Arg.Set diff_mode, " Generate up/down migrations from the diff of -base schema against -target schema";
      "-migrate", Arg.Set migrate_mode, " All-in-one: diff initial+migrations against target, append the up/down delta to the migrations file and (re)generate code";
      "-base", Arg.String (fun f -> base_files := f :: !base_files), "<file> Baseline schema / prior up migrations (repeatable)";
      "-target", Arg.String (fun f -> target_files := f :: !target_files), "<file> Target schema (repeatable)";
      "-initial", Arg.String (fun f -> initial_files := f :: !initial_files), "<file> Baseline DDL for -migrate (repeatable)";
      "-migrations-file", Arg.String (fun f -> migrations_file := Some f), "<file> Generated migrations SQL for -migrate (read and appended in place unless -migrations-output is given; required only when there is a new delta to record)";
      "-migrations-output", Arg.String (fun f -> migrations_output := Some f), "<file> Write the complete migration log here, leaving -migrations-file untouched";
      "-extends", Arg.String (fun f -> extends_file := Some f), "<file> Hand-written migrations SQL, merged with generated ones by `id` (read-only)";
      "-now", Arg.String (fun raw ->
        match Int64.of_string_opt raw with
        | Some n when Int64.compare n 0L >= 0 -> now := Some n
        | Some _ | None -> fatal "bad migration timestamp %S" raw),
        "<YYYYMMDDHHMMSS> Pin the migration id timestamp (default: current clock); ids are <timestamp>_<descriptive_name>";
      "-max-migration-id-length", Arg.Int (fun n -> max_id_length := n),
        sprintf "<N> Limit generated migration ids to N bytes (default and maximum: %d)"
          Sqlgg_migrations.Migration_id.max_length;
      "-ddl-as-migration", Arg.Unit (fun () -> new_tables := Schema_diff.Created_by_migrations), " Write new tables as CREATE TABLE migrations instead of plain schema DDL";
      "-alter-algorithm", Arg.String (fun s -> alter_algorithm := Some (parse_alter_algorithm s)),
        sprintf "%s Add ALGORITHM policy to generated ALTER TABLE statements (MySQL and TiDB only)"
          alter_algorithm_values;
      "-alter-lock", Arg.String (fun s -> alter_lock := Some (parse_alter_lock s)),
        sprintf "%s Add LOCK policy to generated ALTER TABLE statements (MySQL only)"
          alter_lock_values;
    ] };

    { title = "Dialect and checks"; opts =
    [
      "-dialect", Arg.String set_dialect,
        sprintf "%s Set SQL dialect. Queries can only use its features. ClickHouse session settings follow the name: clickhouse,join_use_nulls=1,..."
          (enum_values Dialect.to_string Dialect.all);
      "-no-check", Arg.String set_no_check,
        sprintf "{all|<feature>{,<feature>}+} Disable dialect feature checks (possible features: %s)"
          (enum_values Dialect.feature_to_string Dialect.all_of_feature);
      "-allow-write-notnull-null", Arg.Unit Sqlgg_config.allow_write_notnull_null,
        " Accept writing a nullable value into a NOT NULL column, instead of failing (MySQL, TiDB and SQLite only; for ClickHouse use -dialect clickhouse,insert_null_as_default=1)";
    ] };

    { title = "Generated header"; opts =
    [
      "-no-header", Arg.Unit (fun () -> Sqlgg_config.gen_header := None),
        " Do not put version header in generated output";
      "-no-header-timestamp", Arg.Unit (fun () -> Sqlgg_config.gen_header := Some `Without_timestamp),
        " Do not put timestamp in version header in generated output";
      "-static-header", Arg.Unit (fun () -> Sqlgg_config.gen_header := Some `Static), " Only output short static header without version/timestamp";
    ] };

    { title = "Diagnostics"; opts =
    [
      "-show-tables", Arg.Unit Tables.print_all, " Show all current tables";
      "-show-table", Arg.String Tables.print1, "<name> Show specified table";
      "-debug", Arg.Int Sqlgg_config.set_debug_level, "<N> Set debug level";
      "-version", Arg.Unit show_version, " Show version";
      "-test", Arg.Unit Test.run, " Run unit tests";
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
    "-options-md", Arg.Unit (fun () -> printf "```text\n%s\n```\n" (render groups); exit 0), "";
  ]
  in
  Arg.parse args work usage_msg;
  if Array.length Sys.argv = 1 then show_help ();
  Gen.line_directives :=
    (match !emit_line_directives, !line_directives_file with
     | false, None -> None
     | false, Some _ -> fatal "-line-directives-file requires -line-directives"
     | true, Some generated_file -> Some generated_file
     | true, None ->
       match !migrate_mode, List.filter (fun f -> f <> "-") !input_files with
       | true, _ -> fatal "-line-directives with -migrate requires -line-directives-file"
       | false, [] -> None
       | false, [ input ] -> Some (input ^ ".ml")
       | false, _ :: _ :: _ ->
         fatal "-line-directives with several input files requires -line-directives-file");
  if !max_id_length > Sqlgg_migrations.Migration_id.max_length then
    fatal "-max-migration-id-length %d is above %d, the width of the migration id column"
      !max_id_length Sqlgg_migrations.Migration_id.max_length;
  begin match !alter_lock, Dialect.dialect_of_configuration !Sqlgg_config.dialect with
  | Some _, (Dialect.PostgreSQL | Dialect.SQLite | Dialect.TiDB | Dialect.ClickHouse) ->
    fatal "-alter-lock is only supported for dialect mysql"
  | None, _ | Some _, Dialect.MySQL -> ()
  end;
  begin match !alter_algorithm, Dialect.dialect_of_configuration !Sqlgg_config.dialect with
  | Some _, (Dialect.PostgreSQL | Dialect.SQLite | Dialect.ClickHouse) ->
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
  (* these modes reset the schema and rebuild it from -base/-target/-initial,
     silently discarding whatever -open loaded *)
  let has_open =
    Hashtbl.fold (fun _ v acc -> acc || (match v with `Open _ -> true | `Positional -> false)) files false
  in
  if has_open && (!migrate_mode || !diff_mode) then
    fatal "-open is not supported with -diff/-migrate";
  match !migrate_mode, !diff_mode with
  | true, true -> fatal "-migrate and -diff are mutually exclusive"
  | true, false ->
    let migration_log =
      match !migrations_file, !migrations_output with
      | None, None -> None
      | None, Some _ -> fatal "-migrations-output requires -migrations-file"
      | Some source, output ->
        Some { Migrations_command.source; output }
    in
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
      migration_log;
      extends_file = !extends_file;
    }
  | false, true ->
    Diff { delta; base_files = List.rev !base_files; output = !output }
  | false, false ->
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
       gen_lang; initial_files; migration_log; extends_file } : migrate_args) =
  let initial = to_file_sources initial_files in
  let ext = Option.map_default read_blocks [] extends_file in
  let migration_source =
    let read_source source =
      if Sys.file_exists source
      then
        match Std.input_file source with
        | text -> text
        | exception Sys_error message ->
          fatal "cannot read %s: %s" source message
      else ""
    in
    match migration_log with
    | None -> None
    | Some ({ Migrations_command.source; _ } as log) ->
      Some (log, read_source source)
  in
  let read_recorded generated =
    recorded_blocks (merge_blocks generated ext)
  in
  let generated =
    match migration_source with
    | None -> []
    | Some ({ source; _ }, text) ->
      Main.migration_blocks ~file:source text
  in
  let before = read_recorded generated in
  let taken_ids, before_blocks =
    match before with
    | Function_list_blocks blocks ->
        List.filter_map fst blocks, List.map snd blocks
    | Typed_plan_blocks { baseline; blocks } ->
        Stdlib.Option.to_list baseline @ List.map fst blocks, List.map snd blocks
  in
  let current =
    schema_of_sources
      (initial @ List.map (fun (block : Main.migration_block) -> From_block block.stmt) before_blocks)
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
    begin match migration_source with
    | Some ({ output = Some filename; _ }, text) ->
      Std.output_file ~filename ~text
    | None | Some ({ output = None; _ }, _) -> ()
    end;
    (match before_blocks with
     | [] -> prerr_endline "nothing to do"
     | _ :: _ ->
       eprintf "nothing new to migrate; regenerated code from %d recorded migration(s)\n"
         (List.length before_blocks))
  | migs ->
    let { Migrations_command.source; output }, existing =
      match migration_source with
      | Some source -> source
      | None -> fatal "%d new migration(s) to record: pass -migrations-file <file>" (List.length migs)
    in
    let entries = assign_ids base migs in
    let sql = entries_to_sql entries in
    let text =
      match String.trim existing with
      | "" -> sql ^ "\n"
      | existing -> existing ^ "\n\n" ^ sql ^ "\n"
    in
    regenerate
      (read_recorded (Main.migration_blocks ~file:source text));
    let destination = Stdlib.Option.value output ~default:source in
    Std.output_file ~filename:destination ~text;
    let ids =
      String.concat ", "
        (List.map
           (fun (id, _) -> Sqlgg_migrations.Migration_id.to_string id)
           entries)
    in
    match output with
    | None ->
      eprintf "appended %d migration(s) to %s (id %s)\n"
        (List.length migs) destination ids
    | Some _ ->
      eprintf "wrote %d migration(s) to %s (id %s)\n"
        (List.length migs) destination ids

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
      "-dialect",     Arg.String set_dialect,                              "<dialect> Parse under this dialect (see the main -dialect option)";
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
  let semantics = Sqlgg_config.semantics () in
  let statements_of_sql =
    match !schema with
    | None -> Ir.parse ~allow_extensions:(not !plain_sql) ~semantics
    | Some path ->
      match
        Analysis.context_of_schema_sql ~dialect:(Analysis.Unchecked !Sqlgg_config.dialect)
          (read_path path)
      with
      | Ok context -> Ir.analyze ~allow_extensions:(not !plain_sql) ~semantics ~context
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
        migration_log; manual_file; module_name; now; new_tables;
        destination } ->
    select_dialect
      (Dialect.default_configuration
         (match dialect with
          | `MySQL -> Dialect.MySQL
          | `TiDB -> Dialect.TiDB));
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
                    | Some id, None -> id, block
                    | Some id, Some cutoff ->
                      let open Sqlgg_migrations.Migration_id in
                      let order = compare_position id cutoff in
                      if order > 0 then id, block
                      else if order < 0 || String.equal (to_string id) (to_string cutoff) then
                        fatal "migration %s is not after --baseline %s"
                          (to_string id) (to_string cutoff)
                      else
                        fatal
                          "migration %s shares its timestamp with --baseline %s, \
                           which archives every migration of that timestamp"
                          (to_string id) (to_string cutoff))
                  blocks;
            })
        {
          delta =
            {
              name = module_name;
              target_files = [ schema_file ];
              now;
              max_id_length = Sqlgg_migrations.Migration_id.max_length;
              new_tables;
              alter_options = [];
            };
          gen_lang = Caml_io;
          initial_files = Stdlib.Option.to_list initial_file;
          migration_log = Some migration_log;
          extends_file = manual_file;
        }
    in
    match destination with
    | Migrations_command.Stdout -> generate ()
    | Migrations_command.File { filename; line_directives } ->
      Gen.line_directives :=
        (match line_directives with
         | Migrations_command.Omit_line_directives -> None
         | Migrations_command.Emit_line_directives -> Some filename);
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
