(** command-line *)

open Printf
open ExtLib
open Sqlgg

module Name = Migration_id.Name

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

type lang = Cxx | Caml | Caml_io | Xml | Java | CSharp

type output = Lang of lang | Sql_ddl

exception Cli_fatal of string
exception Cli_errors_found

let fatal fmt = ksprintf (fun s -> raise (Cli_fatal s)) fmt

let process_stmts = function
  | Cxx -> Cxx.process
  | Caml -> Caml.process
  | Caml_io -> Caml_io.process
  | Xml -> Xml_gen.process
  | Java -> Java.process
  | CSharp -> CSharp.process

let process_migrations =
  let unsupported lang = failwith (sprintf "migrations not supported for %s" lang) in
  function
  | Caml | Caml_io -> Gen_caml.generate_migrations
  | Xml -> Gen_xml.generate_migrations
  | Cxx -> unsupported "C++"
  | Java -> unsupported "Java"
  | CSharp -> unsupported "C#"

let set_params_mode s =
  Gen.params_mode :=
  match String.lowercase_ascii s with
  | "named" -> Some Gen.Named
  | "unnamed" -> Some Gen.Unnamed
  | "oracle" -> Some Gen.Oracle
  | "postgresql" -> Some Gen.PostgreSQL
  | "none" -> None
  | _ -> failwith (sprintf "Unknown params mode: %s" s)

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
  | "java" -> Some (Lang Java)
  | "csharp" | "c#" | "cs" -> Some (Lang CSharp)
  | "sql" | "ddl" -> Some Sql_ddl
  | "none" -> None
  | _ -> failwith (sprintf "Unknown output language: %s" s)

let each_input ~output =
  let run input =
    (* parse always runs for its schema-registration side effects, even in -gen none *)
    let stmts = match input with Some ch -> Main.get_statements ch | None -> [] in
    match !output with
    | None -> []
    | Some _ ->
      stmts |> List.filter (fun stmt -> filter_category (Stmt.category_of_stmt_kind stmt.Gen.kind))
  in
  function
  | "-" -> run (Some stdin)
  | filename -> Main.with_channel filename run

let generate ~output ~name results =
  match output with
  | None | Some Sql_ddl -> ()
  | Some (Lang l) ->
    process_stmts l name (List.concat results)

let migration_to_sql ~id ~name (m : Gen_migrations.migration) =
  let single what = function
    | [s] -> String.trim s
    | _ ->
      fatal
        "-gen sql: %s of migration %s is multi-statement and cannot be emitted as \
         a single SQL entry; write this step by hand in the extends file" what name
  in
  let up = single "up" m.apply in
  match m.revert with
  | [] -> sprintf "-- [sqlgg] generated\n-- [sqlgg] irreversible\n-- [sqlgg] id=%s\n%s;" id up
  | revert -> sprintf "-- [sqlgg] generated\n-- [sqlgg] id=%s\n%s;\n%s;" id up (single "down" revert)

let block_id ((_, props) : string * Props.t) =
  Props.get props "id" |> Option.map (fun s ->
    match Migration_id.parse s with
    | Some id -> id
    | None -> fatal "bad migration block id %S (expected integer or <timestamp>_<name>)" s)

let merge_blocks gen ext =
  let numbered, rest =
    List.fold_right
      (fun b (num, rest) ->
        Option.map_default (fun id -> ((id, b) :: num, rest)) (num, b :: rest) (block_id b))
      (gen @ ext) ([], [])
  in
  List.map snd (List.stable_sort (fun (a, _) (b, _) -> Migration_id.compare a b) numbered) @ rest

let now_stamp () =
  let t = Unix.localtime (Unix.time ()) in
  List.fold_left (fun acc x -> acc * 100 + x)
    (t.Unix.tm_year + 1900)
    [t.Unix.tm_mon + 1; t.Unix.tm_mday; t.Unix.tm_hour; t.Unix.tm_min; t.Unix.tm_sec]

let next_base now existing =
  let ts = match now with Some n -> n | None -> now_stamp () in
  List.filter_map block_id existing
  |> List.map Migration_id.next_ord
  |> List.fold_left Int.max 0
  |> Int.max ts

type entry = { id : string; code_name : string; mig : Gen_migrations.migration }

let assign_ids ~naming base migs =
  let entries =
    List.mapi (fun i (mig : Gen_migrations.migration) ->
      let descr = Name.fit naming (Gen.choose_name mig.props mig.kind i) in
      let id = Migration_id.to_string (Migration_id.make ~name:descr base) in
      { id; code_name = id; mig }) migs
  in
  let seen = Hashtbl.create 16 in
  entries |> List.iter (fun { id; _ } ->
    if Hashtbl.mem seen id then
      fatal "two migrations get the same id %S. increase -max-migration-id-length" id;
    Hashtbl.add seen id ());
  entries

let entry_to_sql { id; code_name; mig } = migration_to_sql ~id ~name:code_name mig

let entry_to_codegen { code_name; mig; _ } =
  { mig with props = Props.set mig.props "name" code_name }

let entries_to_sql entries = entries |> List.map entry_to_sql |> String.concat "\n\n"

let read_file_opt path =
  if Sys.file_exists path then
    match Std.input_file path with
    | exception Sys_error msg -> fatal "cannot read %s: %s" path msg
    | s -> let s = String.trim s in if s = "" then None else Some s
  else
    None

let usage_msg =
  let s1 = sprintf "SQL Guided (code) Generator ver. %s\n" Sqlgg_config.version in
  let s2 = sprintf "Usage: %s <options> <file.sql> [<file2.sql> ...]\n" (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

type group = { title : string; opts : (Arg.key * Arg.spec * Arg.doc) list }

let render groups =
  let option (key, _spec, doc) = if doc = "" then None else Some (sprintf "  %s %s" key doc) in
  let group g = sprintf " %s:\n%s" g.title (String.concat "\n" (List.filter_map option g.opts)) in
  String.concat "\n\n" (List.map group groups)

let show_version () = print_endline Sqlgg_config.version

let set_dialect s =
  let d =
    match Dialect.of_string s with
    | Some d -> d
    | None -> failwith (sprintf "Unknown dialect: %s" s)
  in
  Dialect.set_selected d;
  match !Gen.params_mode with
  | Some _ -> () (* if manually set via -params, keep user's choice *)
  | None ->
    Gen.params_mode :=
      match d with
      | Dialect.MySQL | Dialect.TiDB | Dialect.SQLite -> Some Gen.Unnamed  (* ? syntax *)
      | Dialect.PostgreSQL -> Some Gen.PostgreSQL  (* $1, $2, etc. *)

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

type schema_source = From_file of string | From_sql of string

let to_file_sources files = List.map (fun f -> From_file f) files

let replay_sources sources =
  Tables.reset ();
  User_types.reset ();
  List.iter (function
    | From_file f -> Main.with_channel f (Option.map_default Main.replay_schema ())
    | From_sql sql -> Main.replay_sql sql) sources

let schema_of_sources sources =
  replay_sources sources;
  abort_on_errors ();
  Tables.snapshot ()

let load_schema files = schema_of_sources (to_file_sources files)

let diff_schema ~naming ~ddl_as_migration ~from_ ~to_ =
  let migs =
    try Schema_diff.generate ~naming ~ddl_as_migration ~from_ ~to_
    with Gen_migrations.Migration_error msg ->
      fatal "cannot generate migration (write this step manually):\n%s" msg
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
  inputs : Gen.stmt list list;
}

type delta_args = {
  name : string;
  target_files : string list;
  now : int option;
  max_id_length : int option;
  ddl_as_migration : bool;
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
  let diff_mode = ref false in
  let base_files = ref [] in
  let target_files = ref [] in
  let migrate_mode = ref false in
  let initial_files = ref [] in
  let migrations_file = ref None in
  let extends_file = ref None in
  let now = ref None in
  let max_id_length = ref None in
  let ddl_as_migration = ref false in
  let work s = inputs := each_input ~output s :: !inputs in
  let groups =
  [
    { title = "Code generation"; opts =
    [
      "-gen", Arg.String (fun s -> output := parse_output s), "cxx|caml|caml_io|java|xml|csharp|sql|none Set output language (default: none)";
      "-name", Arg.String (fun x -> name := x), "<identifier> Set output module name (default: sqlgg)";
      "-params", Arg.String set_params_mode, "named|unnamed|oracle|postgresql|none Output query parameters substitution (default: auto-detected from dialect, can be overridden)";
      "-category", Arg.String set_category, sprintf "{all|none|[-]<category>{,<category>}+} Only generate code for these specific query categories (possible values: %s)" all_categories;
      "-dynamic-select", Arg.Set Sqlgg_config.dynamic_select,
        " Generate static and dynamic version for every SELECT (dynamic allows to pick columns per call)";
      "-", Arg.Unit (fun () -> work "-"), " Read sql from stdin";
    ] };

    { title = "Schema and migrations"; opts =
    [
      "-diff", Arg.Set diff_mode, " Generate up/down migrations from the diff of -base schema against -target schema";
      "-migrate", Arg.Set migrate_mode, " All-in-one: diff initial+migrations against target, append the up/down delta to the migrations file and (re)generate code";
      "-base", Arg.String (fun f -> base_files := f :: !base_files), "<file> Baseline schema / prior up migrations (repeatable)";
      "-target", Arg.String (fun f -> target_files := f :: !target_files), "<file> Target schema (repeatable)";
      "-initial", Arg.String (fun f -> initial_files := f :: !initial_files), "<file> Baseline DDL for -migrate (repeatable)";
      "-migrations-file", Arg.String (fun f -> migrations_file := Some f), "<file> Generated migrations SQL for -migrate (read and appended in place; required only when there is a new delta to record)";
      "-extends", Arg.String (fun f -> extends_file := Some f), "<file> Hand-written migrations SQL, merged with generated ones by `id` (read-only)";
      "-now", Arg.Int (fun n -> now := Some n), "<YYYYMMDDHHMMSS> Pin the migration id timestamp (default: current clock); ids are <timestamp>_<descriptive_name>";
      "-max-migration-id-length", Arg.Int (fun n -> max_id_length := Some n),
        "<N> Limit generated migration ids to N characters (default: no limit)";
      "-ddl-as-migration", Arg.Set ddl_as_migration, " Write new tables as CREATE TABLE migrations instead of plain schema DDL";
    ] };

    { title = "Dialect and checks"; opts =
    [
      "-dialect", Arg.String set_dialect, sprintf "%s Set SQL dialect. Queries can only use its features" (Dialect.all |> List.map Dialect.to_string |> String.concat "|");
      "-no-check", Arg.String set_no_check,
        sprintf "{all|<feature>{,<feature>}+} Disable dialect feature checks (possible features: %s)"
          (Dialect.all_of_feature |> List.map Dialect.feature_to_string |> String.concat "|");
      "-allow-write-notnull-null", Arg.Unit (fun () -> Sqlgg_config.allow_write_notnull_null true), " Accept writing a nullable value into a NOT NULL column, instead of failing (MySQL, TiDB and SQLite only)";
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
  let delta =
    { name = !name;
      target_files = List.rev !target_files;
      now = !now;
      max_id_length = !max_id_length;
      ddl_as_migration = !ddl_as_migration }
  in
  match !migrate_mode, !diff_mode with
  | true, true -> fatal "-migrate and -diff are mutually exclusive"
  | true, false ->
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

let read_blocks f = Main.with_channel f (function Some ch -> Main.raw_blocks ch | None -> [])

let parse_migrations blocks =
  let migs = List.filter_map Main.migration_of_block blocks in
  abort_on_errors ();
  migs

let run_migrate ({ delta = { name; target_files; now; max_id_length; ddl_as_migration };
                   gen_lang; initial_files; migrations_file; extends_file } : migrate_args) =
  let initial = to_file_sources initial_files in
  let ext = Option.map_default read_blocks [] extends_file in
  let recorded_blocks () = merge_blocks (Option.map_default read_blocks [] migrations_file) ext in
  let before = recorded_blocks () in
  let current =
    schema_of_sources (initial @ List.map (fun (sql, _) -> From_sql sql) before)
  in
  let target = load_schema target_files in
  let regenerate () =
    replay_sources initial;
    let full = parse_migrations (recorded_blocks ()) in
    process_migrations gen_lang name full
  in
  let base = next_base now before in
  let naming = Migration_id.naming ~max_length:max_id_length base in
  match diff_schema ~naming ~ddl_as_migration ~from_:current ~to_:target with
  | [] ->
    regenerate ();
    (match before with
     | [] -> prerr_endline "nothing to do"
     | _ ->
       eprintf "nothing new to migrate; regenerated code from %d recorded migration(s)\n"
         (List.length before))
  | migs ->
    let migrations_file =
      match migrations_file with
      | Some f -> f
      | None -> fatal "%d new migration(s) to record: pass -migrations-file <file>" (List.length migs)
    in
    let entries = assign_ids ~naming base migs in
    let sql = entries_to_sql entries in
    let text =
      Option.map_default (fun existing -> existing ^ "\n\n" ^ sql) sql
        (read_file_opt migrations_file)
    in
    Std.output_file ~filename:migrations_file ~text:(text ^ "\n");
    regenerate ();
    let ids = List.map (fun e -> e.id) entries in
    eprintf "appended %d migration(s) to %s (id %s)\n"
      (List.length migs) migrations_file (String.concat ", " ids)

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

let run_diff ({ delta = { name; target_files; now; max_id_length; ddl_as_migration };
                base_files; output } : diff_args) =
  let from_ = load_schema base_files in
  let to_   = load_schema target_files in
  let base = next_base now [] in
  let naming = Migration_id.naming ~max_length:max_id_length base in
  let migs = diff_schema ~naming ~ddl_as_migration ~from_ ~to_ in
  Tables.restore from_;
  match output with
  | None -> ()
  | Some Sql_ddl ->
    if migs <> [] then print_endline (entries_to_sql (assign_ids ~naming base migs))
  | Some (Lang l) ->
    process_migrations l name (assign_ids ~naming base migs |> List.map entry_to_codegen)

let run_generate ({ output; name; inputs } : gen_args) =
  match inputs with
  | [] -> ()
  | results ->
    fail_on_errors "Errors encountered, no code generated";
    generate ~output ~name results

let main () =
  match parse_args () with
  | Generate c -> run_generate c
  | Diff c -> run_diff c
  | Migrate c -> run_migrate c
  | Materialize_schema c -> run_materialize_schema c

let () =
  exit @@
  match main () with
  | () -> 0
  | exception Cli_errors_found -> 1
  | exception Cli_fatal msg -> Error.logs msg; 1
