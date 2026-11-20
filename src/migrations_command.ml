open Sqlgg

type destination =
  | Stdout
  | File of string

type generation = {
  dialect : [ `MySQL | `TiDB ];
  schema_file : string;
  initial_file : string option;
  baseline : Sqlgg_migrations.Migration_id.t option;
  migrations_file : string;
  manual_file : string option;
  module_name : Config_file.Migrations.Module_name.t;
  now : int64 option;
  new_tables : Schema_diff.new_tables;
  destination : destination;
}

let parse argv =
  let open Cmdliner in
  let optional_file name ~doc =
    Arg.(value & opt (some string) None & info [ name ] ~docv:"FILE" ~doc)
  in
  let generation =
    let config_file =
      optional_file "config"
        ~doc:
          "A sqlgg configuration file. Its $(b,dialect) and the keys of its \
           $(b,migrations) object stand in for the options that are not on \
           the command line."
    in
    let dialect =
      Arg.(
        value
        & opt
            (some
               (enum
                  (List.map
                     (fun dialect -> Dialect.to_string dialect, dialect)
                     Dialect.all)))
            None
        & info [ "dialect" ] ~docv:"DIALECT"
            ~doc:
              "SQL dialect of the schema. The migration runtime supports \
               $(b,mysql) (also for MariaDB) and $(b,tidb). Required.")
    in
    let host_language =
      Arg.(
        value
        & opt
            (some
               (enum
                  (List.map
                     (fun language ->
                       ( Config_file.Migrations.host_language_to_string
                           language,
                         language ))
                     Config_file.Migrations.all_of_host_language)))
            None
        & info [ "lang" ] ~docv:"LANG"
            ~doc:
              "Host language of the generated migrations. Only $(b,ocaml) \
               has a migration runtime so far. Required.")
    in
    let schema_file =
      optional_file "schema"
        ~doc:
          "The schema to migrate to: plain $(b,CREATE TABLE) DDL. Required."
    in
    let initial_file =
      optional_file "initial"
        ~doc:
          "The schema the database already has. Its tables are the \
           starting point and are not turned into migrations."
    in
    let baseline =
      Arg.(
        value
        & opt
            (some
               (conv' ~docv:"ID"
                  ( (fun raw ->
                      Result.map_error
                        Sqlgg_migrations.Migration_id.error_to_string
                        (Sqlgg_migrations.Migration_id.parse raw)),
                    fun fmt id ->
                      Format.pp_print_string fmt
                        (Sqlgg_migrations.Migration_id.to_string id) )))
            None
        & info [ "baseline" ] ~docv:"ID"
            ~doc:
              "Last migration id folded into $(b,--initial). Stored \
               applied rows at or before it are treated as archived \
               history.")
    in
    let migrations_file =
      optional_file "migrations"
        ~doc:
          "The migration log. Read to learn the current schema; new \
           migrations are appended to it in place. May not exist yet. \
           Required."
    in
    let manual_file =
      optional_file "manual"
        ~doc:
          "Hand-written migrations, merged with the generated ones by \
           id. Never written to."
    in
    let module_name =
      Arg.(
        value
        & opt
            (some
               (conv' ~docv:"NAME"
                  ( (fun raw ->
                      Result.map_error
                        Config_file.Migrations.Module_name.error_to_string
                        (Config_file.Migrations.Module_name.of_string raw)),
                    fun fmt name ->
                      Format.pp_print_string fmt
                        (Config_file.Migrations.Module_name.to_string name) )))
            None
        & info [ "module" ] ~docv:"NAME"
            ~doc:"Name of the generated module. Required.")
    in
    let now =
      let parse raw =
        match Int64.of_string_opt raw with
        | Some ordinal when Int64.compare ordinal 0L >= 0 -> Ok ordinal
        | Some _ | None ->
            Error ("invalid migration timestamp " ^ Printf.sprintf "%S" raw)
      in
      Arg.(
        value
        & opt
            (some
               (conv' ~docv:"YYYYMMDDHHMMSS"
                  ( parse,
                    fun fmt ordinal ->
                      Format.pp_print_string fmt (Int64.to_string ordinal) )))
            None
        & info [ "now" ] ~docv:"YYYYMMDDHHMMSS"
            ~doc:
              "Timestamp for the ids of new migrations (default: the \
               current time).")
    in
    let new_tables =
      Arg.(
        value
        & vflag None
            [
              ( Some Schema_diff.Created_by_migrations,
                info [ "ddl-as-migration" ]
                  ~doc:
                    "Record a table that is new in $(b,--schema) as a \
                     $(b,CREATE TABLE) migration. Without it a new table \
                     must also be added to $(b,--initial), and the schema \
                     DDL creates it." );
            ])
    in
    let destination =
      optional_file "output"
        ~doc:
          "Write the generated migrations module to $(docv). The file \
           is left untouched when generation fails. The default is \
           standard output."
    in
    let resolve
        config_file
        dialect
        host_language
        schema_file
        initial_file
        baseline
        migrations_file
        manual_file
        module_name
        now
        new_tables
        output_file
      =
      let ( let* ) = Result.bind in
      let generation =
        let* configured_dialect, section =
          match config_file with
          | None -> Ok (None, None)
          | Some file ->
            Result.map_error
              (fun (`Config_file error) ->
                `Rejected (Config_file.error_to_string error))
              (Config_file.migrations file)
        in
        let setting given (read : Config_file.Migrations.t -> _ option) =
          match given with
          | Some _ -> given
          | None -> Stdlib.Option.bind section read
        in
        let required option setting =
          Stdlib.Option.to_result ~none:(`Missing option) setting
        in
        let* dialect =
          required "dialect"
            (match dialect with Some _ -> dialect | None -> configured_dialect)
        in
        let* host_language =
          required "lang"
            (setting host_language Config_file.Migrations.lang)
        in
        let* schema_file =
          required "schema"
            (setting schema_file Config_file.Migrations.schema)
        in
        let* migrations_file =
          required "migrations"
            (setting migrations_file Config_file.Migrations.migrations)
        in
        let* module_name =
          required "module"
            (setting module_name Config_file.Migrations.module_name)
        in
        let supported dialect =
          Ok
            {
              dialect;
              schema_file;
              initial_file =
                setting initial_file Config_file.Migrations.initial;
              baseline = setting baseline Config_file.Migrations.baseline;
              migrations_file;
              manual_file =
                setting manual_file Config_file.Migrations.manual;
              module_name;
              now;
              new_tables =
                Stdlib.Option.value ~default:Schema_diff.Created_by_schema
                  (setting new_tables Config_file.Migrations.new_tables);
              destination =
                (match
                   setting output_file Config_file.Migrations.output
                 with
                 | None -> Stdout
                 | Some filename -> File filename);
            }
        in
        match dialect, host_language with
        | (Dialect.PostgreSQL | Dialect.SQLite), _ ->
            Error
              (`Rejected
                 ("the migration runtime does not support --dialect "
                  ^ Dialect.to_string dialect
                  ^ " yet (supported: mysql, tidb)"))
        | ( (Dialect.MySQL | Dialect.TiDB),
            ((Config_file.Migrations.Cxx | Csharp | Java) as language) ) ->
            Error
              (`Rejected
                 ("there is no migration runtime for --lang "
                  ^ Config_file.Migrations.host_language_to_string language
                  ^ " yet (supported: "
                  ^ Config_file.Migrations.host_language_to_string Ocaml
                  ^ ")"))
        | Dialect.MySQL, Ocaml -> supported `MySQL
        | Dialect.TiDB, Ocaml -> supported `TiDB
      in
      match generation with
      | Ok generation -> `Ok generation
      | Error (`Missing option) ->
          `Error (true, "required option --" ^ option ^ " is missing")
      | Error (`Rejected message) -> `Error (false, message)
    in
    Term.(
      ret
        (const resolve
         $ config_file
         $ dialect
         $ host_language
         $ schema_file
         $ initial_file
         $ baseline
         $ migrations_file
         $ manual_file
         $ module_name
         $ now
         $ new_tables
         $ destination))
  in
  let generate =
    Cmd.v
      (Cmd.info "generate"
         ~doc:
           "Record the changes of the schema as new migrations and write \
            the migrations module.")
      generation
  in
  match
    Cmd.eval_value ~argv
      (Cmd.group
         (Cmd.info "sqlgg migrations" ~version:Sqlgg_config.version
            ~doc:"Schema migrations with a runtime provided by sqlgg.")
         [ generate ])
  with
  | Ok (`Ok generation) -> `Generate generation
  | Ok (`Help | `Version) -> `Exit 0
  | Error (`Parse | `Term | `Exn) -> `Exit 2
