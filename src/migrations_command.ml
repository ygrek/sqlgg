open Sqlgg

type host_language = Ocaml

type generation = {
  dialect : [ `MySQL | `TiDB ];
  host_language : host_language;
  schema_file : string;
  migrations_file : string;
  manual_file : string option;
  module_name : string;
  now : int option;
}

type request =
  | Generate of generation
  | Print_dune_rules of {
      generation : generation;
      generated_file : string;
    }

let parse argv =
  let open Cmdliner in
  let generation =
    let dialect =
      Arg.(
        required
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
               $(b,mysql) (also for MariaDB) and $(b,tidb).")
    in
    let host_language =
      Arg.(
        required
        & opt
            (some
               (enum
                  [
                    "ocaml", `Runtime Ocaml;
                    "cxx", `No_runtime "cxx";
                    "csharp", `No_runtime "csharp";
                    "java", `No_runtime "java";
                  ]))
            None
        & info [ "lang" ] ~docv:"LANG"
            ~doc:
              "Host language of the generated migrations. Only $(b,ocaml) \
               has a migration runtime so far.")
    in
    let schema_file =
      Arg.(
        required
        & opt (some string) None
        & info [ "schema" ] ~docv:"FILE"
            ~doc:"The schema to migrate to: plain $(b,CREATE TABLE) DDL.")
    in
    let migrations_file =
      Arg.(
        required
        & opt (some string) None
        & info [ "migrations" ] ~docv:"FILE"
            ~doc:
              "The migration log. Read to learn the current schema; new \
               migrations are appended to it in place. May not exist yet.")
    in
    let manual_file =
      Arg.(
        value
        & opt (some string) None
        & info [ "manual" ] ~docv:"FILE"
            ~doc:
              "Hand-written migrations, merged with the generated ones by \
               id. Never written to.")
    in
    let module_name =
      Arg.(
        required
        & opt (some string) None
        & info [ "module" ] ~docv:"NAME"
            ~doc:"Name of the generated module.")
    in
    let now =
      Arg.(
        value
        & opt (some int) None
        & info [ "now" ] ~docv:"YYYYMMDDHHMMSS"
            ~doc:
              "Timestamp for the ids of new migrations (default: the \
               current time).")
    in
    Term.(
      term_result' ~usage:false
        (const
           (fun
             dialect
             host_language
             schema_file
             migrations_file
             manual_file
             module_name
             now
           ->
             let supported dialect host_language =
               Ok
                 {
                   dialect;
                   host_language;
                   schema_file;
                   migrations_file;
                   manual_file;
                   module_name;
                   now;
                 }
             in
             match dialect, host_language with
             | (Dialect.PostgreSQL | Dialect.SQLite), _ ->
                 Error
                   ("the migration runtime does not support --dialect "
                    ^ Dialect.to_string dialect
                    ^ " yet (supported: mysql, tidb)")
             | (Dialect.MySQL | Dialect.TiDB), `No_runtime name ->
                 Error
                   ("there is no migration runtime for --lang " ^ name
                    ^ " yet (supported: ocaml)")
             | Dialect.MySQL, `Runtime host_language ->
                 supported `MySQL host_language
             | Dialect.TiDB, `Runtime host_language ->
                 supported `TiDB host_language)
         $ dialect
         $ host_language
         $ schema_file
         $ migrations_file
         $ manual_file
         $ module_name
         $ now))
  in
  let generate =
    Cmd.v
      (Cmd.info "generate"
         ~doc:
           "Record the changes of the schema as new migrations and print \
            the migrations module.")
      Term.(const (fun generation -> Generate generation) $ generation)
  in
  let dune =
    let generated_file =
      Arg.(
        required
        & opt (some string) None
        & info [ "generated" ] ~docv:"FILE"
            ~doc:"The file the Dune rules write the migrations module to.")
    in
    Cmd.v
      (Cmd.info "dune"
         ~doc:
           "Print Dune rules that run $(b,generate) on every build and \
            promote the migration log.")
      Term.(
        const (fun generation generated_file ->
          Print_dune_rules { generation; generated_file })
        $ generation
        $ generated_file)
  in
  match
    Cmd.eval_value ~argv
      (Cmd.group
         (Cmd.info "sqlgg migrations" ~version:Sqlgg_config.version
            ~doc:"Schema migrations with a runtime provided by sqlgg.")
         [ generate; dune ])
  with
  | Ok (`Ok request) -> `Request request
  | Ok (`Help | `Version) -> `Exit 0
  | Error (`Parse | `Term | `Exn) -> `Exit 2

let generate_arguments
    {
      dialect;
      host_language;
      schema_file;
      migrations_file;
      manual_file;
      module_name;
      now;
    } =
  [
    "migrations";
    "generate";
    "--dialect";
    (match dialect with
     | `MySQL -> Dialect.to_string Dialect.MySQL
     | `TiDB -> Dialect.to_string Dialect.TiDB);
    "--lang";
    (match host_language with
     | Ocaml -> "ocaml");
    "--schema";
    schema_file;
    "--migrations";
    migrations_file;
    "--module";
    module_name;
  ]
  @ (match manual_file with
     | Some file -> [ "--manual"; file ]
     | None -> [])
  @ (match now with
     | Some stamp -> [ "--now"; string_of_int stamp ]
     | None -> [])
