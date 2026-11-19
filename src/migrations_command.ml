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
  module_name : string;
  now : int64 option;
  new_tables : Schema_diff.new_tables;
  destination : destination;
}

let parse argv =
  let open Cmdliner in
  let runtime_language = "ocaml" in
  let required_string name ~docv ~doc =
    Arg.(required & opt (some string) None & info [ name ] ~docv ~doc)
  in
  let optional_file name ~doc =
    Arg.(value & opt (some string) None & info [ name ] ~docv:"FILE" ~doc)
  in
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
                    runtime_language, `Runtime;
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
      required_string "schema" ~docv:"FILE"
        ~doc:"The schema to migrate to: plain $(b,CREATE TABLE) DDL."
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
      required_string "migrations" ~docv:"FILE"
        ~doc:
          "The migration log. Read to learn the current schema; new \
           migrations are appended to it in place. May not exist yet."
    in
    let manual_file =
      optional_file "manual"
        ~doc:
          "Hand-written migrations, merged with the generated ones by \
           id. Never written to."
    in
    let module_name =
      let parse raw =
        let name = String.capitalize_ascii raw in
        let valid_tail =
          String.for_all
            (function
              | 'a' .. 'z'
              | 'A' .. 'Z'
              | '0' .. '9'
              | '_'
              | '\'' ->
                  true
              | _ -> false)
        in
        if
          name <> ""
          &&
          match name.[0] with
          | 'A' .. 'Z' -> valid_tail name
          | _ -> false
        then
          Ok name
        else
          Error ("invalid OCaml module name " ^ Printf.sprintf "%S" raw)
      in
      Arg.(
        required
        & opt
            (some
               (conv' ~docv:"NAME"
                  (parse, Format.pp_print_string)))
            None
        & info [ "module" ] ~docv:"NAME"
            ~doc:"Name of the generated module.")
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
        & vflag Schema_diff.Created_by_schema
            [
              ( Schema_diff.Created_by_migrations,
                info [ "ddl-as-migration" ]
                  ~doc:
                    "Record a table that is new in $(b,--schema) as a \
                     $(b,CREATE TABLE) migration. Without it a new table \
                     must also be added to $(b,--initial), and the schema \
                     DDL creates it." );
            ])
    in
    let destination =
      Arg.(
        value
        & opt (some string) None
        & info [ "output" ] ~docv:"FILE"
            ~doc:
              "Write the generated migrations module to $(docv). The file \
               is left untouched when generation fails. The default is \
               standard output.")
    in
    Term.(
      term_result' ~usage:false
        (const
           (fun
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
           ->
             let supported dialect =
               Ok
                 {
                   dialect;
                   schema_file;
                   initial_file;
                   baseline;
                   migrations_file;
                   manual_file;
                   module_name;
                   now;
                   new_tables;
                   destination =
                     (match output_file with
                      | None -> Stdout
                      | Some filename -> File filename);
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
                    ^ " yet (supported: " ^ runtime_language ^ ")")
             | Dialect.MySQL, `Runtime -> supported `MySQL
             | Dialect.TiDB, `Runtime -> supported `TiDB)
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
