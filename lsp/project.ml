open Sqlgg

type t = {
  schema_files : string list;
  dialect : Dialect.t;
  watch_paths : string list;
}

let default = { schema_files = []; dialect = Dialect.MySQL; watch_paths = [] }

type config = {
  schema : string list [@default []];
  dialect : Dialect.t [@default default.dialect];
} [@@deriving of_yojson { strict = false }]

let locate path =
  let rec find_config_opt dir =
    let config = Filename.concat dir "sqlgg.json" in
    if Sys.file_exists config then Some config
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else find_config_opt parent
  in
  find_config_opt (Filename.dirname path)

let load config_path =
  let config =
    match config_of_yojson (Yojson.Safe.from_file config_path) with
    | Ok config -> config
    | Error _ | exception (Yojson.Json_error _ | Sys_error _) ->
      { schema = []; dialect = default.dialect }
  in
  let root = Filename.dirname config_path in
  let globs = File_glob.create () in
  let expand schema =
    let path =
      if Filename.is_relative schema then Filename.concat root schema
      else schema
    in
    (match File_glob.expand globs path with Ok files -> files | Error _ -> []),
    Filename.dirname path
  in
  let file_groups, watched_directories =
    List.split (List.map expand config.schema)
  in
  let schema_files = List.concat file_groups in
  let watch_paths = config_path :: watched_directories in
  { schema_files; dialect = config.dialect; watch_paths }
