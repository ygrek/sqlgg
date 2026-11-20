open Sqlgg
open Jsonkit.Primitives

type gen =
  | Cxx [@name "cxx"]
  | Caml [@name "caml"]
  | Caml_io [@name "caml_io"]
  | Java [@name "java"]
  | Xml [@name "xml"]
  | Json [@name "json"]
  | Csharp [@name "csharp"]
  | Sql [@name "sql"]
  | No_output [@name "none"]
[@@deriving of_string, of_json, to_json, jsonschema] [@@compact_variants]

type notnull_null_write = Allow [@name "allow"] | Reject [@name "reject"]
[@@deriving of_json, to_json, jsonschema] [@@compact_variants]

type dynamic_select = Both [@name "both"] | Off [@name "off"]
[@@deriving of_json, to_json, jsonschema] [@@compact_variants]

type setting =
  | Dialect of Dialect.t
  | No_check of Dialect.feature list
  | Write_notnull_null of notnull_null_write
  | Header of [ `Full | `Without_timestamp | `Static ] option
  | Gen of gen
  | Name of string
  | Params of Gen.params_selection
  | Category of
      [ `All | `None | `Only of Stmt.category list | `Except of Stmt.category list ]
  | Dynamic_select of dynamic_select
  | Diff_mode
  | Migrate_mode
  | Base of string
  | Target of string
  | Initial of string
  | Migrations_file of string
  | Extends of string
  | Max_migration_id_length of int
  | New_tables of Schema_diff.new_tables
  | Alter_algorithm of Sql.alter_algorithm
  | Alter_lock of Sql.alter_lock
  | Open of string
  | Input of string
  | Stdin

module Migrations = struct
  type host_language =
    | Ocaml [@name "ocaml"]
    | Cxx [@name "cxx"]
    | Csharp [@name "csharp"]
    | Java [@name "java"]
  [@@deriving enumerate, to_string, of_json, jsonschema] [@@compact_variants]

  module Module_name = struct
    type t = string

    let of_string raw =
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
        Error (`Invalid_ocaml_module_name raw)

    let error_to_string (`Invalid_ocaml_module_name raw) =
      Printf.sprintf "invalid OCaml module name %S" raw

    let to_string name = name

    let of_json json =
      match of_string (string_of_json json) with
      | Ok name -> name
      | Error error -> Jsonkit.of_json_msg_error (error_to_string error)

    let t_jsonschema =
      `Assoc
        [
          "type", `String "string";
          "pattern", `String "^[A-Za-z][A-Za-z0-9_']*$";
        ]
  end

  type baseline =
    | Parsed_baseline of Sqlgg_migrations.Migration_id.t
    [@@unboxed]

  let baseline_of_json json =
    match Sqlgg_migrations.Migration_id.parse (string_of_json json) with
    | Ok id -> Parsed_baseline id
    | Error error ->
      Jsonkit.of_json_msg_error
        (Sqlgg_migrations.Migration_id.error_to_string error)

  let baseline_jsonschema =
    `Assoc [ "type", `String "string"; "description", `String "a migration id" ]

  type document = {
    lang : host_language option; [@option]
    schema : string option; [@option]
    initial : string option; [@option]
    baseline : baseline option; [@option]
    migrations : string option; [@option]
    manual : string option; [@option]
    module_name : Module_name.t option; [@option] [@key "module"]
    new_tables : Schema_diff.new_tables option; [@option]
    output : string option; [@option]
  }
  [@@deriving of_json, jsonschema] [@@disallow_extra_fields]

  type t = {
    document : document;
    resolve : string -> string;
  }

  let lang config = config.document.lang
  let schema config = Stdlib.Option.map config.resolve config.document.schema
  let initial config = Stdlib.Option.map config.resolve config.document.initial

  let baseline config =
    Stdlib.Option.map
      (fun (Parsed_baseline id) -> id)
      config.document.baseline

  let migrations config =
    Stdlib.Option.map config.resolve config.document.migrations

  let manual config = Stdlib.Option.map config.resolve config.document.manual
  let module_name config = config.document.module_name
  let new_tables config = config.document.new_tables
  let output config = Stdlib.Option.map config.resolve config.document.output
end

let spelled_of_json ~name spellings json =
  let raw = string_of_json json in
  match List.assoc_opt raw spellings with
  | Some value -> value
  | None ->
    Jsonkit.of_json_msg_error
      (Printf.sprintf "unknown %s %S (expected %s)" name raw
         (String.concat "|" (List.map fst spellings)))

let spelled_jsonschema spellings =
  `Assoc
    [
      ( "enum",
        `List (List.map (fun (spelling, _) -> `String spelling) spellings) );
    ]

module Dialect_name : sig
  type t

  val default : t
  val of_json : Jsonkit.t -> t
  val to_json : t -> Jsonkit.t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Dialect.t
end = struct
  type t = Dialect.t

  let default = Dialect.MySQL

  let spellings =
    List.map (fun dialect -> Dialect.to_string dialect, dialect) Dialect.all

  let of_json json = spelled_of_json ~name:"dialect" spellings json
  let to_json dialect = `String (Dialect.to_string dialect)
  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

module Feature_name : sig
  type t

  val of_json : Jsonkit.t -> t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Dialect.feature
end = struct
  type t = Dialect.feature

  let spellings =
    List.map
      (fun feature -> Dialect.feature_to_string feature, feature)
      Dialect.all_of_feature

  let of_json json = spelled_of_json ~name:"feature" spellings json
  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

module Params_name : sig
  type t

  val default : t
  val of_json : Jsonkit.t -> t
  val to_json : t -> Jsonkit.t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Gen.params_selection
end = struct
  type t = Gen.params_selection

  let default = Gen.From_dialect

  let spellings =
    ("auto", Gen.From_dialect)
    :: ("none", Gen.Disabled)
    :: List.map
         (fun placeholder ->
           Sql_template.placeholder_to_string placeholder, Gen.Explicit placeholder)
         Sql_template.all_of_placeholder

  let of_json json = spelled_of_json ~name:"params mode" spellings json

  let to_json = function
    | Gen.From_dialect -> `String "auto"
    | Disabled -> `String "none"
    | Explicit placeholder ->
      `String (Sql_template.placeholder_to_string placeholder)

  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

module Category_name : sig
  type t

  val of_json : Jsonkit.t -> t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Stmt.category
end = struct
  type t = Stmt.category

  let spellings =
    List.map
      (fun category -> Stmt.category_to_string category, category)
      Stmt.all_categories

  let of_json json = spelled_of_json ~name:"category" spellings json
  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

module Category_filter : sig
  type t

  val default : t
  val of_json : Jsonkit.t -> t
  val to_json : t -> Jsonkit.t
  val t_jsonschema : Jsonkit.Jsonschema.t

  val value :
    t ->
    [ `All | `None | `Only of Stmt.category list | `Except of Stmt.category list ]
end = struct
  type t = All | Only of Stmt.category list

  let default = All

  let of_json json =
    Only
      (List.map Category_name.value
         (list_of_json Category_name.of_json json))

  let to_json value =
    let categories =
      match value with All -> Stmt.all_categories | Only categories -> categories
    in
    `List
      (List.map
         (fun category -> `String (Stmt.category_to_string category))
         categories)

  let t_jsonschema =
    `Assoc
      [ "type", `String "array"; "items", Category_name.t_jsonschema ]

  let value = function All -> `All | Only categories -> `Only categories
end

module Alter_algorithm_name : sig
  type t

  val of_json : Jsonkit.t -> t
  val to_json : t -> Jsonkit.t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Sql.alter_algorithm
end = struct
  type t = Sql.alter_algorithm

  let spellings =
    List.map
      (fun algorithm -> Sql.alter_algorithm_to_string algorithm, algorithm)
      Sql.all_of_alter_algorithm

  let of_json json =
    spelled_of_json ~name:"ALTER TABLE algorithm" spellings json

  let to_json algorithm = `String (Sql.alter_algorithm_to_string algorithm)
  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

module Alter_lock_name : sig
  type t

  val of_json : Jsonkit.t -> t
  val to_json : t -> Jsonkit.t
  val t_jsonschema : Jsonkit.Jsonschema.t
  val value : t -> Sql.alter_lock
end = struct
  type t = Sql.alter_lock

  let spellings =
    List.map
      (fun lock -> Sql.alter_lock_to_string lock, lock)
      Sql.all_of_alter_lock

  let of_json json =
    spelled_of_json ~name:"ALTER TABLE lock" spellings json

  let to_json lock = `String (Sql.alter_lock_to_string lock)
  let t_jsonschema = spelled_jsonschema spellings
  let value value = value
end

type header =
  | Full [@name "full"]
  | No_timestamp [@name "no_timestamp"]
  | Static [@name "static"]
  | No_header [@name "none"]
[@@deriving of_json, to_json, jsonschema] [@@compact_variants]

type mode =
  | Generate [@name "generate"]
  | Diff of {
      base : string list; [@default []]
      target : string list; [@default []]
      max_migration_id_length : int;
          [@default Sqlgg_migrations.Migration_id.max_length]
      new_tables : Schema_diff.new_tables;
          [@default Schema_diff.Created_by_schema]
      alter_algorithm : Alter_algorithm_name.t option; [@option]
      alter_lock : Alter_lock_name.t option; [@option]
    } [@name "diff"] [@disallow_extra_fields]
  | Migrate of {
      initial : string list; [@default []]
      migrations_file : string option; [@option]
      extends : string option; [@option]
      target : string list; [@default []]
      max_migration_id_length : int;
          [@default Sqlgg_migrations.Migration_id.max_length]
      new_tables : Schema_diff.new_tables;
          [@default Schema_diff.Created_by_schema]
      alter_algorithm : Alter_algorithm_name.t option; [@option]
      alter_lock : Alter_lock_name.t option; [@option]
    } [@name "migrate"] [@disallow_extra_fields]
[@@deriving of_json, to_json, jsonschema] [@@compact_variants]

type document = {
  dialect : Dialect_name.t; [@default Dialect_name.default]
  no_check : Feature_name.t list; [@default []]
  write_notnull_null : notnull_null_write; [@default Reject]
  header : header; [@default Full]
  gen : gen; [@default No_output]
  name : string; [@default "sqlgg"]
  params : Params_name.t; [@default Params_name.default]
  category : Category_filter.t; [@default Category_filter.default]
  dynamic_select : dynamic_select; [@default Off]
  schema : string list; [@default []]
  inputs : string list; [@default []]
  mode : mode; [@default Generate]
  migrations : Migrations.document option; [@option]
}
[@@deriving of_json, jsonschema] [@@disallow_extra_fields] [@@warning "-69"]

type migrations_document = {
  dialect : Dialect_name.t option; [@option]
  migrations : Migrations.document option; [@option]
}
[@@deriving of_json]

type problem =
  | Unreadable of string
  | Malformed of string
  | No_schema_files of string
  | Schema_with_mode
  | Inputs_with_mode

type error = { file : string; problem : problem }

let error_to_string { file; problem } =
  match problem with
  | Unreadable reason -> Printf.sprintf "cannot read %s: %s" file reason
  | Malformed reason -> Printf.sprintf "%s: %s" file reason
  | No_schema_files pattern ->
    Printf.sprintf "%s: schema %S matches no files" file pattern
  | Schema_with_mode ->
    Printf.sprintf "%s: schema is not supported together with mode" file
  | Inputs_with_mode ->
    Printf.sprintf "%s: inputs are not supported together with mode" file

let load file decode interpret =
  let ( let* ) = Result.bind in
  Result.map_error
    (fun problem -> `Config_file { file; problem })
    (let* document =
       match In_channel.with_open_bin file In_channel.input_all with
       | exception Sys_error reason -> Error (Unreadable reason)
       | text ->
         match decode (Jsonkit.of_string text) with
         | exception Jsonkit.Of_string_error reason -> Error (Malformed reason)
         | exception Jsonkit.Of_json_error reason ->
           Error (Malformed (Jsonkit.of_json_error_to_string reason))
         | document -> Ok document
     in
     let directory = Filename.dirname file in
     interpret
       ~resolve:(fun path ->
         if
           Filename.is_relative path
           && not (String.equal directory Filename.current_dir_name)
         then Filename.concat directory path
         else path)
       document)

let settings file =
  load file document_of_json (fun ~resolve document ->
    match document.mode, document.schema, document.inputs with
    | (Diff _ | Migrate _), _ :: _, _ -> Error Schema_with_mode
    | (Diff _ | Migrate _), [], _ :: _ -> Error Inputs_with_mode
    | _ ->
    let globs = File_glob.create () in
    let given make = function
      | None -> []
      | Some value -> [ make value ]
    in
    let mode_options
        target
        max_migration_id_length
        new_tables
        alter_algorithm
        alter_lock
      =
      List.concat
        [
          List.map (fun file -> Target (resolve file)) target;
          [ Max_migration_id_length max_migration_id_length ];
          [ New_tables new_tables ];
          given
            (fun algorithm ->
              Alter_algorithm (Alter_algorithm_name.value algorithm))
            alter_algorithm;
          given
            (fun lock -> Alter_lock (Alter_lock_name.value lock))
            alter_lock;
        ]
    in
    let mode =
      match document.mode with
      | Generate -> []
      | Diff
          {
            base;
            target;
            max_migration_id_length;
            new_tables;
            alter_algorithm;
            alter_lock;
          } ->
        Diff_mode
        :: List.map (fun file -> Base (resolve file)) base
        @ mode_options target max_migration_id_length new_tables
            alter_algorithm alter_lock
      | Migrate
          {
            initial;
            migrations_file;
            extends;
            target;
            max_migration_id_length;
            new_tables;
            alter_algorithm;
            alter_lock;
          } ->
        Migrate_mode
        :: List.map (fun file -> Initial (resolve file)) initial
        @ given
            (fun file -> Migrations_file (resolve file))
            migrations_file
        @ given (fun file -> Extends (resolve file)) extends
        @ mode_options target max_migration_id_length new_tables
            alter_algorithm alter_lock
    in
    let rec opened = function
      | [] -> Ok []
      | pattern :: rest ->
        match File_glob.expand globs (resolve pattern) with
        | Error `Malformed_pattern ->
          Error (Malformed (Printf.sprintf "invalid schema glob %S" pattern))
        | Error (`Unreadable_directory reason) ->
          Error
            (Malformed
               (Printf.sprintf "cannot read directory for schema %S: %s"
                  pattern reason))
        | Ok [] -> Error (No_schema_files pattern)
        | Ok files ->
          Result.map
            (List.append (List.map (fun file -> Open file) files))
            (opened rest)
    in
    Result.map
      (fun opened ->
        List.concat
         [
           [ Dialect (Dialect_name.value document.dialect) ];
           [ No_check (List.map Feature_name.value document.no_check) ];
           [ Write_notnull_null document.write_notnull_null ];
           [
             Header
               (match document.header with
                | Full -> Some `Full
                | No_timestamp -> Some `Without_timestamp
                | Static -> Some `Static
                | No_header -> None);
           ];
           [ Gen document.gen ];
           [ Name document.name ];
           [ Params (Params_name.value document.params) ];
           [ Category (Category_filter.value document.category) ];
           [ Dynamic_select document.dynamic_select ];
           mode;
           opened;
           List.map
             (function "-" -> Stdin | path -> Input (resolve path))
             document.inputs;
         ])
      (opened document.schema))

let migrations file =
  load file migrations_document_of_json
    (fun ~resolve (document : migrations_document) ->
    Ok
      ( Stdlib.Option.map Dialect_name.value document.dialect,
        Stdlib.Option.map
          (fun section -> { Migrations.document = section; resolve })
          document.migrations ))
