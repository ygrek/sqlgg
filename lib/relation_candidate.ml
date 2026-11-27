open Jsonkit.Primitives

type origin =
  | Table of Sql.table
  | Query of Shared_queries.entry
[@@deriving json, jsonschema]

type t = {
  origin : origin;
  builder : string;
  constructor : string;
}
[@@deriving to_json, jsonschema]

let make origin =
  let prefix, raw =
    match origin with
    | Table (name, _) -> "table_", Sql.show_table_name name
    | Query entry -> "query_", entry.name
  in
  let normalized =
    String.map (function
      | 'a'..'z' | '0'..'9' | '_' as c -> c
      | 'A'..'Z' as c -> Char.lowercase_ascii c
      | _ -> '_') raw
  in
  let builder = prefix ^ normalized in
  { origin; builder; constructor = String.capitalize_ascii builder }

(* the generated names follow from the origin, they are not read back *)
let of_json json =
  let module Stored = struct
    type t = { origin : origin } [@@deriving of_json] [@@json.allow_extra_fields]
  end in
  make (Stored.of_json json).origin

type error = Name_collision of { builder : string; first : string; second : string }

let show_error (Name_collision { builder; first; second }) =
  Printf.sprintf "relation candidates %s and %s both map to the generated name %s" first second builder

let source_name c =
  match c.origin with
  | Table (name, _) -> Sql.show_table_name name
  | Query entry -> "&" ^ entry.name

let all () =
  let tables = List.rev_map (fun table -> make (Table table)) (Tables.all ()) in
  let queries = List.map (fun entry -> make (Query entry)) (Shared_queries.entries ()) in
  let module Builders = Map.Make (String) in
  List.fold_left (fun acc candidate ->
    Result.bind acc (fun (seen, kept) ->
      match Builders.find_opt candidate.builder seen with
      | Some previous ->
        Error (Name_collision {
          builder = candidate.builder;
          first = source_name previous;
          second = source_name candidate })
      | None -> Ok (Builders.add candidate.builder candidate seen, candidate :: kept)))
    (Ok (Builders.empty, [])) (tables @ queries)
  |> Result.map (fun (_, kept) -> List.rev kept)
