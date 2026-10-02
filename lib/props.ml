open Stdlib
open Jsonkit.Primitives

type include_ =
  | Reuse [@as "reuse"]
  | Execute [@as "execute"]
  | Reuse_and_execute [@as "reuse_and_execute"]
[@@deriving eq, of_string, json, jsonschema] [@@compact_variants]

type dynamic_select =
  | Off [@as "false"]
  | Only [@as "true"]
  | Both [@as "both"]
[@@deriving eq, of_string, json, jsonschema] [@@compact_variants]

type t =
  | Name of string
  | Include of include_
  | Noparse
  | Dynamic_select of dynamic_select
  | Subst of string
  | Id of string
  | Down of string
  | Irreversible
  | Auto
  | Manual
  | File of string
  | Noop
[@@deriving eq, json, jsonschema] [@@compact_variants]

let has prop = List.exists (equal prop)
let name props = List.find_map (function Name n -> Some n | _ -> None) props
let include_ props = Option.value ~default:Execute (List.find_map (function Include i -> Some i | _ -> None) props)
let dynamic_select props = List.find_map (function Dynamic_select d -> Some d | _ -> None) props
let resolve_dynamic_select ~default_enabled props =
  match dynamic_select props, default_enabled with
  | Some mode, _ -> mode
  | None, true -> Both
  | None, false -> Off
let substs props = List.filter_map (function Subst s -> Some s | _ -> None) props
let id props = List.find_map (function Id i -> Some i | _ -> None) props
let down props = List.find_map (function Down d -> Some d | _ -> None) props
