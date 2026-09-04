open Stdlib

type include_ =
  | Reuse [@as "reuse"]
  | Execute [@as "execute"]
  | Reuse_and_execute [@as "reuse_and_execute"]
[@@deriving eq, of_string]

type dynamic_select =
  | Off [@as "false"]
  | Only [@as "true"]
  | Both [@as "both"]
[@@deriving eq, of_string]

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
  | Sql of string
  | File of string
  | Noop
[@@deriving eq]

let has prop = List.exists (equal prop)
let name props = List.find_map (function Name n -> Some n | _ -> None) props
let include_ props = Option.value ~default:Execute (List.find_map (function Include i -> Some i | _ -> None) props)
let dynamic_select props = List.find_map (function Dynamic_select d -> Some d | _ -> None) props
let substs props = List.filter_map (function Subst s -> Some s | _ -> None) props
let id props = List.find_map (function Id i -> Some i | _ -> None) props
let down props = List.find_map (function Down d -> Some d | _ -> None) props
