open Jsonkit.Primitives

exception Unsupported of string

let () = Printexc.register_printer (function Unsupported msg -> Some msg | _ -> None)

let unsupported ~at fmt = Printf.ksprintf (fun s -> raise (Prelude.At (at, Unsupported s))) fmt

module Name = struct
  type t = string [@@deriving eq, ord, json, jsonschema]
  let of_param_id (id : Sql.param_id) = id.value
end

type column = { slot : Name.t; column : string } [@@deriving eq, ord, json, jsonschema]

type kind_class =
  | Int_kind
  | UInt64_kind
  | Text_kind
  | Blob_kind
  | Float_kind
  | Decimal_kind
  | Bool_kind
  | Datetime_kind
  | Json_kind
  | Enum_kind of string list
[@@deriving eq, json, jsonschema] [@@compact_variants]

let kind_class (typ : Sql.Type.t) =
  match typ.t with
  | Int -> Some Int_kind
  | UInt64 -> Some UInt64_kind
  | Text | StringLiteral _ | Json_path | One_or_all -> Some Text_kind
  | Blob -> Some Blob_kind
  | Float | FloatingLiteral _ -> Some Float_kind
  | Decimal _ -> Some Decimal_kind
  | Bool -> Some Bool_kind
  | Datetime -> Some Datetime_kind
  | Json -> Some Json_kind
  | Union { ctors; _ } -> Some (Enum_kind (Sql.Type.Enum_kind.Ctors.elements ctors))
  | Any -> None

type binding =
  | Open
  | Bound of Sql.schema

let binding bound name =
  match bound with
  | Some (slot, schema) when Name.equal slot name -> Bound schema
  | Some _ | None -> Open

type use = { column : column; pos : Sql.Pos.t; expected : kind_class list }

type own_param = { name : string; param : Sql.Type.t Sql.param; meta : Sql.Meta.t }
[@@deriving json, jsonschema]

type index =
  | Column of int
  | Parameter of string
  | Coupling of column
[@@deriving eq]

type evidence =
  | Column_type of int * Sql.attr
  | Parameter_type of string * (Sql.Type.t * Sql.Meta.t)
  | Coupling_kind of column * kind_class
[@@deriving json, jsonschema]

let index = function
  | Column_type (i, _) -> Column i
  | Parameter_type (name, _) -> Parameter name
  | Coupling_kind (column, _) -> Coupling column

type instance = {
  candidate : Relation_candidate.t;
  own_params : own_param list;
  evidence : evidence list;
}
[@@deriving json, jsonschema]

type rejection = { rejected : Relation_candidate.t; reason : string }
[@@deriving json, jsonschema]

type t = {
  name : Name.t;
  instances : instance * instance list;
  rejections : rejection list;
}
[@@deriving json, jsonschema]

let instances t = fst t.instances :: snd t.instances
let indices t = List.map index (fst t.instances).evidence
