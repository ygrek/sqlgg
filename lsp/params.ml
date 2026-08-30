open Sqlgg

type token_type =
  | Parameter [@as "parameter"]
  | Enum [@as "enum"]
  | Enum_member [@as "enumMember"]
[@@deriving enum, enumerate, to_string]

type kind =
  | Var of Sql.param_id * Sql.var
  | Branch of Sql.param_id * Sql.ctor

type node = {
  base : int;
  kind : kind;
  children : node list;
}

let name (id : Sql.param_id) = match id.value with Some name -> "@" ^ name | None -> "?"

let cursor_pos node =
  let pos =
    match node.kind with
    | Var (_, DynamicSelect _) -> None
    | Var (id, _) ->
      Option.map (fun name -> fst id.pos, fst id.pos + 1 + String.length name) id.value
    | Branch (_, Simple { ctor; ctor_pos; _ }) ->
      Some (if Pos.is_empty ctor_pos then ctor.pos else fst ctor_pos, snd ctor.pos)
    | Branch (_, Verbatim _) -> None
  in
  Option.map (Pos.shift node.base) pos

let token_pos node =
  let pos =
    match node.kind with
    | Var (id, (Sql.Single _ | SingleIn _)) -> Some id.pos
    | Var (_, DynamicSelect _) -> None
    | Var (id, (Choice _ | ChoiceIn _ | DynamicSelectJoin _ | TupleList _ | OptionActionChoice _)) ->
      Option.map (fun name -> fst id.pos, fst id.pos + 1 + String.length name) id.value
    | Var (_, SharedVarsGroup _) -> None
    | Branch (_, Simple { ctor_pos; _ }) -> if Pos.is_empty ctor_pos then None else Some ctor_pos
    | Branch (_, Verbatim _) -> None
  in
  Option.map (Pos.shift node.base) pos

let label node =
  match node.kind with
  | Var (id, var) ->
    let suffix =
      match var with
      | ChoiceIn { kind = `In; _ } -> " — IN"
      | ChoiceIn { kind = `NotIn; _ } -> " — NOT IN"
      | DynamicSelect _ -> " — dynamic select"
      | DynamicSelectJoin _ -> " — dynamic join"
      | TupleList _ -> " — tuple list"
      | Single _ | SingleIn _ | Choice _ | OptionActionChoice _ | SharedVarsGroup _ -> ""
    in
    name id ^ suffix
  | Branch (_, Simple { ctor; _ }) -> Option.value ~default:"_" ctor.value
  | Branch (_, Verbatim (name, _)) -> name

let of_vars ~base vars =
  let create ?(children = []) kind = { base; kind; children } in
  let param_node ?children var id = create ?children (Var (id, var)) in
  let rec of_var (var : Sql.var) =
    match var with
    | Sql.Single (p, _) | Sql.SingleIn (p, _) -> [ create (Var (p.id, var)) ]
    | Sql.ChoiceIn { param; vars; _ } ->
      let pos = match vars with [ Sql.SingleIn (p, _) ] -> p.id.pos | _ -> param.pos in
      [ param_node ~children:(of_vars vars) var { param with pos } ]
    | Sql.Choice (id, ctors) | Sql.DynamicSelect (id, ctors) ->
      [ param_node ~children:(List.map (of_ctor id) ctors) var id ]
    | Sql.DynamicSelectJoin { pid; _ } -> [ param_node var pid ]
    | Sql.TupleList (id, _) -> [ param_node var id ]
    | Sql.OptionActionChoice (id, vars, _, _) -> [ param_node ~children:(of_vars vars) var id ]
    | Sql.SharedVarsGroup (vars, _) -> of_vars vars
  and of_ctor choice (c : Sql.ctor) =
    match c with
    | Sql.Simple _ ->
      create ~children:(of_vars (Sql.ctor_vars c)) (Branch (choice, c))
    | Sql.Verbatim _ -> create (Branch (choice, c))
  and of_vars vars = List.concat_map of_var vars in
  of_vars vars

type shape =
  | Scalar of Sql.Type.t
  | List of Sql.Type.t list
  | Compound

let rec shape node =
  match node.kind with
  | Var (_, (Sql.Single (p, _) | SingleIn (p, _))) -> Scalar p.typ
  | Var (_, ChoiceIn _) ->
    List (List.filter_map (fun child -> match shape child with Scalar t -> Some t | List _ | Compound -> None) node.children)
  | Var (_, TupleList (_, Where_in { value = (types, _); _ })) -> List (List.map fst types)
  | Var (_, TupleList (_, ValueRows { types; _ })) -> List types
  | Var (_, TupleList (_, Insertion schema)) -> List (List.map (fun (attr : Sql.attr) -> attr.domain) schema)
  | Var (_, (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _ | SharedVarsGroup _))
  | Branch _ -> Compound

let outline nodes =
  let module Occurrence = struct
    type t = { branch : bool; pos : int * int; name : string option } [@@deriving ord]

    let of_node node =
      match node.kind with
      | Var (id, _) -> { branch = false; pos = id.pos; name = id.value }
      | Branch (_, Simple { ctor; _ }) -> { branch = true; pos = ctor.pos; name = ctor.value }
      | Branch (choice, Verbatim (name, _)) ->
        { branch = true; pos = choice.pos; name = Some name }
  end in
  let rec loop nodes =
    Prelude.unique_by (module Occurrence) Occurrence.of_node nodes
    |> List.map (fun node -> { node with children = loop node.children })
  in
  loop nodes

let all_nodes nodes =
  let rec preorder node = Seq.cons node (Seq.concat_map preorder (List.to_seq node.children)) in
  Seq.concat_map preorder (List.to_seq nodes)

let find_node nodes offset ~f =
  all_nodes nodes
  |> Seq.filter_map (fun node ->
    match cursor_pos node, f node with
    | Some pos, Some x -> Some (x, pos)
    | _ -> None)
  |> Pos.find_innermost offset
