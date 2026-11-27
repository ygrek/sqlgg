open Sqlgg

type token_type =
  | Parameter [@as "parameter"]
  | Enum [@as "enum"]
  | Enum_member [@as "enumMember"]
[@@deriving enumerate, to_string]

type kind =
  | Var of Sql.var
  | Branch of Sql.ctor

type placement = { cursor : Sql.Pos.t; token : Sql.Pos.t option }

type node = {
  param : Sql.param_id;
  kind : kind;
  placement : placement option;
  children : node list;
}

let name (id : Sql.param_id) =
  match id.value with Some name -> "@" ^ name | None -> "?"

let token_type node =
  match node.kind with
  | Var (Sql.Single _ | SingleIn _ | ChoiceIn _ | TupleList _ | SharedVarsGroup _ | RelationSlot _) -> Parameter
  | Var (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _) -> Enum
  | Branch _ -> Enum_member

let label node =
  match node.kind with
  | Var var ->
    let suffix =
      match var with
      | ChoiceIn { kind = `In; _ } -> " — IN"
      | ChoiceIn { kind = `NotIn; _ } -> " — NOT IN"
      | DynamicSelect _ -> " — dynamic select"
      | DynamicSelectJoin _ -> " — dynamic join"
      | RelationSlot _ -> " — relation"
      | TupleList _ -> " — tuple list"
      | Single _ | SingleIn _ | Choice _ | OptionActionChoice _ | SharedVarsGroup _ -> ""
    in
    name node.param ^ suffix
  | Branch (Simple { ctor; _ }) -> Option.value ~default:"_" ctor.value
  | Branch (Verbatim (name, _)) -> name

let of_vars ~base vars =
  let sigil_pos (id : Sql.param_id) =
    Option.map
      (fun name -> Sql.Pos.span id.pos (fst id.pos, fst id.pos + 1 + String.length name))
      id.value
  in
  let placement param = function
    | Var (Sql.DynamicSelect _) -> None
    | Var (Sql.Single _ | SingleIn _) ->
      Some { cursor = Option.value (sigil_pos param) ~default:param.pos;
        token = Some param.pos }
    | Var (Choice _ | ChoiceIn _ | DynamicSelectJoin _ | TupleList _
          | OptionActionChoice _ | SharedVarsGroup _ | RelationSlot _) ->
      Some { cursor = Option.value (sigil_pos param) ~default:param.pos;
        token = sigil_pos param }
    | Branch (Sql.Simple { ctor; ctor_pos; _ }) ->
      if Sql.Pos.is_empty ctor_pos
      then Some { cursor = ctor.pos; token = None }
      else Some { cursor = Sql.Pos.span ctor_pos ctor.pos; token = Some ctor_pos }
    | Branch (Verbatim _) -> None
  in
  let create ?(children = []) ~visible param kind =
    let shift_pos = Sql.Pos.shift base in
    let placement =
      if not visible then None
      else
        Option.map
          (fun { cursor; token } ->
            { cursor = shift_pos cursor; token = Option.map shift_pos token })
          (placement param kind)
    in
    { param; kind; placement; children }
  in
  let rec of_var ~visible (var : Sql.var) =
    let param_node ?children param = create ?children ~visible param (Var var) in
    match var with
    | Sql.Single (p, _) | Sql.SingleIn (p, _) -> [ param_node p.id ]
    | Sql.ChoiceIn { param; vars; _ } ->
      let pos = match vars with [ Sql.SingleIn (p, _) ] -> p.id.pos | _ -> param.pos in
      [ param_node ~children:(of_list ~visible vars) { param with pos } ]
    | Sql.Choice (id, ctors) | Sql.DynamicSelect (id, ctors) ->
      [ param_node ~children:(List.map (of_ctor ~visible id) ctors) id ]
    | Sql.DynamicSelectJoin { pid; _ } -> [ param_node pid ]
    | Sql.RelationSlot { id; _ } -> [ param_node id ]
    | Sql.TupleList (id, _) -> [ param_node id ]
    | Sql.OptionActionChoice (id, vars, _, _) ->
      [ param_node ~children:(of_list ~visible vars) id ]
    | Sql.SharedVarsGroup (vars, _) -> of_list ~visible:false vars
  and of_ctor ~visible choice (ctor : Sql.ctor) =
    create ~visible ~children:(of_list ~visible (Sql.ctor_vars ctor)) choice (Branch ctor)
  and of_list ~visible vars = List.concat_map (of_var ~visible) vars in
  of_list ~visible:true vars

let rec shape node =
  match node.kind with
  | Var (Sql.Single (p, _) | SingleIn (p, _)) -> `Scalar p.typ
  | Var (ChoiceIn _) ->
    let rec scalars acc = function
      | [] -> `Row (List.rev acc)
      | child :: rest ->
        match shape child with
        | `Scalar typ -> scalars (typ :: acc) rest
        | `Row _ | `Compound -> `Compound
    in
    scalars [] node.children
  | Var (TupleList (_, Where_in { value = (types, _); _ })) ->
    `Row (List.map fst types)
  | Var (TupleList (_, ValueRows { types; _ })) -> `Row types
  | Var (TupleList (_, Insertion schema)) ->
    `Row (List.map (fun (attr : Sql.attr) -> attr.domain) schema)
  | Var (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _ | SharedVarsGroup _ | RelationSlot _)
  | Branch _ -> `Compound

let outline nodes =
  let key_of_node node =
    match node.kind with
    | Var _ -> `Var, node.param.pos, node.param.value
    | Branch (Simple { ctor; _ }) -> `Branch, ctor.pos, ctor.value
    | Branch (Verbatim (name, _)) -> `Branch, node.param.pos, Some name
  in
  let equal_key (kind, pos, name) (kind', pos', name') =
    let equal_kind =
      match kind, kind' with
      | `Var, `Var | `Branch, `Branch -> true
      | (`Var | `Branch), _ -> false
    in
    equal_kind
    && Sql.Pos.equal pos pos'
    && Option.equal String.equal name name'
  in
  let rec loop seen = function
    | [] -> []
    | node :: rest ->
      let key = key_of_node node in
      if List.exists (equal_key key) seen then loop seen rest
      else { node with children = loop [] node.children } :: loop (key :: seen) rest
  in
  loop [] nodes

let all_nodes nodes =
  let rec preorder node = Seq.cons node (Seq.concat_map preorder (List.to_seq node.children)) in
  Seq.concat_map preorder (List.to_seq nodes)
