open ExtLib
open Sql

module Qualified_attr = struct
  module T = struct
    type t = { sources : table_name list; name : string } [@@deriving eq, ord]
  end
  include T

  let of_attr (a : table_name Schema.Source.Attr.t) = { sources = a.sources; name = a.attr.name }
  let named = function { name = ""; _ } -> None | key -> Some key

  module Map = Map.Make(T)
  module Set = Set.Make(T)
end

module Attr_refinement = struct
  type t = { not_null : Qualified_attr.Set.t; meta : Meta.t Qualified_attr.Map.t }

  let empty = { not_null = Qualified_attr.Set.empty; meta = Qualified_attr.Map.empty }

  let add a b = {
    not_null = Qualified_attr.Set.union a.not_null b.not_null;
    meta = Qualified_attr.Map.union (fun _ x y -> Some (Meta.merge_right x y)) a.meta b.meta }

  let keep_all = List.fold_left add empty

  let keep_shared = function
    | [] -> empty
    | x :: l ->
      List.fold_left (fun a b -> {
        not_null = Qualified_attr.Set.inter a.not_null b.not_null;
        meta = Qualified_attr.Map.merge (fun _ x y ->
          match x, y with Some x, Some y -> Meta.declared (Meta.inter x y) | _ -> None) a.meta b.meta }) x l

  let not_null attr = { empty with not_null = Qualified_attr.Set.singleton attr }
  let restrict_not_null keep t = { t with not_null = Qualified_attr.Set.filter keep t.not_null }

  let inherit_meta ~constrains (col : table_name Schema.Source.Attr.t)
      ~(referenced : table_name Schema.Source.Attr.t) =
    let inherited = Meta.of_domain referenced.attr.meta in
    let carries =
      match col.attr.domain.t, referenced.attr.domain.t with
      | Union a, Union b -> Type.Enum_kind.Ctors.subset a.ctors b.ctors
      | a, b -> constrains col && Type.equal_kind a b
    in
    if carries && not (Meta.is_empty inherited)
    then { empty with meta = Qualified_attr.Map.singleton (Qualified_attr.of_attr col) inherited }
    else empty

  let refine_nullability t a =
    if Qualified_attr.Set.mem (Qualified_attr.of_attr a) t.not_null
    then Schema.Source.Attr.map_attr (fun attr -> { attr with domain = Type.make_strict attr.domain }) a
    else a

  let apply t a =
    let inherited = Option.default (Meta.empty ()) (Qualified_attr.Map.find_opt (Qualified_attr.of_attr a) t.meta) in
    refine_nullability t
      (Schema.Source.Attr.map_attr (fun attr -> { attr with meta = Meta.merge_right inherited attr.meta }) a)
end

let narrow_columns ~resolve ~constrains e =
  let open Attr_refinement in
  let strict col =
    match resolve col with
    | Some a when constrains a -> not_null (Qualified_attr.of_attr a)
    | Some _ | None -> empty
  in
  let borrow_meta = function
    | Sql.Fun { kind = Comparison (Comp_equal | Not_distinct_op); parameters = [Column a; Column b]; _ } ->
      begin match resolve a.collated, resolve b.collated with
      | Some a, Some b ->
        let borrow = inherit_meta ~constrains in
        add (borrow a ~referenced:b) (borrow b ~referenced:a)
      | None, _ | _, None -> empty
      end
    | _ -> empty
  in
  let rec narrow (e : Sql.expr) known =
    let same e = narrow e known in
    let has_value e = narrow e `Has_value in
    let every_path { Sql.case; branches; else_ } =
      let condition when_ =
        match case with
        | Some x -> add (has_value x) (has_value when_)
        | None -> narrow when_ `Holds
      in
      let per_branch = List.map (fun { Sql.when_; then_ } -> add (condition when_) (same then_)) branches in
      let no_branch = Option.map_default (fun e -> [ same e ]) [] else_ in
      keep_shared (per_branch @ no_branch)
    in
    match known with
    | `Has_value ->
      begin match e with
      | Column col -> strict col.collated
      | Fun { kind = Null_handling (Coalesce _ | If_null); parameters; _ } -> keep_shared (List.map same parameters)
      | Fun { kind; parameters; _ } ->
        let strict_args =
          match kind, parameters with
          | (Comparison (Comp_equal | Comp_num_cmp | Comp_text_cmp | Comp_num_eq)
            | Negation | Arith _ | Like | Like_escape), _ -> parameters
          | Membership, _ :: (SelectExpr _ | Inparam _) :: _ -> []
          | (Membership | Range), _ -> List.take 1 parameters
          | (Comparison (Not_distinct_op | Is_null | Is_not_null)
            | Quantified_comparison _
            | Agg _ | Null_handling _ | Logical _ | Ret _ | F _ | Col_assign _ | Multi _), _ -> []
        in
        keep_all (List.map same strict_args)
      | Case c -> every_path c
      | Value _ | Param _ | Inparam _ | Choices _ | InChoice _ | InTupleList _
      | SelectExpr _ | OptionActions _ | Of_values _ -> empty
      end
    | (`Holds | `Fails) as known ->
      let required =
        match e with
        | Fun { kind = Logical ((And | Or) as op); parameters; _ } ->
          let combine = match op, known with And, `Holds | Or, `Fails -> keep_all | _ -> keep_shared in
          combine (List.map same parameters)
        | Fun { kind = Logical Xor; parameters; _ } ->
          keep_all (List.map (fun e -> keep_shared [narrow e `Holds; narrow e `Fails]) parameters)
        | Fun { kind = Negation; parameters = [e]; _ } ->
          let opposite = match known with `Holds -> `Fails | `Fails -> `Holds in
          narrow e opposite
        | Fun { kind = Comparison Is_not_null; parameters = [e]; _ }
        | Fun { kind = Quantified_comparison
                  { op = Comp_equal | Comp_num_cmp | Comp_text_cmp | Comp_num_eq; quantifier = `Any };
                parameters = e :: _; _ } ->
          begin match known with `Holds -> has_value e | `Fails -> empty end
        | Fun { kind = Comparison Is_null; parameters = [e]; _ } ->
          begin match known with `Holds -> empty | `Fails -> has_value e end
        | Fun { kind = Quantified_comparison _; _ } -> empty
        | Case c -> every_path c
        | Choices (_, alternatives) ->
          keep_shared (List.map (fun (_, e) -> Option.map_default same empty e) alternatives)
        | InChoice _ | OptionActions _ -> empty
        | e -> has_value e
      in
      let from_equality = match known with `Holds -> borrow_meta e | `Fails -> empty in
      add from_equality required
  in
  narrow e `Holds
