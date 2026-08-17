(* Property-based laws for Type and Meta. *)

open Printf
open ExtLib
open OUnit
open Sqlgg
open Sql

let any_of l = QCheck.Gen.(oneof (List.map return l))

let arb_type =
  let open QCheck.Gen in
  let simple = Type.[ Int; UInt64; Text; Blob; Float; Bool; Datetime; Json; Json_path; One_or_all; Any ] in
  let ctors = list_size (int_range 1 3) (any_of [ "a"; "b"; "c" ]) in
  let kind = oneof [
    any_of simple;
    map2 (fun ctors is_closed -> Type.Union { ctors = Type.Enum_kind.make ctors; is_closed }) ctors bool;
    map (fun s -> Type.StringLiteral s) (any_of [ "a"; "b"; "{}" ]);
    map (fun f -> Type.FloatingLiteral f) (any_of [ 0.; 1.5 ]);
    map2 (fun precision scale -> Type.Decimal { precision; scale }) (option (int_range 1 10)) (option (int_range 0 4));
  ] in
  QCheck.make ~print:Type.show
    (map2 (fun t nullability -> { Type.t; nullability }) kind (any_of Type.[ Nullable; Strict; Depends ]))

let same_result a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> Type.equal a b
  | None, Some _ | Some _, None -> false

let qcheck (QCheck2.Test.Test cell) =
  QCheck2.Test.get_name cell >:: (fun () ->
    try QCheck2.Test.check_cell_exn ~rand:(Random.State.make [| 42 |]) cell
    with QCheck2.Test.Test_fail (_, msgs) -> assert_failure (String.concat "\n" msgs))

let arb_meta =
  let open QCheck.Gen in
  let entry = pair (any_of [ "module"; "get_column"; "set_param" ]) (any_of [ "A"; "B" ]) in
  QCheck.make ~print:(Format.asprintf "%a" Meta.pp)
    (map Meta.of_list (list_size (int_range 0 3) entry))

let arb_opt_meta = QCheck.option arb_meta

let same_meta a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> Meta.equal a b
  | None, Some _ | Some _, None -> false

let test_meta_laws = List.map qcheck [
  QCheck.Test.make ~count:2000 ~name:"common is commutative"
    (QCheck.pair arb_opt_meta arb_opt_meta)
    (fun (a, b) -> same_meta (Meta.common a b) (Meta.common b a));
  QCheck.Test.make ~count:2000 ~name:"common is associative"
    (QCheck.triple arb_opt_meta arb_opt_meta arb_opt_meta)
    (fun (a, b, c) -> same_meta (Meta.common a (Meta.common b c)) (Meta.common (Meta.common a b) c));
  QCheck.Test.make ~count:2000 ~name:"common is idempotent"
    arb_opt_meta (fun a -> same_meta (Meta.common a a) a);
  QCheck.Test.make ~count:2000 ~name:"an absent meta is the identity"
    arb_opt_meta (fun a -> same_meta (Meta.common None a) a);
  QCheck.Test.make ~count:2000 ~name:"an empty meta absorbs"
    arb_opt_meta
    (fun a -> same_meta (Meta.common (Some (Meta.empty ())) a) (Some (Meta.empty ())));
  QCheck.Test.make ~count:2000 ~name:"merge_right is associative"
    (QCheck.triple arb_meta arb_meta arb_meta)
    (fun (a, b, c) -> Meta.equal (Meta.merge_right a (Meta.merge_right b c)) (Meta.merge_right (Meta.merge_right a b) c));
  QCheck.Test.make ~count:2000 ~name:"merge_right keeps every key of the right side"
    (QCheck.pair arb_meta arb_meta)
    (fun (a, b) ->
      let m = Meta.merge_right a b in
      Meta.StringMap.for_all (fun k v -> Option.map_default (String.equal v) false (Meta.find_opt m k)) b);
  QCheck.Test.make ~count:2000 ~name:"merge_right keeps the left keys the right side does not mention"
    (QCheck.pair arb_meta arb_meta)
    (fun (a, b) ->
      let m = Meta.merge_right a b in
      Meta.StringMap.for_all (fun k v ->
        Option.is_some (Meta.find_opt b k)
        || Option.map_default (String.equal v) false (Meta.find_opt m k)) a);
  QCheck.Test.make ~count:2000 ~name:"an undeclared side does not erase"
    arb_meta (fun a -> Meta.equal (Meta.shared [ a; Meta.empty () ]) a);
]

let enums_of_different_shape a b =
  match a.Type.t, b.Type.t with
  | Type.Union x, Type.Union y -> not (Type.Enum_kind.Ctors.equal x.ctors y.ctors)
  | _ -> false

let test_type_laws = List.map qcheck [
  (* TODO enums of different shape are excluded: we keep one direction failing on purpose,
     see the TODO in order_kind. *)
  QCheck.Test.make ~count:2000 ~name:"common_type is commutative"
    (QCheck.pair arb_type arb_type)
    (fun (a, b) ->
      QCheck.assume (not (enums_of_different_shape a b));
      same_result (Type.common_type a b) (Type.common_type b a));
  QCheck.Test.make ~count:2000 ~name:"common_type is idempotent"
    arb_type
    (fun a -> same_result (Type.common_type a a) (Some a));
  QCheck.Test.make ~count:2000 ~name:"subtype and supertype are defined together"
    (QCheck.pair arb_type arb_type)
    (fun (a, b) -> Bool.equal (Option.is_some (Type.subtype a b)) (Option.is_some (Type.supertype a b)));
  QCheck.Test.make ~count:2000 ~name:"a nullable side makes the result nullable"
    (QCheck.pair arb_type arb_type)
    (fun (a, b) ->
      match Type.common_type a b with
      | Some r -> not (Type.is_nullable a || Type.is_nullable b) || Type.is_nullable r
      | None -> true);
  QCheck.Test.make ~count:2000 ~name:"two strict sides stay strict"
    (QCheck.pair arb_type arb_type)
    (fun (a, b) ->
      match Type.common_type a b with
      | Some r -> not (Type.is_strict a && Type.is_strict b) || Type.is_strict r
      | None -> true);
]

module Narrowing_model = struct

  let t1 = Sql.make_table_name "t1"
  let t2 = Sql.make_table_name "t2"

  let columns = [| t1, "a"; t2, "a"; t1, "b" |]
  let n_columns = Array.length columns
  let all_columns = Array.to_list (Array.init n_columns (fun i -> i))

  let show_column i = let (tn, name) = columns.(i) in sprintf "%s.%s" (Sql.show_table_name tn) name
  let show_column_set m =
    "{" ^ String.concat ", " (List.map show_column (List.filter (Array.get m) all_columns)) ^ "}"

  let refs = [|
    [ { cname = "a"; tname = Some t1 } ];
    [ { cname = "a"; tname = Some t2 } ];
    [ { cname = "b"; tname = Some t1 }; { cname = "b"; tname = None } ];
  |]

  let column_of_ref (c : col_name) =
    let hit i =
      let (tn, name) = columns.(i) in
      String.equal name c.cname && Option.map_default (Sql.equal_table_name tn) true c.tname
    in
    match List.filter hit all_columns with
    | [ i ] -> Some i
    | [] | _ :: _ :: _ -> None

  let call kind parameters = Sql.fn "test" kind parameters
  let conj a b = call (Logical And) [ a; b ]
  let neg a = call Negation [ a ]
  let is_not_null a = call (Comparison Is_not_null) [ a ]
  let subquery =
    let select = { columns = []; from = None; where = None; group = []; having = None } in
    SelectExpr ({ select_complete = { select = select, []; order = []; limit = None;
                                      select_row_locking = None }; cte = None }, `AsValue)

  let like a b = call Like [ a; b ]
  let like_escape a b esc = call Like_escape [ like a b; esc ]

  let list_param = make_located ~value:(Some "p") ~pos:(0, 0)

  let in_param kind x =
    let arg = Inparam (make_param ~id:list_param ~typ:(Source_type.depends Any), Meta.empty ()) in
    InChoice (list_param, kind, call Membership [ x; arg ])

  let in_tuple_list kind exprs =
    InTupleList (make_located ~pos:(0, 0)
      ~value:{ exprs; param_id = list_param; kind_in_tuple_list = kind })

  let binary_comparisons = [ Comp_equal; Comp_num_eq; Comp_num_cmp; Comp_text_cmp; Not_distinct_op ]

  let narrowed e =
    let resolve (c : col_name) : table_name Schema.Source.Attr.t option =
      column_of_ref c |> Option.map (fun i ->
        let (tn, name) = columns.(i) in
        { Schema.Source.Attr.attr = make_attribute' name Type.(nullable Int); sources = [ tn ] })
    in
    (Narrowing.narrow_columns ~resolve ~constrains:(fun _ -> true) e)
      .Narrowing.Attr_refinement.not_null

  let any_of l = QCheck2.Gen.(oneof (List.map return l))

  let gen_column =
    let open QCheck2.Gen in
    int_range 0 (n_columns - 1) >>= fun i -> map Sql.column (any_of refs.(i))

  let rec gen_scalar depth =
    let open QCheck2.Gen in
    if depth <= 0 then gen_column
    else frequency [
      5, gen_column;
      2, map2 (fun a b -> call (Arith (Source_type.depends Any)) [ a; b ])
           (gen_scalar (depth - 1)) (gen_scalar (depth - 1));
      2, map2 (fun a b -> call (Null_handling (Coalesce (Type.Var 0, Type.Var 0))) [ a; b ])
           (gen_scalar (depth - 1)) (gen_scalar (depth - 1));
      1, map2 (fun a b -> call (Null_handling If_null) [ a; b ])
           (gen_scalar (depth - 1)) (gen_scalar (depth - 1));
      2, map2 (fun branches else_ -> Case { case = None; branches; else_ })
           (list_size (int_range 1 2)
              (map2 (fun when_ then_ -> { when_; then_ }) (gen_cond (depth - 1)) (gen_scalar (depth - 1))))
           (option (gen_scalar (depth - 1)));
      1, map3 (fun scrutinee branches else_ -> Case { case = Some scrutinee; branches; else_ })
           (gen_scalar (depth - 1))
           (list_size (int_range 1 2)
              (map2 (fun when_ then_ -> { when_; then_ }) (gen_scalar (depth - 1)) (gen_scalar (depth - 1))))
           (option (gen_scalar (depth - 1)));
    ]

  and gen_cond depth =
    let open QCheck2.Gen in
    let scalar = gen_scalar (min depth 2) in
    let leaf = frequency [
      4, map3 (fun op a b -> call (Comparison op) [ a; b ]) (any_of binary_comparisons) scalar scalar;
      2, map is_not_null scalar;
      2, map (fun x -> call (Comparison Is_null) [ x ]) scalar;
      2, map2 (fun x l -> call Membership (x :: l)) scalar (list_size (int_range 1 2) scalar);
      2, map (fun x -> call Membership [ x; subquery ]) scalar;
      1, map3 (fun x lo hi -> call Range [ x; lo; hi ]) scalar scalar scalar;
      2, map2 in_param (any_of [ `In; `NotIn ]) scalar;
      2, map2 in_tuple_list (any_of [ `In; `NotIn ]) (list_size (int_range 1 2) scalar);
      2, map3 (fun op quantifier x -> call (Quantified_comparison { op; quantifier }) [ x; subquery ])
           (any_of binary_comparisons) (any_of [ `Any; `All ]) scalar;
      1, map2 like scalar scalar;
      1, map3 like_escape scalar scalar scalar;
    ] in
    if depth <= 0 then leaf
    else frequency [
      3, leaf;
      3, map2 conj (gen_cond (depth - 1)) (gen_cond (depth - 1));
      3, map2 (fun a b -> call (Logical Or) [ a; b ]) (gen_cond (depth - 1)) (gen_cond (depth - 1));
      2, map2 (fun a b -> call (Logical Xor) [ a; b ]) (gen_cond (depth - 1)) (gen_cond (depth - 1));
      2, map neg (gen_cond (depth - 1));
      2, map2 (fun branches else_ -> Case { case = None; branches; else_ })
           (list_size (int_range 1 2)
              (map2 (fun when_ then_ -> { when_; then_ }) (gen_cond (depth - 1)) (gen_cond (depth - 1))))
           (option (gen_cond (depth - 1)));
      2, map3 (fun scrutinee branches else_ -> Case { case = Some scrutinee; branches; else_ })
           scalar
           (list_size (int_range 1 2)
              (map2 (fun when_ then_ -> { when_; then_ }) scalar (gen_cond (depth - 1))))
           (option (gen_cond (depth - 1)));
      1, map2 (fun a b ->
           Choices (make_located ~value:(Some "q") ~pos:(0, 0),
             [ make_located ~value:(Some "A") ~pos:(0, 0), Some a;
               make_located ~value:(Some "B") ~pos:(0, 0), Some b ]))
           (gen_cond (depth - 1)) (gen_cond (depth - 1));
    ]

  type element = Equal | Distinct | Null
  type in_side = Empty_list | One_tuple of element list

  type row = { cols : int option array; sub : int option list; in_side : in_side }

  let in_sides arity =
    let rec patterns n =
      if n = 0 then [ [] ]
      else List.concat_map (fun c -> List.map (List.cons c) (patterns (n - 1))) [ Equal; Distinct; Null ]
    in
    Empty_list :: List.map (fun p -> One_tuple p) (patterns arity)

  let rec max_in_arity e =
    let own =
      match e with
      | InChoice (_, _, Fun { kind = Membership; parameters = _ :: _; _ }) -> 1
      | InTupleList { value = { exprs; _ }; _ } -> List.length exprs
      | Value _ | Param _ | Inparam _ | Choices _ | InChoice _ | Fun _ | SelectExpr _
      | Column _ | OptionActions _ | Case _ | Of_values _ -> 0
    in
    List.fold_left (fun acc e -> max acc (max_in_arity e)) own (Sql.sub_exprs e)

  let kleene_and a b =
    match a, b with
    | Some false, _ | _, Some false -> Some false
    | Some true, Some true -> Some true
    | None, _ | _, None -> None
  let kleene_or a b =
    match a, b with
    | Some true, _ | _, Some true -> Some true
    | Some false, Some false -> Some false
    | None, _ | _, None -> None

  let in_side_holds row values =
    let compare_element element x =
      match x, element with
      | None, _ | Some _, Null -> None
      | Some _, Equal -> Some true
      | Some _, Distinct -> Some false
    in
    match row.in_side with
    | Empty_list -> Some false
    | One_tuple pattern ->
      List.fold_left2 (fun acc element x -> kleene_and acc (compare_element element x))
        (Some true) (List.take (List.length values) pattern) values

  let rec eval_scalar row = function
    | Column c -> Option.map_default (Array.get row.cols) None (column_of_ref c.collated)
    | Fun { kind = Arith _; parameters = [ a; b ]; _ } ->
      (match eval_scalar row a, eval_scalar row b with Some a, Some b -> Some (a + b) | _ -> None)
    | Fun { kind = Null_handling (Coalesce _ | If_null); parameters = [ a; b ]; _ } ->
      (match eval_scalar row a with None -> eval_scalar row b | v -> v)
    | Case { case; branches; else_ } ->
      let rec taken = function
        | [] -> Option.map_default (eval_scalar row) None else_
        | b :: rest -> if guard row case b = Some true then eval_scalar row b.then_ else taken rest
      in
      taken branches
    | Value _ -> None
    | Fun _ | Param _ | Inparam _ | Choices _ | InChoice _ | SelectExpr _
    | InTupleList _ | OptionActions _ | Of_values _ as e ->
      failwith ("oracle: not a scalar: " ^ Format.asprintf "%a" Sql.pp_expr e)

  and strict_binop op row a b =
    match eval_scalar row a, eval_scalar row b with Some a, Some b -> Some (op a b) | _ -> None

  and guard row case { when_; _ } =
    match case with
    | None -> eval_condition row when_
    | Some scrutinee -> strict_binop ( = ) row scrutinee when_

  and kleene_cmp op a b =
    let strict f = match a, b with Some a, Some b -> Some (f a b) | None, _ | _, None -> None in
    match op with
    | Comp_equal -> strict Int.equal
    | Comp_num_eq -> strict (fun a b -> not (Int.equal a b))
    | Comp_num_cmp -> strict (fun a b -> Int.compare a b < 0)
    | Comp_text_cmp -> strict (fun a b -> Int.compare a b > 0)
    | Not_distinct_op -> Some (Stdlib.Option.equal Int.equal a b)
    | Is_null | Is_not_null -> failwith "oracle: unary comparison used as a binary one"

  and eval_condition row e =
    match e with
    | Fun { kind = Comparison ((Comp_equal | Comp_num_eq | Comp_num_cmp | Comp_text_cmp
                               | Not_distinct_op) as op); parameters = [ a; b ]; _ } ->
      kleene_cmp op (eval_scalar row a) (eval_scalar row b)
    | Fun { kind = Like; parameters = [ a; b ]; _ } -> strict_binop ( = ) row a b
    | Fun { kind = Like_escape; parameters = [ e; esc ]; _ } ->
      Option.map_default (fun _ -> eval_condition row e) None (eval_scalar row esc)
    | Fun { kind = Comparison Is_null; parameters = [ a ]; _ } -> Some (eval_scalar row a = None)
    | Fun { kind = Comparison Is_not_null; parameters = [ a ]; _ } -> Some (eval_scalar row a <> None)
    | Fun { kind = Logical And; parameters = [ a; b ]; _ } -> kleene_and (eval_condition row a) (eval_condition row b)
    | Fun { kind = Logical Or; parameters = [ a; b ]; _ } -> kleene_or (eval_condition row a) (eval_condition row b)
    | Fun { kind = Logical Xor; parameters = [ a; b ]; _ } ->
      (match eval_condition row a, eval_condition row b with Some a, Some b -> Some (a <> b) | _ -> None)
    | Fun { kind = Negation; parameters = [ a ]; _ } -> Option.map not (eval_condition row a)
    | Fun { kind = Membership; parameters = [ x; SelectExpr _ ]; _ } ->
      let x = eval_scalar row x in
      List.fold_left (fun acc v -> kleene_or acc (kleene_cmp Comp_equal x v)) (Some false) row.sub
    | Fun { kind = Membership; parameters = x :: candidates; _ } ->
      let x = eval_scalar row x in
      List.fold_left (fun acc c -> kleene_or acc (kleene_cmp Comp_equal x (eval_scalar row c)))
        (Some false) candidates
    | Fun { kind = Range; parameters = [ x; lo; hi ]; _ } ->
      kleene_and (strict_binop ( >= ) row x lo) (strict_binop ( <= ) row x hi)
    | InChoice (_, kind, Fun { kind = Membership; parameters = x :: _; _ }) ->
      let member = in_side_holds row [ eval_scalar row x ] in
      begin match kind with `In -> member | `NotIn -> Option.map not member end
    | InTupleList { value = { exprs; kind_in_tuple_list; _ }; _ } ->
      let member = in_side_holds row (List.map (eval_scalar row) exprs) in
      begin match kind_in_tuple_list with `In -> member | `NotIn -> Option.map not member end
    | Fun { kind = Quantified_comparison { op; quantifier }; parameters = x :: _; _ } ->
      let x = eval_scalar row x in
      let fold combine unit = List.fold_left (fun acc v -> combine acc (kleene_cmp op x v)) unit row.sub in
      begin match quantifier with
      | `All -> fold kleene_and (Some true)
      | `Any -> fold kleene_or (Some false)
      end
    | Case { case; branches; else_ } ->
      let rec taken = function
        | [] -> Option.map_default (eval_condition row) None else_
        | b :: rest -> if guard row case b = Some true then eval_condition row b.then_ else taken rest
      in
      taken branches
    | Fun _ | Value _ | Param _ | Inparam _ | Choices _ | InChoice _ | SelectExpr _
    | OptionActions _ | Column _ | Of_values _ as e ->
      failwith ("oracle: not a condition: " ^ Format.asprintf "%a" Sql.pp_expr e)

  let rec disambiguate e =
    let cartesian parameters =
      List.fold_right (fun p acc ->
        List.concat_map (fun p -> List.map (List.cons p) acc) (disambiguate p)) parameters [ [] ]
    in
    match e with
    | Choices (_, alternatives) ->
      List.concat_map (fun (_, e) -> Option.map_default disambiguate [] e) alternatives
    | Fun ({ kind = Like_escape; parameters = [ Fun ({ kind = Like; parameters = [ a; b ]; _ } as inner); esc ]; _ } as f) ->
      List.concat_map (fun a ->
        List.concat_map (fun b ->
          List.concat_map (fun esc ->
            let node = Fun { f with parameters = [ Fun { inner with parameters = [ a; b ] }; esc ] } in
            [ node; neg node ]) (disambiguate esc)) (disambiguate b)) (disambiguate a)
    | Fun ({ kind = Membership | Range | Like | Like_escape; _ } as f) ->
      List.concat_map (fun parameters ->
        let node = Fun { f with parameters } in [ node; neg node ]) (cartesian f.parameters)
    | Fun ({ parameters; _ } as f) ->
      List.map (fun parameters -> Fun { f with parameters }) (cartesian parameters)
    | InChoice (id, kind, Fun ({ kind = Membership; parameters = x :: rest; _ } as f)) ->
      List.map (fun x -> InChoice (id, kind, Fun { f with parameters = x :: rest })) (disambiguate x)
    | InTupleList ({ value = ({ exprs; _ } as tuples); _ } as l) ->
      List.map (fun exprs -> InTupleList { l with value = { tuples with exprs } }) (cartesian exprs)
    | Case { case; branches; else_ } ->
      let opt = function None -> [ None ] | Some e -> List.map (fun e -> Some e) (disambiguate e) in
      let branches =
        List.fold_right (fun { when_; then_ } acc ->
          List.concat_map (fun when_ ->
            List.concat_map (fun then_ ->
              List.map (List.cons { when_; then_ }) acc) (disambiguate then_)) (disambiguate when_))
          branches [ [] ]
      in
      let elses = opt else_ in
      List.concat_map (fun case ->
        List.concat_map (fun branches ->
          List.map (fun else_ -> Case { case; branches; else_ }) elses) branches) (opt case)
    | e -> [ e ]

  let rows_for arity =
    let rec tuples values n =
      if n = 0 then [ [] ]
      else List.concat_map (fun v -> List.map (List.cons v) (tuples values (n - 1))) values
    in
    let cols = List.map Array.of_list (tuples [ None; Some 0; Some 1; Some 2 ] n_columns) in
    let subs = [ []; [ Some 0 ]; [ Some 1 ]; [ None ]; [ Some 0; None ]; [ Some 0; Some 1 ] ] in
    List.concat_map (fun cols ->
      List.concat_map (fun sub ->
        List.map (fun in_side -> { cols; sub; in_side }) (in_sides arity)) subs) cols

  let show = Format.asprintf "%a" Sql.pp_expr

  let shrink e =
    let opt = Option.map_default (fun e -> [ e ]) [] in
    List.to_seq @@
    match e with
    | Fun { kind = Logical _ | Negation; parameters; _ } -> parameters
    | Case { case = None; branches; else_ } ->
      List.concat_map (fun b -> [ b.when_; b.then_ ]) branches @ opt else_
    | Case { case = Some _; branches; else_ } ->
      List.map (fun b -> b.then_) branches @ opt else_
    | Choices (_, alternatives) -> List.filter_map snd alternatives
    | Fun _ | Value _ | Param _ | Inparam _ | InChoice _ | SelectExpr _ | Column _
    | InTupleList _ | OptionActions _ | Of_values _ -> []

  let any_condition = QCheck2.Gen.(set_shrink shrink (sized_size (int_range 0 3) gen_cond))

  let marked_columns promised =
    Narrowing.Qualified_attr.Set.elements promised
    |> List.map (fun (k : Narrowing.Qualified_attr.t) ->
      let hit i =
        let (tn, name) = columns.(i) in
        String.equal name k.name && (match k.sources with [ s ] -> Sql.equal_table_name tn s | _ -> false)
      in
      match List.find_opt hit all_columns with
      | Some i -> i
      | None ->
        QCheck2.Test.fail_reportf "narrowing promised unknown column [%s].%s"
          (String.concat ";" (List.map Sql.show_table_name k.sources)) k.name)

  let column_set l = let a = Array.make n_columns false in List.iter (fun i -> a.(i) <- true) l; a

  let marks_only_non_null e =
    let promised = marked_columns (narrowed e) in
    let promised_null row = List.find_opt (fun i -> row.cols.(i) = None) promised in
    let broken =
      if promised = [] then None
      else
        let rows = rows_for (max_in_arity e) in
        List.find_map (fun instance ->
          List.find_map (fun row ->
            match promised_null row with
            | Some i when eval_condition row instance = Some true -> Some (instance, row, i)
            | Some _ | None -> None)
            rows)
          (disambiguate e)
    in
    match broken with
    | None -> true
    | Some (instance, row, i) ->
      let show_in_side = function
        | Empty_list -> "empty"
        | One_tuple pattern ->
          String.concat ", " (List.map (function Equal -> "equal" | Distinct -> "distinct" | Null -> "NULL") pattern)
      in
      QCheck2.Test.fail_reportf
        "@[<v>promised %s is NULL in a kept row@,instance: %s@,row: %s@,subquery: %s@,list parameter: %s@]"
        (show_column i) (show instance)
        (String.concat ", " (List.mapi (fun i v ->
          sprintf "%s=%s" (show_column i) (Option.map_default string_of_int "NULL" v)) (Array.to_list row.cols)))
        (String.concat ", " (List.map (Option.map_default string_of_int "NULL") row.sub))
        (show_in_side row.in_side)

  let rec mentions acc = function
    | Column c -> Option.map_default (fun i -> i :: acc) acc (column_of_ref c.collated)
    | Fun { parameters; _ } -> List.fold_left mentions acc parameters
    | InChoice (_, _, e) -> mentions acc e
    | InTupleList { value = { exprs; _ }; _ } -> List.fold_left mentions acc exprs
    | Case _ | Value _ | Param _ | Inparam _ | Choices _ | SelectExpr _
    | OptionActions _ | Of_values _ -> acc

  let rec gen_strict_scalar depth =
    let open QCheck2.Gen in
    if depth <= 0 then gen_column
    else frequency [
      3, gen_column;
      2, map2 (fun a b -> call (Arith (Source_type.depends Any)) [ a; b ])
           (gen_strict_scalar (depth - 1)) (gen_strict_scalar (depth - 1));
    ]

  let any_strict_conjunction =
    let open QCheck2.Gen in
    let scalar = gen_strict_scalar 2 in
    let atom = frequency [
      3, map3 (fun op a b -> call (Comparison op) [ a; b ])
           (any_of [ Comp_equal; Comp_num_eq; Comp_num_cmp; Comp_text_cmp ]) scalar scalar;
      2, map is_not_null scalar;
      1, map2 like scalar scalar;
      1, map3 like_escape scalar scalar scalar;
      2, map (in_param `In) scalar;
      2, map (in_tuple_list `In) (list_size (int_range 1 2) scalar);
    ] in
    set_shrink shrink
      (sized_size (int_range 0 3)
         (fix (fun self depth ->
           if depth <= 0 then atom
           else frequency [ 2, atom; 3, map2 conj (self (depth - 1)) (self (depth - 1)) ])))

  let marks_every_non_null e =
    let expected = column_set (mentions [] e) in
    let got = column_set (marked_columns (narrowed e)) in
    if expected = got then true
    else
      QCheck2.Test.fail_reportf
        "@[<v>a conjunction of strict atoms must narrow every column it mentions@,expr: %s@,expected: %s@,got: %s@]"
        (show e) (show_column_set expected) (show_column_set got)

end

let test_narrowing_laws = [
  qcheck (QCheck2.Test.make ~count:2000 ~print:Narrowing_model.show
    ~name:"soundness: where Kleene reads the condition TRUE, every column narrowing marked is non-NULL"
    Narrowing_model.any_condition Narrowing_model.marks_only_non_null);
  qcheck (QCheck2.Test.make ~count:1000 ~print:Narrowing_model.show
    ~name:"completeness: on ANDed strict atoms, narrowing marks every column Kleene forces non-NULL"
    Narrowing_model.any_strict_conjunction Narrowing_model.marks_every_non_null);
]

let () =
  let suite = "qcheck laws" >::: [
    "test_type_laws" >::: test_type_laws;
    "test_meta_laws" >::: test_meta_laws;
    "test_narrowing_laws" >::: test_narrowing_laws;
  ] in
  let results = run_test_tt suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
