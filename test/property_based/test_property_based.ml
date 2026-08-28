(* Property-based laws for Type and Meta. *)

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

let () =
  let suite = "qcheck laws" >::: [
    "test_type_laws" >::: test_type_laws;
    "test_meta_laws" >::: test_meta_laws;
  ] in
  let results = run_test_tt suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0
