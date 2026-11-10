
open OUnit
open Sqlgg_request

module Run = Sqlgg_request.Make (Model_driver)

type (_, _) native_ty =
  | Native_required : ('a, 'types) scalar -> ('a, 'types) native_ty
  | Native_optional : ('a, 'types) scalar -> ('a option, 'types) native_ty

type value = Value : ('a, Run.types) native_ty * 'a -> value

type scalar_gen = Scalar_gen : ('a, Run.types) scalar * 'a QCheck2.Gen.t -> scalar_gen

type _ tys =
  | Nil : value list tys
  | Cons : ('x, Run.types) native_ty * 'rest tys -> ('x -> 'rest) tys

type packed_tys = Tys : 'f tys -> packed_tys

let gen_json =
  let open QCheck2.Gen in
  let leaf =
    oneof
      [ return `Null; map (fun b -> `Bool b) bool; map (fun n -> `Int n) int_small;
        map (fun f -> `Float f) float; map (fun s -> `String s) (string_size (int_range 0 6));
        map (fun s -> `Intlit s) (string_size ~gen:numeral (int_range 1 25)) ]
  in
  sized_size (int_range 0 3)
    (fix (fun self depth ->
         if depth = 0 then leaf
         else
           oneof
             [ leaf;
               map (fun items -> `List items) (list_size (int_range 0 3) (self (depth - 1)));
               map
                 (fun fields -> `Assoc fields)
                 (list_size (int_range 0 3)
                    (pair (string_size ~gen:printable (int_range 0 4)) (self (depth - 1)))) ]))

let gen_json_path =
  let open QCheck2.Gen in
  let index =
    oneof
      [ map (fun n -> Sqlgg_json_path.Ast.Index n) (int_range 0 9); return Sqlgg_json_path.Ast.Wildcard;
        return Sqlgg_json_path.Ast.Last; map (fun n -> Sqlgg_json_path.Ast.LastOffset n) (int_range 0 9) ]
  in
  let leg =
    oneof
      [ map (fun name -> Sqlgg_json_path.Ast.Member name) (string_size ~gen:printable (int_range 0 4));
        map (fun index -> Sqlgg_json_path.Ast.ArrayAccess index) index;
        map2
          (fun start_idx end_idx -> Sqlgg_json_path.Ast.ArrayRange { start_idx; end_idx })
          index index;
        return Sqlgg_json_path.Ast.MemberWildcard; return Sqlgg_json_path.Ast.DoubleWildcard ]
  in
  map (fun legs -> { Sqlgg_json_path.Ast.scope = "$"; legs }) (list_size (int_range 0 4) leg)

let gen_enum =
  let open QCheck2.Gen in
  map
    (fun ctors ->
      let ctors = List.sort_uniq String.compare ctors in
      Scalar_gen
        ( Enum
            (module struct
              type t = string

              let inj value =
                if List.mem value ctors then value
                else failwith (Printf.sprintf "Invalid enum value: %s" value)

              let proj value = value
            end : Sqlgg_traits.Enum
              with type t = string),
          oneof_list ctors ))
    (list_size (int_range 1 4) (string_size ~gen:printable (int_range 0 5)))

let gen_scalar =
  let open QCheck2.Gen in
  let int64s = oneof [ int64; oneof_list [ Int64.min_int; Int64.max_int; 0L; -1L ] ] in
  oneof
    [ return (Scalar_gen (Bool, bool));
      return (Scalar_gen (Int, int64s));
      return (Scalar_gen (UInt64, map Unsigned.UInt64.of_int64 int64s));
      return (Scalar_gen (Float, float));
      return (Scalar_gen (Decimal, float));
      return (Scalar_gen (Text, string));
      return (Scalar_gen (Blob, string));
      return (Scalar_gen (Datetime, string));
      return (Scalar_gen (Json, gen_json));
      return (Scalar_gen (Json_path, gen_json_path));
      return (Scalar_gen (Any, string));
      return (Scalar_gen (One_or_all, oneof_list [ `One; `All ]));
      gen_enum ]

let gen_value =
  let open QCheck2.Gen in
  gen_scalar >>= fun (Scalar_gen (scalar, gen)) ->
  oneof
    [ map (fun value -> Value (Native_required scalar, value)) gen;
      map (fun value -> Value (Native_optional scalar, value)) (option gen) ]

let gen_values = QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 14) gen_value

let equal_scalar : type a. (a, Run.types) scalar -> a -> a -> bool =
 fun scalar left right ->
  match scalar with
  | Enum (module E) -> E.proj left = E.proj right
  | Bool | Int | UInt64 | Float | Decimal | Text | Blob | Datetime | Json | Json_path | Any | One_or_all ->
    compare left right = 0

let equal : type a. (a, Run.types) native_ty -> a -> a -> bool =
 fun ty left right ->
  match ty, left, right with
  | Native_required scalar, left, right -> equal_scalar scalar left right
  | Native_optional _, None, None -> true
  | Native_optional scalar, Some left, Some right -> equal_scalar scalar left right
  | Native_optional _, None, Some _ | Native_optional _, Some _, None -> false

let column_of_native : type a. (a, Run.types) native_ty -> (a, Run.types) ty = function
  | Native_required scalar -> Required (Native, scalar)
  | Native_optional scalar -> Optional (Native, scalar)

let request ?(params = fun _ -> ()) ~name ~kind ~sql result =
  let param_count =
    let count = ref 0 in
    params { bind = (fun _ _ _ -> incr count); null = (fun () -> incr count) };
    !count
  in
  { sql; name; kind; param_count; params; result }

let sink params =
  (request ~params ~name:"sink" ~kind:(Sqlgg_query.Insert "t") ~sql:(fun _ -> Sql "INSERT") (Build ())
   : (unit, Run.types, affected_rows) t)

let bind params =
  let store = Model_driver.connect () in
  ignore (Run.execute store (sink params));
  store.Model_driver.bound

let decode (columns : ('o, Run.types) columns) cells =
  let store = Model_driver.connect () in
  store.Model_driver.rows <- [ Array.of_list cells ];
  let source : ('o, Run.types, zero_or_one) t =
    request ~name:"source" ~kind:(Sqlgg_query.Select Sqlgg_query.Zero_one) ~sql:(fun _ -> Sql "SELECT") columns
  in
  let decoded = Run.find_opt store source in
  decoded, Model_driver.reads store

let params_of values (binder : Run.types binder) =
  List.iter
    (fun (Value (ty, value)) ->
      match ty, value with
      | Native_required scalar, value ->
        binder.bind Native scalar value
      | Native_optional _, None -> binder.null ()
      | Native_optional scalar, Some value ->
        binder.bind Native scalar value)
    values

let print_values values =
  "[" ^ String.concat "; " (List.map Model_driver.describe (bind (params_of values))) ^ "]"

let columns_of values =
  let rec tys_of = function
    | [] -> Tys Nil
    | Value (ty, _) :: rest ->
      let (Tys tail) = tys_of rest in
      Tys (Cons (ty, tail))
  in
  let rec builder : type f. f tys -> value list -> f =
   fun tys collected ->
    match tys with
    | Nil -> List.rev collected
    | Cons (ty, rest) -> fun value -> builder rest (Value (ty, value) :: collected)
  in
  let rec chain : type f. (f, Run.types) columns -> f tys -> (value list, Run.types) columns =
   fun columns tys ->
    match tys with
    | Nil -> columns
    | Cons (ty, rest) -> chain (Column (columns, column_of_native ty)) rest
  in
  let (Tys tys) = tys_of values in
  chain (Build (builder tys [])) tys

let qcheck (QCheck2.Test.Test cell) =
  let seed =
    match Sys.getenv_opt "SQLGG_PBT_SEED" with Some seed -> int_of_string seed | None -> 42
  in
  QCheck2.Test.get_name cell >:: fun () ->
  try QCheck2.Test.check_cell_exn ~rand:(Random.State.make [| seed |]) cell
  with QCheck2.Test.Test_fail (_, messages) -> assert_failure (String.concat "\n" messages)

let laws =
  [ qcheck
      (QCheck2.Test.make ~count:2000 ~name:"a bound value reads back as itself"
         ~print:(fun (Value (ty, value)) -> print_values [ Value (ty, value) ])
         gen_value
         (fun (Value (ty, value)) ->
           match
             decode (Column (Build Fun.id, column_of_native ty))
               (bind (params_of [ Value (ty, value) ]))
           with
           | Some read, [ 0 ] -> equal ty value read
           | _ -> false));

    qcheck
      (QCheck2.Test.make ~count:2000
         ~name:"an optional value is NULL exactly when it is None"
         ~print:(fun value -> print_values [ value ]) gen_value
         (fun (Value (ty, value) as packed) ->
           match bind (params_of [ packed ]), ty with
           | [ Null ], Native_optional _ -> value = None
           | [ Null ], Native_required _ -> false
           | [ _ ], Native_optional _ -> value <> None
           | [ _ ], Native_required _ -> true
           | _ -> false));

    qcheck
      (QCheck2.Test.make ~count:1000
         ~name:"a required column refuses NULL, an optional one reads None"
         ~print:(fun _ -> "scalar") gen_scalar
         (fun (Scalar_gen (scalar, _)) ->
           (match decode (Column (Build Fun.id, Optional (Native, scalar))) [ Null ] with
            | Some None, [ 0 ] -> true
            | _ -> false)
           && (match decode (Column (Build Fun.id, Required (Native, scalar))) [ Null ] with
               | _ -> false
               | exception Model_driver.Oops _ -> true)));

    qcheck
      (QCheck2.Test.make ~count:2000
         ~name:"a row of any shape reads its columns once each, left to right, and gives them back"
         ~print:print_values gen_values
         (fun values ->
           let cells = bind (params_of values) in
           match decode (columns_of values) cells with
           | Some read, reads -> reads = List.init (List.length values) Fun.id && compare (bind (params_of read)) cells = 0
           | None, _ -> false));

    (let module Scalar_value = struct
       type t = Pack : ('a, Run.types) scalar * 'a -> t
     end in
     qcheck
       (QCheck2.Test.make ~count:2000
          ~name:"a literal is printed by the driver's own type for that scalar"
          QCheck2.Gen.(
            gen_scalar >>= fun (Scalar_gen (scalar, gen)) ->
            map (fun value -> Scalar_value.Pack (scalar, value)) gen)
          (fun (Scalar_value.Pack (scalar, value)) ->
            let expected : type a. (a, Run.types) scalar -> a -> string =
             fun scalar value ->
              match scalar with
              | Bool -> string_of_bool value
              | Int -> Int64.to_string value
              | UInt64 -> Unsigned.UInt64.to_string value
              | Float -> Printf.sprintf "%h" value
              | Decimal -> Printf.sprintf "%h" value
              | Text -> Printf.sprintf "%S" value
              | Blob -> Printf.sprintf "X%S" value
              | Datetime -> Printf.sprintf "%S" value
              | Json -> "JSON"
              | Json_path -> "JSON_PATH"
              | Any -> Printf.sprintf "%S" value
              | One_or_all -> (match value with `One -> "'one'" | `All -> "'all'")
              | Enum (module E) -> "'" ^ E.proj value ^ "'"
            in
            let printing : (unit, Run.types, affected_rows) t =
              request ~name:"literal" ~kind:Sqlgg_query.Other
                ~sql:(fun literals ->
                  Sql (literals.literal Native scalar value))
                (Build ())
            in
            Run.sql printing = Some (expected scalar value))));

    qcheck
      (QCheck2.Test.make ~count:200
         ~name:"a request with nothing to run never reaches the driver"
         QCheck2.Gen.(pair string gen_values)
         (fun (name, values) ->
           let store = Model_driver.connect () in
           let nothing : (unit, Run.types, affected_rows) t =
             request ~params:(params_of values) ~name ~kind:Sqlgg_query.Other ~sql:(fun _ -> Nothing_to_run) (Build ())
           in
           let response = Run.execute store nothing in
           response.Model_driver.affected_rows = 0L
           && response.Model_driver.insert_id = None
           && store.Model_driver.query = None
           && store.Model_driver.bound = []
           && Run.sql nothing = None));

    qcheck
      (QCheck2.Test.make ~count:500
         ~name:"the statement reaches the driver unchanged"
         QCheck2.Gen.(triple string string
                        (oneof_list Sqlgg_query.[ Other; Insert "t"; Update None; Select Nat; Select One ]))
         (fun (sql, name, kind) ->
           let store = Model_driver.connect () in
           ignore
             (Run.execute store
                (request ~name ~kind ~sql:(fun _ -> Sql sql) (Build ()) : (unit, Run.types, affected_rows) t));
           match store.Model_driver.query with
           | Some query -> query.sql = sql && query.name = name && query.kind = kind
           | None -> false));

    qcheck
      (QCheck2.Test.make ~count:1000
         ~name:"executors agree: iter sees every row in order, find_opt and find see the first"
         ~print:(fun rows ->
           print_values
             (List.concat_map
                (fun (id, name) ->
                  [ Value (Native_required Int, id);
                    Value (Native_optional Text, name) ])
                rows))
         QCheck2.Gen.(list_size (int_range 0 6) (pair int64 (option string)))
         (fun rows ->
           let store = Model_driver.connect () in
           List.iter
             (fun (id, name) ->
               ignore
                 (Run.execute store
                    (sink
                       (params_of
                          [ Value (Native_required Int, id);
                            Value (Native_optional Text, name) ]))))
             rows;
           let columns = Column (Column (Build (fun id name -> id, name), Required (Native, Int)), Optional (Native, Text)) in
           let seen = ref [] in
           let select name kind = request ~name ~kind:(Sqlgg_query.Select kind) ~sql:(fun _ -> Sql "SELECT") columns in
           Run.iter store (select "all" Sqlgg_query.Nat) (fun row -> seen := row :: !seen);
           let first : (int64 * string option, Run.types, zero_or_one) t = select "first" Sqlgg_query.Zero_one in
           let exactly : (int64 * string option, Run.types, exactly_one) t = select "exactly" Sqlgg_query.One in
           let found = Run.find_opt store first in
           List.rev !seen = rows
           && found = (match rows with [] -> None | row :: _ -> Some row)
           && (match rows, Run.find store exactly with
               | row :: _, found -> found = row
               | [], _ -> false
               | exception Model_driver.Oops _ -> rows = [])))
  ]

let () = ignore (run_test_tt_main ("request" >::: laws))
