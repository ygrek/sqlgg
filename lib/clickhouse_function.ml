open Sql
open Sql.Type

type error =
  | Unknown_name
  | Invalid_arity

module Computed = struct
  (* Functions whose signature depends on the number of arguments. *)
  type t =
    | Array_map [@as "arraymap"]
    | Array_constructor [@as "array"]
    | Map_constructor [@as "map"]
    | Tuple_constructor [@as "tuple"]
    | Multi_if [@as "multiif"]
  [@@deriving enumerate, of_string, to_string]

  let signature computed argument_count =
    let repeat count templates = List.concat (List.init count (fun _ -> templates)) in
    match computed with
    | Array_map when argument_count >= 2 ->
      let element_templates = List.init (argument_count - 1) (fun index -> Var index) in
      let result_template = Var (argument_count - 1) in
      Ok (F (Array_of result_template,
             Fun_of (element_templates, result_template)
             :: List.map (fun template -> Array_of template) element_templates))
    | Array_constructor when Int.equal argument_count 0 -> Ok (F (Typ (strict (Array (strict Any))), []))
    | Array_constructor -> Ok (F (Array_of (Var 0), repeat argument_count [Var 0]))
    | Map_constructor when Int.equal argument_count 0 -> Ok (F (Typ (strict (Map (strict Any, strict Any))), []))
    | Map_constructor when Int.equal (argument_count mod 2) 0 ->
      Ok (F (Map_of (Var 0, Var 1), repeat (argument_count / 2) [Var 0; Var 1]))
    | Tuple_constructor ->
      let element_templates = List.init argument_count (fun index -> Var index) in
      Ok (F (Tuple_of element_templates, element_templates))
    | Multi_if when argument_count >= 3 && Int.equal (argument_count mod 2) 1 ->
      Ok (F (Var 0, repeat (argument_count / 2) [Typ (depends Bool); Var 0] @ [Var 0]))
    | Array_map | Map_constructor | Multi_if -> Error Invalid_arity
end

module Aggregate = struct
  type t =
    | Count [@as "count"]
    | Sum [@as "sum"]
    | Avg [@as "avg"]
    | Min [@as "min"]
    | Max [@as "max"]
    | Any_value [@as "any"]
    | Any_last [@as "anylast"]
    | Any_heavy [@as "anyheavy"]
    | Arg_min [@as "argmin"]
    | Arg_max [@as "argmax"]
    | Uniq [@as "uniq"]
    | Uniq_exact [@as "uniqexact"]
    | Uniq_combined [@as "uniqcombined"]
    | Uniq_combined64 [@as "uniqcombined64"]
    | Uniq_hll12 [@as "uniqhll12"]
    | Uniq_theta [@as "uniqtheta"]
    | Group_array [@as "grouparray"]
    | Group_uniq_array [@as "groupuniqarray"]
    | Top_k [@as "topk"]
    | Quantile [@as "quantile"]
    | Quantile_exact [@as "quantileexact"]
    | Quantile_timing [@as "quantiletiming"]
    | Quantiles [@as "quantiles"]
    | Median [@as "median"]
    | Stddev_pop [@as "stddevpop"]
    | Stddev_samp [@as "stddevsamp"]
    | Var_pop [@as "varpop"]
    | Var_samp [@as "varsamp"]
    | Covar_pop [@as "covarpop"]
    | Covar_samp [@as "covarsamp"]
    | Corr [@as "corr"]
    | Group_bit_and [@as "groupbitand"]
    | Group_bit_or [@as "groupbitor"]
    | Group_bit_xor [@as "groupbitxor"]
  [@@deriving enumerate, of_string, to_string]

  (* Listed outermost first: sumArrayIf is [If; Array_values] over Sum. *)
  type combinator =
    | If [@as "if"]
    | Array_values [@as "array"]
    | Distinct [@as "distinct"]
    | Or_default [@as "ordefault"]
    | State [@as "state"]
    | Merge [@as "merge"]
  [@@deriving enumerate, to_string]

  let rec parse name =
    match of_string name with
    | Some aggregate -> Some (aggregate, [])
    | None ->
      List.find_map (fun combinator ->
        let suffix = combinator_to_string combinator in
        match String.ends_with ~suffix name with
        | true ->
          let base = String.sub name 0 (String.length name - String.length suffix) in
          Option.map (fun (aggregate, inner) -> aggregate, combinator :: inner) (parse base)
        | false -> None)
        all_of_combinator

  let signature aggregate argument_count =
    match aggregate, argument_count with
    | Count, 0 -> Some (Typ (strict Int), [])
    | Count, 1 -> Some (Typ (strict Int), [Typ (depends Any)])
    | (Sum | Min | Max | Any_value | Any_last | Any_heavy | Quantile_exact
      | Group_bit_and | Group_bit_or | Group_bit_xor), 1 ->
      Some (Var 0, [Var 0])
    | (Avg | Quantile | Quantile_timing | Median | Stddev_pop | Stddev_samp | Var_pop | Var_samp), 1 ->
      Some (Typ (depends Float), [Var 0])
    | (Covar_pop | Covar_samp | Corr), 2 -> Some (Typ (depends Float), [Var 0; Var 1])
    | (Arg_min | Arg_max), 2 -> Some (Var 0, [Var 0; Var 1])
    | (Uniq | Uniq_exact | Uniq_combined | Uniq_combined64 | Uniq_hll12 | Uniq_theta), count
      when count >= 1 ->
      Some (Typ (strict UInt64), List.init count (fun index -> Var index))
    | (Group_array | Group_uniq_array | Top_k), 1 -> Some (Array_of (Var 0), [Var 0])
    | Quantiles, 1 -> Some (Typ (strict (Array (strict Float))), [Var 0])
    | (Count | Sum | Avg | Min | Max | Any_value | Any_last | Any_heavy | Arg_min | Arg_max
      | Uniq | Uniq_exact | Uniq_combined | Uniq_combined64 | Uniq_hll12 | Uniq_theta
      | Group_array | Group_uniq_array | Top_k | Quantile | Quantile_exact | Quantile_timing
      | Quantiles | Median | Stddev_pop | Stddev_samp | Var_pop | Var_samp | Covar_pop
      | Covar_samp | Corr | Group_bit_and | Group_bit_or | Group_bit_xor), _ ->
      None

  let rec signature_with aggregate combinators argument_count =
    match combinators with
    | [] -> signature aggregate argument_count
    | If :: inner ->
      Option.map (fun (ret, args) -> ret, args @ [Typ (depends Bool)])
        (signature_with aggregate inner (argument_count - 1))
    | Array_values :: inner ->
      Option.map (fun (ret, args) -> ret, List.map (fun arg -> Array_of arg) args)
        (signature_with aggregate inner argument_count)
    | (Distinct | Or_default) :: inner -> signature_with aggregate inner argument_count
    | State :: inner ->
      Option.map (fun (_, args) -> Typ (strict Blob), args)
        (signature_with aggregate inner argument_count)
    | Merge :: inner ->
      begin match argument_count with
      | 1 ->
        (* The state does not carry the argument types, so only a fixed result type survives. *)
        List.find_map (signature_with aggregate inner) [0; 1; 2]
        |> Option.map (fun (ret, _) ->
          let ret = match ret with
            | Typ _ -> ret
            | Var _ | Array_of _ | Map_of _ | Tuple_of _ | Fun_of _ -> Typ (depends Any)
          in
          ret, [Typ (depends Any)])
      | _ -> None
      end
end

let scalars =
  let table = Hashtbl.create 512 in
  let any = Typ (depends Any) in
  let text = Typ (depends Text) in
  let returns kind = Typ (depends kind) in
  let always kind = Typ (strict kind) in
  let element = Var 0 in
  let array = Array_of element in
  let predicate = Fun_of ([element], always Bool) in
  let each names signatures = List.concat_map (fun name -> List.map (fun f -> name, f) signatures) names in
  let conversions =
    List.concat_map (fun (name, kind) ->
      [name, F (returns kind, [Var 0]);
       name ^ "ornull", F (Typ (nullable kind), [Var 0]);
       name ^ "orzero", F (returns kind, [Var 0]);
       name ^ "ordefault", F (returns kind, [Var 0])])
      (List.map (fun name -> name, Int) ["toint8"; "toint16"; "toint32"; "toint64"]
       @ List.map (fun name -> name, UInt64) ["touint8"; "touint16"; "touint32"; "touint64"]
       @ List.map (fun name -> name, Float) ["tofloat32"; "tofloat64"]
       @ List.map (fun name -> name, Datetime) ["todate"; "todate32"; "todatetime"]
       @ ["touuid", Blob; "tobool", Bool])
  in
  let hash = Multi { ret = returns UInt64; fixed_args = [any]; repeating_pattern = [any] } in
  let json kind = Multi { ret = returns kind; fixed_args = [text]; repeating_pattern = [any] } in
  conversions
  @ each ["tostring"] [F (returns Text, [Var 0]); F (returns Text, [Var 0; text])]
  @ each ["todatetime"] [F (returns Datetime, [Var 0; text])]
  @ each ["todatetime64"] [F (returns Datetime, [Var 0; any]); F (returns Datetime, [Var 0; any; text])]
  @ each ["todecimal32"; "todecimal64"; "todecimal128"; "todecimal256"]
      [F (returns (Decimal { precision = None; scale = None }), [Var 0; any])]
  @ each ["today"; "yesterday"] [F (always Datetime, [])]
  @ each ["now"; "now64"] [F (always Datetime, []); F (always Datetime, [any])]
  @ each ["now64"] [F (always Datetime, [any; text])]
  @ each
      ["tostartofyear"; "tostartofquarter"; "tostartofmonth"; "tostartofweek"; "tostartofday";
       "tostartofhour"; "tostartofminute"; "tostartofsecond"; "tostartoffiveminutes";
       "tostartoffiveminute"; "tostartoftenminutes"; "tostartoffifteenminutes"; "tomonday";
       "totimezone"]
      [F (returns Datetime, [Var 0]); F (returns Datetime, [Var 0; text])]
  @ each ["tostartofinterval"] [F (returns Datetime, [Var 0; any]); F (returns Datetime, [Var 0; any; text])]
  @ each
      ["toyear"; "toquarter"; "tomonth"; "todayofmonth"; "todayofweek"; "todayofyear"; "tohour";
       "tominute"; "tosecond"; "toyyyymm"; "toyyyymmdd"; "toyyyymmddhhmmss"; "tounixtimestamp";
       "toisoweek"; "toisoyear"; "torelativedaynum"]
      [F (returns Int, [Var 0]); F (returns Int, [Var 0; text])]
  @ each ["datediff"; "date_diff"] [F (returns Int, [text; Var 0; Var 1]); F (returns Int, [text; Var 0; Var 1; text])]
  @ each ["dateadd"; "date_add"; "datesub"; "date_sub"] [F (returns Datetime, [text; any; Var 0])]
  @ each
      ["addseconds"; "addminutes"; "addhours"; "adddays"; "addweeks"; "addmonths"; "addquarters";
       "addyears"; "subtractseconds"; "subtractminutes"; "subtracthours"; "subtractdays";
       "subtractweeks"; "subtractmonths"; "subtractquarters"; "subtractyears"]
      [F (returns Datetime, [Var 0; any])]
  @ each ["formatdatetime"] [F (returns Text, [Var 0; text]); F (returns Text, [Var 0; text; text])]
  @ each ["parsedatetimebesteffort"; "parsedatetimebesteffortorzero"]
      [F (returns Datetime, [text]); F (returns Datetime, [text; text])]
  @ each ["parsedatetimebesteffortornull"] [F (Typ (nullable Datetime), [text]); F (Typ (nullable Datetime), [text; text])]
  @ each ["fromunixtimestamp"] [F (returns Datetime, [any])]
  @ each ["isnull"; "isnotnull"] [F (always Bool, [any])]
  @ each ["length"; "lengthutf8"] [F (returns UInt64, [Var 0])]
  @ each ["empty"; "notempty"] [F (returns Bool, [Var 0])]
  @ each
      ["lowerutf8"; "upperutf8"; "trimboth"; "trimleft"; "trimright"; "reverse"; "base64encode";
       "base64decode"]
      [F (returns Text, [text])]
  @ each ["replaceall"; "replaceone"; "replaceregexpall"; "replaceregexpone"] [F (returns Text, [text; text; text])]
  @ each ["position"; "positioncaseinsensitive"; "positionutf8"] [F (returns UInt64, [text; text])]
  @ each ["startswith"; "endswith"; "match"] [F (returns Bool, [text; text])]
  @ each ["extract"] [F (returns Text, [text; text])]
  @ each ["extractall"; "splitbychar"; "splitbystring"; "splitbyregexp"] [F (returns (Array (strict Text)), [text; text])]
  @ each ["arraystringconcat"] [F (returns Text, [array]); F (returns Text, [array; text])]
  @ each ["leftpad"; "rightpad"] [F (returns Text, [text; returns Int]); F (returns Text, [text; returns Int; text])]
  @ each ["cityhash64"; "siphash64"; "xxhash64"; "murmurhash3_64"; "farmhash64"; "xxh3"] [hash]
  @ each ["jsonextractstring"; "jsonextractraw"] [json Text]
  @ each ["jsonextractint"] [json Int]
  @ each ["jsonextractuint"; "jsonlength"] [json UInt64]
  @ each ["jsonextractfloat"] [json Float]
  @ each ["jsonextractbool"; "jsonhas"] [json Bool]
  @ each ["jsonextractkeys"] [json (Array (strict Text))]
  @ each ["arrayfilter"] [F (array, [predicate; array])]
  @ each ["arraysum"] [F (element, [array]); F (Var 1, [Fun_of ([element], Var 1); array])]
  @ each ["has"] [F (always Bool, [array; element])]
  @ each ["arrayjoin"; "arraymin"; "arraymax"; "arrayproduct"] [F (element, [array])]
  @ each ["arrayavg"] [F (returns Float, [array])]
  @ each ["hasany"; "hasall"] [F (always Bool, [array; array])]
  @ each ["indexof"; "countequal"] [F (always UInt64, [array; element])]
  @ each ["arrayconcat"] [Multi { ret = array; fixed_args = [array]; repeating_pattern = [array] }]
  @ each
      ["arraydistinct"; "arraysort"; "arrayreversesort"; "arrayreverse"; "arraycompact";
       "arrayshuffle"; "arraypopback"; "arraypopfront"]
      [F (array, [array])]
  @ each ["arraysort"; "arrayreversesort"] [F (array, [Fun_of ([element], Var 1); array])]
  @ each ["arrayexists"; "arrayall"] [F (always Bool, [predicate; array])]
  @ each ["arraycount"] [F (always UInt64, [array]); F (always UInt64, [predicate; array])]
  @ each ["arrayfirst"; "arraylast"] [F (element, [predicate; array])]
  @ each ["arrayfirstindex"; "arraylastindex"] [F (always UInt64, [predicate; array])]
  @ each ["arrayslice"] [F (array, [array; returns Int]); F (array, [array; returns Int; returns Int])]
  @ each ["arrayelement"] [F (element, [array; returns Int])]
  @ each ["arraypushback"; "arraypushfront"] [F (array, [array; element])]
  @ each ["arrayuniq"] [F (always UInt64, [array])]
  @ each ["arrayenumerate"] [F (always (Array (strict UInt64)), [array])]
  @ each ["range"] [F (always (Array (strict UInt64)), [any]); F (always (Array (strict UInt64)), [any; any])]
  @ each ["arrayzip"]
      [F (Array_of (Tuple_of [Var 0; Var 1]), [Array_of (Var 0); Array_of (Var 1)]);
       F (Array_of (Tuple_of [Var 0; Var 1; Var 2]), [Array_of (Var 0); Array_of (Var 1); Array_of (Var 2)])]
  @ each ["arrayflatten"; "flatten"] [F (array, [Array_of array])]
  @ each ["mapkeys"] [F (Array_of (Var 0), [Map_of (Var 0, Var 1)])]
  @ each ["mapvalues"] [F (Array_of (Var 1), [Map_of (Var 0, Var 1)])]
  @ each ["mapcontains"] [F (always Bool, [Map_of (Var 0, Var 1); Var 0])]
  @ each ["totypename"] [F (always Text, [any])]
  @ each ["generateuuidv4"] [F (always Blob, [])]
  @ each ["rand"; "rand64"] [F (always UInt64, [])]
  @ each ["intdiv"; "intdivorzero"] [F (Var 0, [Var 0; Var 0])]
  @ each ["row_number"; "rank"; "dense_rank"] [Agg (Typed (always UInt64, []))]
  (* Hashtbl.find_all returns the latest binding first, so add in reverse to keep the listed order. *)
  |> List.rev
  |> List.iter (fun (name, signature) -> Hashtbl.add table name signature);
  table

let names =
  List.map Computed.to_string Computed.all
  @ List.map Aggregate.to_string Aggregate.all
  @ Hashtbl.fold (fun name _ names -> name :: names) scalars []
  |> List.sort_uniq String.compare

let resolve name argument_count =
  match Computed.of_string name with
  | Some computed -> Computed.signature computed argument_count
  | None ->
    match Aggregate.parse name with
    | Some ((Count | Sum | Avg | Min | Max), []) -> Error Unknown_name
    | Some (aggregate, combinators) ->
      begin match Aggregate.signature_with aggregate combinators argument_count with
      | Some (ret, args) -> Ok (Agg (Typed (ret, args)))
      | None -> Error Invalid_arity
      end
    | None ->
      match Hashtbl.find_all scalars name with
      | [] -> Error Unknown_name
      | candidates ->
        match List.find_opt (fun signature -> Option.is_some (Sql.signature signature argument_count)) candidates with
        | Some signature -> Ok signature
        | None -> Error Invalid_arity
