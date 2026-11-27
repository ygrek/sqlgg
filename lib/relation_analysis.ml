open Printf
open ExtLib
open Prelude
open Sql
open Relation_slot

type 'statement evaluation = {
  statement : 'statement;
  schema : Sql.schema_column list;
  vars : Sql.var list;
  kind : Stmt.kind;
  uses : Relation_slot.use list;
}

type 'statement t = {
  evaluation : 'statement evaluation;
  relation_slots : Relation_slot.t list;
  scope : Sql.table list -> Sql.table list;
}

(* something of the statement the relations may decide the type of, with the type one evaluation gives it *)
type typed =
  | Result_column of int * attr
  | Named_parameter of string * (Type.t * Meta.t)
  | Unnamed_parameter of Pos.t * (Type.t * Meta.t)
  | List_values of param_id * (Type.t * Meta.t) (** [IN @list] *)
  | Tuple_values of Pos.t * (Type.t * Meta.t) (** one position of [(a, b) IN @tuples] *)

(* a relation the statement compiles with *)
type compiled = {
  candidate : Relation_candidate.t;
  columns : Sql.schema;
  own_params : own_param list;
  kind : Stmt.kind; (** of the statement with the slot bound to the relation *)
  typed : typed list; (** follows the text of the statement: the same things in the same order for every relation *)
}

type coupled = {
  compiled : compiled;
  couplings : (column * kind_class) list; (** the same columns in the same order for every relation of a slot *)
}

(* a slot and the relations left for it *)
type slot = {
  name : Name.t;
  relations : coupled * coupled list;
  rejections : rejection list;
}

(* what the relations chosen decide about a type *)
type verdict =
  | Unknown (** no evaluation gives a definite type *)
  | Fixed of Type.t * Meta.t (** the same whatever is chosen *)
  | Chosen_by of Name.t list (** the slots whose relation decides it *)

(* non-empty lists: the first element and the others *)
let map1 f (x, xs) = f x, List.map f xs
let list1 (x, xs) = x :: xs

(* every value, or the first error *)
let all_ok results =
  List.fold_right (fun r rest -> Result.bind r (fun x -> Result.map (fun rest -> x :: rest) rest)) results (Ok [])

(* the slot of every RelationSlot among [vars], with its alias *)
let slot_aliases vars =
  List.filter_map (function
    | RelationSlot { id; alias } -> Option.map (fun name -> name, alias) (Name.of_param_id id)
    | Single _ | SingleIn _ | ChoiceIn _ | Choice _ | DynamicSelect _ | DynamicSelectJoin _ | TupleList _
    | OptionActionChoice _ | SharedVarsGroup _ -> None)
    (Sql.all_vars vars)

(* the uses of the columns of open slots in source order *)
let in_order (e : _ evaluation) = List.stable_sort (fun (a : use) (b : use) -> Pos.compare a.pos b.pos) e.uses

(* every SQL kind expected of a column of an open slot, in the order of [uses] *)
let expectations uses = List.concat_map (fun (u : use) -> List.rev_map (fun kind -> u.column, kind) u.expected) uses

(* the columns of a candidate and the parameters it owns *)
let prepare ~evaluate (candidate : Relation_candidate.t) =
  match candidate.origin with
  | Table (_, columns) -> Ok (columns, [])
  | Query entry ->
    match Props.substs entry.props, Props.dynamic_select entry.props with
    | _ :: _, _ -> Error "uses subst, its SQL is not known statically"
    | [], Some (Props.Only | Both) -> Error "uses dynamic_select, its columns are not fixed"
    | [], (Some Off | None) ->
      match evaluate ~bound:None entry.select with
      | Error exn | exception (At (_, Unsupported _) as exn) ->
        Error ("does not compile on its own: " ^ Parser_utils.message_of_exn exn)
      | Ok { schema; vars; _ } ->
        match slot_aliases vars with
        | _ :: _ -> Error "contains a relation slot itself"
        | [] ->
          let own_param var =
            let named_builtin =
              match var with
              | Single (({ id = { value = Some name; _ }; typ } as param), meta) ->
                begin match typ.t with
                | Int | UInt64 | Text | StringLiteral _ | Json_path | One_or_all | Blob | Float | FloatingLiteral _
                | Decimal _ | Bool | Datetime | Json | Any ->
                  if List.exists (fun key -> Option.is_some (Meta.find_opt meta key)) ["module"; "set_param"] then None
                  else Some ({ name; param; meta } : own_param)
                | Union _ -> None
                end
              | Single ({ id = { value = None; _ }; _ }, _)
              | SingleIn _ | ChoiceIn _ | Choice _ | DynamicSelect _ | DynamicSelectJoin _ | TupleList _
              | OptionActionChoice _ | SharedVarsGroup _ | RelationSlot _ -> None
            in
            Stdlib.Option.to_result named_builtin
              ~none:"has parameters other than plain named @params of built-in types, not supported for relation slots yet"
          in
          Result.bind
            (all_ok (List.map (function Attr a -> Ok a | Dynamic _ -> Error "has dynamic columns") schema))
            (fun attrs -> Result.map (fun own_params -> attrs, own_params) (all_ok (List.map own_param vars)))

(* result columns, then named parameters in order of first use, then what cannot be an index of a slot type *)
let typed_of (e : _ evaluation) =
  let (named, others) =
    Sql.all_vars e.vars
    |> List.concat_map (function
      | Single ({ id = { value = Some name; _ }; typ }, meta) -> [ Either.Left (name, (typ, meta)) ]
      | Single ({ id = { value = None; pos }; typ }, meta) -> [ Either.Right (Unnamed_parameter (pos, (typ, meta))) ]
      | SingleIn ({ id; typ }, meta) -> [ Either.Right (List_values (id, (typ, meta))) ]
      | TupleList (id, Where_in { value = (types, _); _ }) ->
        List.map (fun typed -> Either.Right (Tuple_values (id.pos, typed))) types
      | TupleList (_, (Insertion _ | ValueRows _)) | ChoiceIn _ | Choice _ | DynamicSelect _ | DynamicSelectJoin _
      | OptionActionChoice _ | SharedVarsGroup _ | RelationSlot _ -> [])
    |> List.partition_map identity
  in
  List.filter_map identity
    (List.mapi (fun i c -> match c with Attr a -> Some (Result_column (i, a)) | Dynamic _ -> None) e.schema)
  @ List.map (fun (name, typed) -> Named_parameter (name, typed)) (unique_by ~key:fst named)
  @ others

(* the statement with [slot] bound to every relation, the other slots open: the relation compiled
   with the expectations it puts on the columns of the other slots, or why it is rejected;
   [uses] and [baseline]: the columns of its slots the statement uses and what it expects of them with every slot open *)
let bind_slot ~evaluate select ~uses ~baseline prepared (slot : Name.t located) =
  List.map (fun ((candidate : Relation_candidate.t), relation) ->
    Result.bind relation (fun (columns, own_params) ->
      (* a relation without a column the statement uses is rejected without compiling the statement *)
      match
        List.find_opt (fun (u : use) ->
          Name.equal u.column.slot slot.value && not (List.exists (Schema.by_name u.column.column) columns)) uses
      with
      | Some u -> Error ("missing attribute : " ^ u.column.column)
      | None ->
        Result.map (fun (bound : _ evaluation) ->
          { candidate; columns; own_params; kind = bound.kind; typed = typed_of bound },
          (* what the statement expects of a column whatever is chosen is not due to the relation *)
          List.fold_left List.remove (expectations (in_order bound)) baseline)
          (Result.map_error Parser_utils.message_of_exn (evaluate ~bound:(Some (slot.value, columns)) select)))
    |> Result.map_error (fun reason -> { rejected = candidate; reason }))
    prepared

(* every relation gives one kind class to each column compared between its slot and another one:
   the columns of other slots it expects a type of, then its own columns other slots expect a type of *)
let couple bound =
  let expected_columns outcomes =
    unique_by ~key:identity
      (List.concat_map (function Ok (_, expected) -> List.map fst expected | Error _ -> []) outcomes)
  in
  let all_expected = List.concat_map (fun (_, outcomes) -> expected_columns outcomes) (list1 bound) in
  map1 (fun ((slot : Name.t located), outcomes) ->
    let keys =
      unique_by ~key:identity
        (expected_columns outcomes @ List.filter (fun (c : column) -> Name.equal c.slot slot.value) all_expected)
    in
    slot,
    List.map (fun outcome ->
      Result.bind outcome (fun (compiled, expected) ->
        let kind (key : column) =
          if Name.equal key.slot slot.value then
            match
              Stdlib.Option.bind (List.find_opt (Schema.by_name key.column) compiled.columns)
                (fun (attr : attr) -> kind_class attr.domain)
            with
            | Some kind -> Ok (key, kind)
            | None -> Error (sprintf "column %s has no SQL kind to compare with other relation slots" key.column)
          else
            match list_same (List.filter_map (fun (c, kind) -> if equal_column c key then Some kind else None) expected) with
            | Some kind -> Ok (key, kind)
            | None ->
              Error (sprintf "gives no single SQL kind to %s.%s shared with relation slot @%s"
                (key.slot : Name.t :> string) key.column (key.slot : Name.t :> string))
        in
        match all_ok (List.map kind keys) with
        | Ok couplings -> Ok { compiled; couplings }
        | Error reason -> Error { rejected = compiled.candidate; reason }))
      outcomes)
    bound

(* the relations left for a slot, an error listing the first reasons when there are none *)
let choose ((slot : Name.t located), outcomes) =
  match
    List.partition_map (function Ok relation -> Either.Left relation | Error rejection -> Either.Right rejection)
      outcomes
  with
  | [], rejections ->
    failed ~at:slot.pos "no relation is compatible with relation slot @%s%s" (slot.value : Name.t :> string)
      (String.concat "" (List.map (fun r ->
        sprintf "\n  %s: %s" (Relation_candidate.source_name r.rejected) r.reason) (List.take 5 rejections)))
  | relation :: others, rejections -> { name = slot.value; relations = (relation, others); rejections }

(* two keys of one constraint seen from both sides, l.id = r.id gives r.id and l.id,
   carry the same evidence in every slot having both: keep one index for them *)
let rec merge_keys slots =
  let relations = List.concat_map (fun s -> list1 s.relations) (list1 slots) in
  (* every relation of a slot has the keys of the first one *)
  let keys =
    unique_by ~key:identity (List.concat_map (fun s -> List.map fst (fst s.relations).couplings) (list1 slots))
  in
  (* the kind classes a relation gives to both keys, when its slot has both *)
  let both (k1, k2) r =
    match List.assoc_opt k1 r.couplings, List.assoc_opt k2 r.couplings with
    | Some c1, Some c2 -> Some (c1, c2)
    | Some _, None | None, (Some _ | None) -> None
  in
  let mergeable pair =
    match List.filter_map (both pair) relations with
    | [] -> false
    | _ :: _ as kinds -> List.for_all (fun (c1, c2) -> equal_kind_class c1 c2) kinds
  in
  let pairs =
    List.concat_map (fun k1 ->
      List.filter_map (fun k2 -> if compare_column k1 k2 < 0 then Some (k1, k2) else None) keys) keys
  in
  match List.find_opt mergeable pairs with
  | None -> slots
  | Some (kept, dropped) ->
    merge_keys (map1 (fun s ->
      { s with
        relations =
          map1 (fun r ->
            { r with
              couplings =
                unique_by ~key:fst
                  (List.map (fun (k, kind) -> (if equal_column k dropped then kept else k), kind) r.couplings) })
            s.relations })
      slots)

(* for everything [base] types, whether the relations chosen decide its type; types that are not definite,
   seen with another slot open, do not count; what two slots type differently (l.a + r.b) depends on both
   even if each slot alone is uniform *)
let verdicts base slots =
  let differ = function
    | [] -> false
    | typed :: rest -> List.exists (fun other -> not ([%eq: Type.t * Meta.t] typed other)) rest
  in
  let definite (t, _) = not (Type.is_any t) in
  (* rows of the same length to columns *)
  let transpose rows = List.fold_right (List.map2 List.cons) (list1 rows) (List.map (const []) (fst rows)) in
  (* for everything typed, for every slot: the types its relations give *)
  let types =
    transpose (map1 (fun s ->
      transpose (map1 (fun r ->
        List.map (function
          | Result_column (_, (attr : attr)) -> attr.domain, attr.meta
          | Named_parameter (_, typed) | Unnamed_parameter (_, typed) | List_values (_, typed)
          | Tuple_values (_, typed) -> typed)
          r.compiled.typed)
        s.relations))
      slots)
  in
  List.map2 (fun typed by_slot ->
    let verdict =
      match List.filter definite (List.concat by_slot) with
      | [] -> Unknown
      | (t, m) :: _ as known ->
        if differ known || List.exists differ by_slot then
          Chosen_by
            (List.filter_map identity
              (List.map2 (fun s types ->
                if differ types || List.for_all definite types then Some s.name else None) (list1 slots) by_slot))
        else Fixed (t, m)
    in
    typed, verdict)
    base.typed types

(* the slot with the evidence of each relation: [dependent], the indices that are not SQL kinds with
   the slots having them, then its couplings *)
let relation_slot dependent s : Relation_slot.t =
  let own =
    List.filter_map (fun (index, owners) -> if List.exists (Name.equal s.name) owners then Some index else None)
      dependent
  in
  let instance r : instance =
    { candidate = r.compiled.candidate;
      own_params = r.compiled.own_params;
      evidence =
        List.filter_map (function
          | Result_column (i, attr) when List.mem (Column i) own -> Some (Column_type (i, attr))
          | Named_parameter (name, typed) when List.mem (Parameter name) own -> Some (Parameter_type (name, typed))
          | Result_column _ | Named_parameter _ | Unnamed_parameter _ | List_values _ | Tuple_values _ -> None)
          r.compiled.typed
        @ List.map (fun (key, kind) -> Coupling_kind (key, kind)) r.couplings }
  in
  { name = s.name; instances = map1 instance s.relations; rejections = s.rejections }

let run ~evaluate select ((first : Name.t located), rest) =
  let other_slots =
    unique_by ~key:(fun (o : Name.t located) -> o.value)
      (List.filter (fun (o : Name.t located) -> not (Name.equal o.value first.value)) rest)
  in
  (* what the statement tells of its slots before any relation is chosen; with one slot it may type-check
     only once the slot is bound, with another one what is left open would stop it the same way *)
  let (uses, baseline) =
    match evaluate ~bound:None select, other_slots with
    | Ok e, _ -> let uses = in_order e in uses, expectations uses
    | Error _, [] -> [], []
    | Error exn, _ :: _ ->
      failed ~at:(match exn with At (pos, _) -> pos | _ -> first.pos)
        "with several relation slots the statement has to type-check before any relation is chosen: %s"
        (Parser_utils.message_of_exn exn)
  in
  let candidates =
    match Relation_candidate.all () with
    | Ok candidates -> candidates
    | Error e -> failed ~at:first.pos "%s" (Relation_candidate.show_error e)
  in
  let prepared = List.map (fun candidate -> candidate, prepare ~evaluate candidate) candidates in
  let chosen =
    map1 choose
      (couple (map1 (fun slot -> slot, bind_slot ~evaluate select ~uses ~baseline prepared slot) (first, other_slots)))
  in
  let base = (fst (fst chosen).relations).compiled in
  (* the statement with the first relation left for the first slot, evaluated again:
     every relation keeps the types and the cardinality of its evaluation only *)
  let evaluation =
    match evaluate ~bound:(Some (first.value, base.columns)) select with
    | Ok evaluation -> evaluation
    | Error exn -> raise exn
  in
  let slots = merge_keys chosen in
  let verdicts = verdicts base slots in
  let dependent =
    List.filter_map (fun (typed, verdict) ->
      match verdict with
      | Unknown | Fixed _ -> None
      | Chosen_by owners ->
        match typed with
        | Result_column (i, _) -> Some (Column i, owners)
        | Named_parameter (name, _) -> Some (Parameter name, owners)
        | Unnamed_parameter (pos, _) -> unsupported ~at:pos "name this parameter, its type depends on a relation slot"
        | List_values ({ pos; _ }, _) | Tuple_values (pos, _) ->
          unsupported ~at:pos "the type of the values of this list depends on a relation slot, not supported yet")
      verdicts
  in
  (* the types that are the same whatever is chosen *)
  let fixed =
    List.filter_map (fun (typed, verdict) ->
      match verdict with
      | Unknown | Chosen_by _ -> None
      | Fixed (t, m) ->
        match typed with
        | Result_column (i, _) -> Some (Column i, (t, m))
        | Named_parameter (name, _) | List_values ({ value = Some name; _ }, _) -> Some (Parameter name, (t, m))
        | Unnamed_parameter _ | List_values ({ value = None; _ }, _) | Tuple_values _ -> None)
      verdicts
  in
  let all = list1 slots in
  let schema =
    List.mapi (fun i c ->
      match c, List.assoc_opt (Column i) fixed with
      | Attr a, Some (domain, meta) -> Attr { a with domain; meta }
      | Attr _, None | Dynamic _, (Some _ | None) -> c)
      evaluation.schema
  in
  let vars =
    map_params (fun ((p : Type.t param), meta) ->
      match Stdlib.Option.bind p.id.value (fun name -> List.assoc_opt (Parameter name) fixed) with
      | Some (typ, meta) -> { p with typ }, meta
      | None -> p, meta)
      evaluation.vars
  in
  (* the widest cardinality among the relations left *)
  let kind =
    let rank = function
      | Stmt.Select `One -> 0
      | Select `Zero_one -> 1
      | Select `Nat | Insert _ | Create _ | CreateIndex _ | Update _ | Delete _ | Alter _ | Drop _
      | CreateRoutine _ | CreateType _ | DropType _ | Other -> 2
    in
    List.fold_left (fun widest r ->
      if rank r.compiled.kind > rank widest then r.compiled.kind else widest)
      base.kind
      (List.concat_map (fun s -> list1 s.relations) all)
  in
  let common =
    List.map (fun s ->
      let (relation, others) = s.relations in
      s.name,
      List.filter (fun (attr : attr) ->
        List.for_all (fun r -> List.exists (Schema.by_name attr.name) r.compiled.columns) others)
        relation.compiled.columns)
      all
  in
  (* every use of a slot with its alias *)
  let aliases = slot_aliases evaluation.vars in
  { evaluation = { evaluation with schema; vars; kind };
    relation_slots = List.map (relation_slot dependent) all;
    scope =
      List.map (fun ((t : table_name), columns) ->
        t,
        Option.default columns
          (List.find_map (fun (name, (alias : table_name)) ->
            if String.equal alias.tn t.tn then List.assoc_opt name common else None) aliases)) }
