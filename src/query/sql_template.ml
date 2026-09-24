open ExtLib
open Sqlgg
open Jsonkit.Primitives

type cond = Dep_selected of Sql.param_id * int
[@@deriving eq, show, json, jsonschema]

let span_after ~cursor source ((start, stop) as pos : Sql.Pos.t) =
  if Sql.Pos.is_empty pos then
    Printf.ksprintf invalid_arg "Sql_template: stop (%d) must be > start (%d) in %S" stop start source;
  if start < cursor then
    Printf.ksprintf invalid_arg "Sql_template: start (%d) must be >= cursor (%d) in %S" start cursor source;
  pos

type bind = { param : Sql.Type.t Sql.param; original : string }
[@@deriving eq, show, json, jsonschema]

type placeholder =
  | Named [@as "named"]
  | Unnamed [@as "unnamed"]
  | Oracle [@as "oracle"]
  | PostgreSQL [@as "postgresql"]
[@@deriving of_string, json, jsonschema] [@@compact_variants]

type 'a fragment = [
  | `Text of string
  | `SubstIn of Sql.Type.t Sql.param * Sql.Meta.t
  | `Choice of Sql.param_id * 'a arm list
  | `Optional of Sql.param_id * 'a optional
  | `DynamicSelect of Sql.param_id * 'a arm list
  | `DynamicIn of Sql.param_id * Sql.in_or_not_in * 'a list
  | `SubstTuple of Sql.param_id * Sql.tuple_list_kind
  | `Cond of cond * 'a list
]

and 'a arm = {
  ctor : Sql.param_id;
  args : Sql.var list option;
  sql : 'a list;
}

and 'a optional = {
  vars : Sql.var list;
  some : 'a list;
  none : string;
}
[@@deriving eq, show, json, jsonschema]

type t = [ t fragment | `Bind of bind ]
[@@deriving eq, show, json, jsonschema]

let merge_texts fragments =
  let pending = Buffer.create 256 in
  let flush acc =
    if Buffer.length pending = 0 then acc
    else begin
      let text = Buffer.contents pending in
      Buffer.clear pending;
      `Text text :: acc
    end
  in
  List.rev
    (flush
       (List.fold_left
          (fun acc -> function
            | `Text text -> Buffer.add_string pending text; acc
            | fragment -> fragment :: flush acc)
          [] fragments))

let rec of_sql sql vars =
  let slice (first, last) = String.slice ~first ~last sql in
  let text first last = `Text (slice (first, last)) in
  let span ~cursor pos = span_after ~cursor sql pos in
  let rec fragments cursor vars =
    let stop, fragments =
      List.fold_left_map
        (fun cursor var ->
          let (start, stop), fragments = fragments_of_var cursor var in
          stop, text cursor start :: fragments)
        cursor vars
    in
    List.concat fragments, stop
  and body ~start ~stop vars =
    let fragments, last = fragments start vars in
    fragments @ [ text last stop ]
  and fragments_of_var cursor = function
    | Sql.Single (param, _) ->
        let span = span ~cursor param.id.pos in
        span, [ `Bind { param; original = slice span } ]
    | SingleIn (param, m) -> span ~cursor param.id.pos, [ `SubstIn (param, m) ]
    | ChoiceIn { param = name; kind; vars } ->
        let (start, _) as span = span ~cursor name.pos in
        span, [ `DynamicIn (name, kind, fst (fragments start vars)) ]
    | Choice (name, ctors) ->
        ( span ~cursor name.pos,
          [ `Choice
              ( name,
                arms
                  (fun pos -> function
                    | None -> []
                    | Some vars ->
                        let start, stop = span ~cursor pos in
                        `Text " (" :: body ~start ~stop vars @ [ `Text ") " ])
                  ctors ) ] )
    | DynamicSelectJoin { pid = name; pos = (j1, _) as pos; _ } ->
        let (_, stop) as join = span ~cursor pos in
        let rec lead k =
          if k > cursor && String.contains " \t\n\r" sql.[k - 1] then lead (k - 1)
          else k
        in
        (lead j1, stop), [ `Cond (Dep_selected (name, j1), [ `Text (" " ^ String.trim (slice join)) ]) ]
    | TupleList (id, (Where_in { value = (_, in_not_in); pos } as kind)) ->
        let id_start, id_stop = span ~cursor id.pos in
        let in_start, _ = span ~cursor pos in
        ( (in_start, id_stop),
          [ `DynamicIn (id, in_not_in, [ text in_start id_start; `SubstTuple (id, kind) ]) ] )
    | TupleList (id, (ValueRows { values_start_pos; _ } as kind)) ->
        let _, stop = span ~cursor id.pos in
        (values_start_pos, stop), [ `SubstTuple (id, kind) ]
    | TupleList (id, (Insertion _ as kind)) ->
        span ~cursor id.pos, [ `SubstTuple (id, kind) ]
    | OptionActionChoice (name, vars, (whole, inner), kind) ->
        let start, stop = span ~cursor inner in
        let none = match kind with BoolChoices -> " TRUE " | SetDefault -> " DEFAULT " in
        ( span ~cursor whole,
          [ `Optional (name, { vars; some = `Text " ( " :: body ~start ~stop vars @ [ `Text " ) " ]; none }) ] )
    | SharedVarsGroup (shared_vars, id) ->
        ( span ~cursor id.pos,
          `Text "(" :: of_sql (fst (Shared_queries.get id.value)) shared_vars @ [ `Text ")" ] )
    | DynamicSelect (name, ctors) ->
        ( span ~cursor name.pos,
          [ `DynamicSelect
              ( name,
                arms
                  (fun pos -> function
                    | None -> [ `Text "" ]
                    | Some vars ->
                        let start, stop = span ~cursor pos in
                        body ~start ~stop vars)
                  ctors ) ] )
  and arms arm ctors =
    List.map
      (function
        | Sql.Simple { ctor; body = args; _ } -> { ctor; sql = arm ctor.pos args; args }
        | Verbatim (n, v) -> { ctor = Sql.dummy_loc (Some n); args = Some []; sql = [ `Text v ] })
      ctors
  in
  merge_texts (body ~start:0 ~stop:(String.length sql) vars)

let fill ~spell template =
  let rec fragments index template = List.fold_left_map fragment index template
  and fragment index = function
    | (`Text _ | `SubstIn _ | `SubstTuple _) as fragment -> index, fragment
    | `Bind bind -> index + 1, `Text (spell index bind)
    | `Choice (pid, arms) ->
        index, `Choice (pid, List.map (fun arm -> { arm with sql = merge_texts (filled arm.sql) }) arms)
    | `Optional (pid, optional) -> index, `Optional (pid, { optional with some = filled optional.some })
    | `DynamicSelect (pid, arms) ->
        index, `DynamicSelect (pid, List.map (fun arm -> { arm with sql = filled arm.sql }) arms)
    | `DynamicIn (pid, kind, sub) -> index, `DynamicIn (pid, kind, filled sub)
    | `Cond (cond, body) ->
        let index, body = fragments index body in
        index, `Cond (cond, body)
  and filled template = snd (fragments 0 template)
  in
  merge_texts (filled template)
