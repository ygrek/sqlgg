open Jsonkit.Primitives

type location =
  | Span of Sql.Pos.t
  | Properties
  | Statement
[@@deriving json, jsonschema] [@@compact_variants]

type diagnostic = {
  message : string;
  location : location;
}
[@@deriving json, jsonschema]

let diagnostic_of_exn outer =
  let rec loop fallback = function
    | Parser_utils.Error (inner, { pos; _ }) -> loop (Span pos) inner
    | (Sql_lexer.Error (_, pos) | Prelude.At (pos, _)) as exn when not (Sql.Pos.is_empty pos) ->
      { message = Parser_utils.message_of_exn exn; location = Span pos }
    | exn -> { message = Parser_utils.message_of_exn exn; location = fallback }
  in
  loop Statement outer

let with_context ?context f =
  let previous = Compile.snapshot () in
  let dynamic_select = !Syntax.Config.dynamic_select in
  Fun.protect
    ~finally:(fun () ->
      Compile.restore previous;
      Syntax.Config.dynamic_select := dynamic_select)
    (fun () ->
      (match context with
       | None -> Compile.reset ()
       | Some context -> Compile.restore context);
      f ())

module Extensions = struct
  open Sql

  let report pos extension_name =
    [ { message = "sqlgg extension not allowed: " ^ extension_name; location = Span pos } ]

  let opt f = Stdlib.Option.fold ~none:[] ~some:f

  let rec expr e =
    let own =
      match e with
      | Param (p, _) -> report p.id.pos "query parameter (@name / ?)"
      | Inparam (p, _) -> report p.id.pos "IN-list parameter"
      | Choices (id, _) -> report id.pos "@choice { ... }"
      | InChoice (id, _, _) -> report id.pos "IN @choice"
      | InTupleList { pos; _ } -> report pos "tuple IN @list"
      | OptionActions { pos = (p1, p2); _ } -> report (Pos.span p1 p2) "optional fragment { ... }?"
      | SelectExpr (sf, _) -> select_full sf
      | Value _ | Column _ | Of_values _ | Fun _ | Case _ -> []
    in
    own @ List.concat_map expr (sub_exprs e)

  and order items =
    List.concat_map
      (fun (e, dir) ->
        let direction =
          match dir with
          | Some (`Param id) -> report id.pos "dynamic ORDER BY direction"
          | Some `Fixed | None -> []
        in
        expr e @ direction)
      items

  and limit_params params =
    List.concat_map (fun (p : _ param) -> report p.id.pos "LIMIT/OFFSET parameter") params

  and limit l = opt (fun (params, _) -> limit_params params) l

  and source (kind, _) =
    match kind with
    | `Select sf -> select_full sf
    | `Table _ -> []
    | `Nested n -> nested n
    | `ValueRows rv ->
      let rows =
        match rv.row_constructor_list with
        | RowExprList rows -> List.concat_map (List.concat_map expr) rows
        | RowParam { id; _ } -> report id.pos "VALUES row parameter"
      in
      rows @ order rv.row_order @ limit rv.row_limit

  and nested (base, joins) =
    source base
    @ List.concat_map
        (fun { value = (src, _, cond); _ } ->
          let condition =
            match cond with
            | Schema.Join.On e -> expr e
            | Schema.Join.Default | Natural | Using _ -> []
          in
          source src @ condition)
        joins

  and select (s : select) =
    List.concat_map
      (fun (col : column) ->
        match col.value with
        | All | AllOf _ -> []
        | Expr (e, _) -> expr e.value)
      s.columns
    @ opt nested s.from
    @ opt expr s.where
    @ List.concat_map expr s.group
    @ opt expr s.having

  and select_complete (sc : select_complete) =
    let first, rest = sc.select in
    select first
    @ List.concat_map (fun (_, s) -> select s) rest
    @ order sc.order
    @ limit sc.limit

  and select_full { select_complete = sc; cte } =
    opt
      (fun { cte_items; _ } ->
        List.concat_map
          (fun item ->
            match item.stmt with
            | CteInline sc -> select_complete sc
            | CteSharedQuery reference -> report reference.pos "shared query reference (&name)")
          cte_items)
      cte
    @ select_complete sc

  let assignment_expr = function
    | RegularExpr e -> expr e
    | AssignDefault -> []
    | WithDefaultParam (e, (p1, p2)) ->
      report (Pos.span p1 p2) "default-with-param assignment" @ expr e

  let assignments = List.concat_map (fun (_, e) -> assignment_expr e)

  let alter_attr (attr : Alter_action_attr.t) =
    List.concat_map
      (fun (c : Alter_action_attr.constraint_ located) ->
        match c.value with
        | Alter_action_attr.Default { expr = default; _ } -> expr default.value
        | Syntax_constraint _ -> [])
      attr.extra

  let update assigns where order_by limit =
    assignments assigns @ opt expr where @ order order_by @ limit_params limit

  let rec stmt = function
    | Select sf -> select_full sf
    | Insert ia ->
      let action =
        match ia.action with
        | `Set assigns -> opt assignments assigns
        | `Values (_, rows) -> opt (List.concat_map (List.concat_map assignment_expr)) rows
        | `Param (_, id) -> report id.pos "INSERT ... VALUES @param"
        | `Select (_, sf) -> select_full sf
      in
      let on_conflict =
        match ia.on_conflict_clause with
        | None | Some { value = On_conflict { action = Do_nothing; _ }; _ } -> []
        | Some { value = On_duplicate { assignments = assigns }
                       | On_conflict { action = Do_update assigns; _ }; _ } ->
          assignments assigns
      in
      action @ on_conflict
    | Delete (_, where) -> opt expr where
    | DeleteMulti (_, tables, where) -> nested tables @ opt expr where
    | Update (_, assigns, where, order_by, limit) -> update assigns where order_by limit
    | UpdateMulti (tables, assigns, where, order_by, limit) ->
      nested tables @ update assigns where order_by limit
    | Set (assigns, rest) -> List.concat_map (fun (_, e) -> expr e) assigns @ opt stmt rest
    | Create (_, Schema { schema; _ }) -> List.concat_map alter_attr schema
    | Create (_, Select { value = sf; _ }) -> select_full sf
    | Alter { alter_actions; _ } ->
      List.concat_map
        (function
          | `Add (attr, _) | `Change (_, attr, _) -> alter_attr attr
          | `AlterColumnPG _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `Drop _
          | `AddIndex _ | `DropIndex _ | `AddPrimaryKey _ | `DropPrimaryKey
          | `AddConstraint _ | `DropConstraint _ | `Default_or_convert_to _
          | `TtlOptions _ | `RemoveTtl _ | `Cache _ | `NoCache _ -> [])
        alter_actions
    | CreateRoutine (_, _, params) ->
      List.concat_map (fun (_, _, default) -> opt expr default) params
    | Drop _ | Rename _ | CreateIndex _ | CreateType _ | DropType _
    | CreateExtension _ | DropExtension _ -> []
end

let catch_diagnostics f =
  match f () with
  | value -> Ok value
  | exception Stack_overflow -> Error ({ message = "stack overflow"; location = Statement }, [])
  | exception exn when Parser_utils.is_reportable_sql_error exn -> Error (diagnostic_of_exn exn, [])

let with_valid_properties (statement : Statements.t) f =
  let offset = fst statement.pos in
  let diagnostic (((start, _) as pos), message) =
    if start < offset then { message; location = Properties }
    else { message; location = Span (Sql.Pos.shift (-offset) pos) }
  in
  match statement.errors with
  | [] -> f ()
  | first :: rest -> Error (diagnostic first, List.map diagnostic rest)

let parse ?(allow_extensions = true) (statement : Statements.t) =
  with_valid_properties statement @@ fun () ->
  Parser_state.Stmt_metadata.load statement.metadata;
  Result.bind (catch_diagnostics (fun () -> Parser.parse_stmt statement.text)) @@ fun parsed ->
  if allow_extensions then Ok parsed
  else
    Result.bind (catch_diagnostics (fun () -> Extensions.stmt parsed.stmt)) @@ function
    | [] -> Ok parsed
    | first :: rest -> Error (first, rest)

let context_of_schema_sql sql =
  with_context @@ fun () ->
  List.fold_left
    (fun compiled (statement : Statements.t) ->
      Result.bind compiled @@ fun () ->
      with_valid_properties statement (fun () ->
        catch_diagnostics (fun () ->
          ignore (Compile.statement ~dynamic_select:Props.Off statement : Compile.outcome)))
      |> Result.map_error (fun (first, rest) -> statement, first, rest))
    (Ok ()) (Statements.split sql)
  |> Result.map Compile.snapshot

let analyze ?allow_extensions ~context (statement : Statements.t) =
  with_context ~context @@ fun () ->
  Syntax.Config.dynamic_select := false;
  Result.bind (parse ?allow_extensions statement) @@ fun parsed ->
  catch_diagnostics (fun () -> parsed, Syntax.eval_parsed statement.text parsed)
