open Sqlgg
open Printf

let ( let* ) = Option.bind

type t = {
  text : string;
  document : Document.t;
  stmt : Document.stmt option;
  offset : int;
}

let create ~text document offset =
  { text; document; offset;
    stmt =
      List.find_map (fun (item : Document.item) ->
        if Pos.covers item.stmt.pos offset then Some item.stmt else None) (Document.items document) }

type ident = {
  name : string;
  pos : Pos.t;
  qualifier : string option;
}

let find_ident t =
  let* (stmt : Document.stmt) = t.stmt in
  let base = fst stmt.pos in
  let rec loop acc = function
    | [] -> None
    | (lexeme : Recover_parser.lexeme) :: rest ->
      match Recover_parser.ident_name lexeme.token with
      | Some name when Pos.covers lexeme.pos (t.offset - base) ->
        Some { name; pos = Pos.shift base lexeme.pos;
          qualifier = Recover_parser.qualifier_before acc }
      | Some _ | None -> loop (lexeme :: acc) rest
  in
  loop [] (Recover_parser.tokens (String.sub t.text base (snd stmt.pos - base)))

let find_param t =
  Params.find_node (Option.fold ~none:[] ~some:Document.params t.stmt) t.offset
    ~f:(fun (node : Params.node) ->
      match node.kind with Var _ -> Some node | Branch _ -> None)

let find_shared_query t =
  let* (stmt : Document.stmt) = t.stmt in
  let base = fst stmt.pos in
  List.find_map (fun (lexeme : Recover_parser.lexeme) ->
    match lexeme.token with
    | SHARED_QUERY_REF reference
      when Pos.covers reference.pos (t.offset - base) ->
      Some (reference.value, Pos.shift base reference.pos)
    | _ -> None)
    (Recover_parser.tokens (String.sub t.text base (snd stmt.pos - base)))

let resolve_alias ~aliases symbols name =
  let* table = Sql.find_table_alias aliases name in
  Symbol.find symbols table.tn

type target =
  | Source of Symbol.t
  | Columns of (Symbol.t * Symbol.column) * (Symbol.t * Symbol.column) list

let stmt_scope t =
  Option.fold ~none:([], []) ~some:(fun stmt ->
    let scope = Document.scope_at stmt t.offset in
    scope.symbols, scope.aliases)
    t.stmt

let resolve t id =
  let (scope, aliases) = stmt_scope t in
  let index = Document.index t.document in
  let find_source symbols name =
    match Symbol.find symbols name with
    | Some _ as found -> found
    | None -> resolve_alias ~aliases symbols name
  in
  let column_scope =
    scope
    |> List.map (fun (symbol : Symbol.t) ->
      Option.value ~default:symbol (find_source index symbol.name))
    |> Symbol.unique
  in
  let find_columns symbols name =
    match List.filter_map (fun symbol -> Option.map (fun col -> symbol, col) (Symbol.find_column symbol name)) symbols with
    | [] -> None
    | head :: tail -> Some (Columns (head, tail))
  in
  let resolve_exact symbols =
    match id.qualifier with
    | None -> Option.map (fun symbol -> Source symbol) (find_source symbols id.name)
    | Some q ->
      let* symbol = find_source symbols q in
      match Symbol.find_column symbol id.name with
      | Some col -> Some (Columns ((symbol, col), []))
      | None -> Some (Source symbol)
  in
  match List.find_map resolve_exact [ index; scope ] with
  | Some _ as found -> found
  | None -> find_columns column_scope id.name

module Markdown = struct
  let with_buffer f = let b = Buffer.create 256 in f b; Buffer.contents b

  let section ?title b = function
    | [] -> ()
    | rows ->
      let width = List.fold_left (fun w (name, _) -> Int.max w (String.length name)) 0 rows in
      Option.iter (bprintf b "\n**%s**\n\n") title;
      bprintf b "```sql\n";
      List.iter (fun (name, typ) -> bprintf b "%-*s  %s\n" width name typ) rows;
      bprintf b "```\n"

  let column_rows = List.map (fun (attr : Sql.attr) -> attr.name, Sql.Type.show attr.domain)

  let shape = function
    | Params.Scalar typ -> Sql.Type.show typ
    | List [ typ ] -> Sql.Type.show typ ^ " list"
    | List types -> sprintf "(%s) list" (String.concat ", " (List.map Sql.Type.show types))
    | Compound -> ""

  let param_row depth node =
    String.make (depth * 2) ' ' ^ Params.label node, shape (Params.shape node)

  let param_rows nodes =
    let rec rows depth (node : Params.node) =
      let shape' = Params.shape node in
      let children = match shape' with Compound -> node.children | Scalar _ | List _ -> [] in
      Seq.cons
        (param_row depth node)
        (Seq.concat_map (rows (depth + 1)) (List.to_seq children))
    in
    Params.outline nodes |> List.to_seq |> Seq.concat_map (rows 0) |> List.of_seq

  let origin b (owner : Symbol.t) =
    match owner.kind, owner.loc with
    | Table, Some loc -> bprintf b "\nDeclared in `%s`\n" loc.file
    | Table, None -> ()
    | (Cte | Local), _ -> bprintf b "\nAvailable in this statement\n"

  let target = function
    | Source (symbol : Symbol.t) ->
      with_buffer @@ fun b ->
        begin match symbol.kind with
        | Table -> bprintf b "**table** `%s`\n\n" symbol.name
        | Cte -> bprintf b "**CTE** `%s`\n\n" symbol.name
        | Local -> bprintf b "`%s`\n\n" symbol.name
        end;
        section b (column_rows (Symbol.columns symbol));
        origin b symbol
    | Columns (head, tail) ->
      head :: tail |> List.map (fun ((owner : Symbol.t), (col : Symbol.column)) ->
        with_buffer @@ fun b ->
          section b [ owner.name ^ "." ^ col.attr.name, Sql.Type.show col.attr.domain ];
          origin b owner)
      |> String.concat "\n---\n"

  let param (node : Params.node) = with_buffer @@ fun b ->
    match Params.shape node with
    | Scalar _ | List _ -> section b (param_rows [ node ])
    | Compound ->
      section b [ param_row 0 node ];
      section ~title:"Branches" b (param_rows node.children)

  let branch (choice, (node : Params.node)) = with_buffer @@ fun b ->
    bprintf b "branch `%s` of `%s`\n" (Params.label node) (Params.name choice);
    match param_rows node.children with
    | [] -> bprintf b "\nTakes no parameters.\n"
    | rows -> section ~title:"Parameters in this branch" b rows

  let expr typ = with_buffer @@ fun b -> section b [ "expression", Sql.Type.show typ ]

  let kind (kind : Stmt.kind) =
    let table = Sql.show_table_name in
    let tables tables = String.concat ", " (List.map table tables) in
    match kind with
    | Stmt.Select `Zero_one -> "SELECT — at most one row"
    | Stmt.Select `One -> "SELECT — exactly one row"
    | Stmt.Select `Nat -> "SELECT — any number of rows"
    | Stmt.Insert (_, t) -> "INSERT into " ^ table t
    | Stmt.Update (Some t) -> "UPDATE " ^ table t
    | Stmt.Update None -> "UPDATE"
    | Stmt.Delete l -> "DELETE from " ^ tables l
    | Stmt.Create t -> "CREATE TABLE " ^ table t
    | Stmt.CreateIndex name -> "CREATE INDEX " ^ name
    | Stmt.CreateRoutine t -> "CREATE ROUTINE " ^ table t
    | Stmt.CreateType name -> "CREATE TYPE " ^ name
    | Stmt.DropType name -> "DROP TYPE " ^ name
    | Stmt.Alter l -> "ALTER " ^ tables l
    | Stmt.Drop t -> "DROP " ^ table t
    | Stmt.Other -> "statement"

  let stmt (stmt : Document.stmt) =
    match stmt.outcome, stmt.name with
    | (Skip | Error _), None -> None
    | (Skip | Error _), Some name -> Some (sprintf "`%s`\n" name)
    | Ok c, name ->
      Some (with_buffer @@ fun b ->
        Option.iter (bprintf b "`%s` — ") name;
        bprintf b "%s\n" (kind c.kind);
        section ~title:"Parameters" b (param_rows c.params);
        section ~title:"Result" b (column_rows c.schema))
end

let hover t =
  let param t =
    let* (node, pos) = find_param t in
    Some (Markdown.param node, pos)
  in
  let ident t =
    let* id = find_ident t in
    let* target = resolve t id in
    Some (Markdown.target target, id.pos)
  in
  let branch t =
    let* (branch, pos) =
      Params.find_node (Option.fold ~none:[] ~some:Document.params t.stmt) t.offset
        ~f:(fun (node : Params.node) ->
          match node.kind with Branch (choice, _) -> Some (choice, node) | Var _ -> None)
    in
    Some (Markdown.branch branch, pos)
  in
  let expr t =
    let* stmt = t.stmt in
    let* (typ, pos) = Pos.find_innermost t.offset (List.to_seq stmt.exprs) in
    Some (Markdown.expr typ, pos)
  in
  let shared_query t =
    let* (name, pos) = find_shared_query t in
    let* (stmt, _) = Document.find_reusable t.document name in
    let* text = Markdown.stmt stmt in
    Some (text, pos)
  in
  let stmt t =
    let* (stmt : Document.stmt) = t.stmt in
    let* text = Markdown.stmt stmt in
    Some (text, stmt.pos)
  in
  List.find_map (fun candidate -> candidate t)
    [ param; ident; branch; expr; shared_query; stmt ]

let definition t =
  let jump (owner : Symbol.t) (loc : Symbol.loc option) =
    match owner.kind, loc with
    | _, None -> []
    | Table, Some loc -> [ loc ]
    | (Cte | Local), Some loc -> if Pos.contains loc.pos t.offset then [] else [ loc ]
  in
  let jump_to (symbol : Symbol.t) = jump symbol symbol.loc in
  match find_shared_query t with
  | Some (name, _) ->
    Option.fold ~none:[] ~some:(fun (_, loc) -> [ loc ])
      (Document.find_reusable t.document name)
  | None ->
    begin match find_param t, find_ident t with
    | Some _, _ | None, None -> []
    | None, Some id ->
      match resolve t id with
      | None -> []
      | Some (Source symbol) ->
        begin match jump_to symbol with
        | _ :: _ as locs -> locs
        | [] ->
          let (scope, aliases) = stmt_scope t in
          resolve_alias ~aliases (Document.index t.document @ scope) symbol.name
          |> Option.fold ~none:[] ~some:jump_to
        end
      | Some (Columns (head, tail)) ->
        head :: tail |> List.concat_map (fun ((owner : Symbol.t), (col : Symbol.column)) ->
          let loc = match col.loc with None -> owner.loc | Some _ -> col.loc in
          jump owner loc)
    end

type token = { pos : Pos.t; typ : Params.token_type }

let semantic_tokens ~lines document =
  let line = Line_index.line lines in
  let single_line token = Int.equal (line (fst token.pos)) (line (snd token.pos)) in
  let rec disjoint acc stop = function
    | [] -> List.rev acc
    | token :: rest when fst token.pos < stop -> disjoint acc stop rest
    | token :: rest -> disjoint (token :: acc) (snd token.pos) rest
  in
  let token_type : Params.kind -> Params.token_type = function
    | Var (_, (Sql.Single _ | SingleIn _ | ChoiceIn _ | TupleList _ | SharedVarsGroup _)) -> Parameter
    | Var (_, (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _)) -> Enum
    | Branch _ -> Enum_member
  in
  List.to_seq (Document.items document)
  |> Seq.concat_map (fun (item : Document.item) -> Params.all_nodes (Document.params item.stmt))
  |> Seq.filter_map (fun (node : Params.node) ->
    match Params.token_pos node with
    | Some pos when not (Pos.is_empty pos) -> Some { pos; typ = token_type node.kind }
    | Some _ | None -> None)
  |> List.of_seq
  |> List.sort (fun a b -> Int.compare (fst a.pos) (fst b.pos))
  |> List.filter single_line
  |> disjoint [] 0
