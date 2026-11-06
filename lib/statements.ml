open Stdlib

type metadata = (string * string) list

type t = {
  text : string;
  props : Props.t list;
  pos : Sql.Pos.t;
  metadata : (int * metadata) list;
  comments : Sql.Pos.t list;
  errors : (Sql.Pos.t * string) list;
}

type stmt_acc = {
  props : Props.t list;
  next_props : metadata;
  metadata : (int * metadata) list;
  comments : Sql.Pos.t list;
  errors : (Sql.Pos.t * string) list;
  start : int;
  end_ : int;
}

type state =
  | Prefix of Props.t list * (Sql.Pos.t * string) list
  | Statement of stmt_acc

type lexeme = [
  | `Text
  | `Literal
  | `Open_literal
  | `Blank
  | `Semicolon
  | `Comment
  | `Props of (string * string) list
  | `Bad_props
]

let lexemes text : (Sql.Pos.t * lexeme) Seq.t =
  let lexbuf = Lexing.from_string text in
  Seq.of_dispenser (fun () ->
    let start = lexbuf.Lexing.lex_curr_pos in
    match Sql_lexer.ruleStatement lexbuf with
    | `Eof -> None
    | #lexeme as lexeme -> Some ((start, lexbuf.Lexing.lex_curr_pos), lexeme))

let split text =
  let initial = Prefix ([], []) in
  let take state (start, end_) =
    match state with
    | Statement ({ next_props = []; _ } as stmt) ->
      Statement { stmt with end_ }
    | Statement stmt ->
      Statement {
        stmt with
        metadata = (start - stmt.start, stmt.next_props) :: stmt.metadata;
        next_props = [];
        end_;
      }
    | Prefix (props, errors) ->
      Statement { props; next_props = []; metadata = []; comments = []; errors; start; end_ }
  in
  let parse key value =
    let flag prop =
      match value with
      | "" | "true" -> Ok prop
      | _ -> Error (Printf.sprintf "%s is a flag, it does not take a value" key)
    in
    let enum of_string expected wrap =
      of_string (String.lowercase_ascii value) |> Option.map wrap
      |> Option.to_result ~none:(Printf.sprintf "unknown %s=%s (expected %s)" key value expected)
    in
    match key with
    | "name" -> Ok (Props.Name value)
    | "include" -> enum Props.include__of_string "reuse, execute or reuse_and_execute" (fun i -> Props.Include i)
    | "noparse" -> flag Props.Noparse
    | "dynamic_select" -> enum Props.dynamic_select_of_string "true, both or false" (fun d -> Props.Dynamic_select d)
    | "subst" -> Ok (Props.Subst value)
    | "id" -> Ok (Props.Id value)
    | "down" -> Ok (Props.Down value)
    | "irreversible" -> flag Props.Irreversible
    | "generated" -> flag Props.Auto
    | "manual" -> flag Props.Manual
    | _ -> Error (Printf.sprintf "unknown property %s" key)
  in
  let properties pos pairs (props, errors) =
    let (valid_props, new_errors) =
      List.partition_map (fun (k, v) ->
        match parse k v with
        | Ok prop -> Either.Left prop
        | Error msg -> Either.Right (pos, msg)) pairs
    in
    valid_props @ props, List.rev_append new_errors errors
  in
  let comment stmt pos = { stmt with comments = pos :: stmt.comments } in
  let block_of_acc stmt =
    let rebase (start, stop) = start - stmt.start, stop - stmt.start in
    { text = String.sub text stmt.start (stmt.end_ - stmt.start);
      props = stmt.props;
      pos = (stmt.start, stmt.end_);
      metadata = List.rev stmt.metadata;
      comments = List.rev_map rebase (List.filter (fun (_, stop) -> stop <= stmt.end_) stmt.comments);
      errors = List.rev stmt.errors }
  in
  let finish_all acc = function
    | Prefix _ -> acc
    | Statement stmt -> block_of_acc stmt :: acc
  in
  let step (acc, state) (pos, lexeme) =
    match lexeme, state with
    | `Blank, _ | `Comment, Prefix _ -> acc, state
    | (`Text | `Literal), _ -> acc, take state pos
    | `Open_literal, _ -> finish_all acc (take state pos), initial
    | `Comment, Statement stmt -> acc, Statement (comment stmt pos)
    | `Props pairs, Prefix (props, errors) ->
      let (props, errors) = properties pos pairs (props, errors) in
      acc, Prefix (props, errors)
    | `Props pairs, Statement stmt ->
      acc, Statement { (comment stmt pos) with next_props = pairs @ stmt.next_props }
    | `Bad_props, Prefix (props, errors) -> acc, Prefix (props, (pos, "malformed property list") :: errors)
    | `Bad_props, Statement stmt ->
      acc, Statement { (comment stmt pos) with errors = (pos, "malformed property list") :: stmt.errors }
    | `Semicolon, _ -> finish_all acc state, initial
  in
  let (acc, state) = Seq.fold_left step ([], initial) (lexemes text) in
  List.rev (finish_all acc state)

let strip_comments (stmt : t) =
  let text =
    let buffer = Buffer.create (String.length stmt.text) in
    let last =
      List.fold_left
        (fun start (comment_start, comment_stop) ->
          Buffer.add_substring buffer stmt.text start (comment_start - start);
          comment_stop)
        0 stmt.comments
    in
    Buffer.add_substring buffer stmt.text last (String.length stmt.text - last);
    Buffer.contents buffer
  in
  let removed_before offset =
    List.fold_left
      (fun n (start, stop) -> if stop <= offset then n + stop - start else n)
      0 stmt.comments
  in
  { stmt with
    text;
    metadata = List.map (fun (offset, props) -> offset - removed_before offset, props) stmt.metadata;
    comments = [] }

let unstripped_offset (stmt : t) offset =
  List.fold_left
    (fun offset (start, stop) -> if start <= offset then offset + stop - start else offset)
    offset stmt.comments

let glue_downs blocks =
  let has_id (block : t) = Option.is_some (Props.id block.props) in
  let rec loop = function
    | up :: down :: rest
      when has_id up && Option.is_none (Props.down up.props) && not (Props.has Irreversible up.props)
           && not (has_id down) ->
      { up with props = Props.Down down.text :: up.props } :: loop rest
    | block :: rest -> block :: loop rest
    | [] -> []
  in
  loop blocks

