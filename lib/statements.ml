open Stdlib

type metadata = (string * string) list

type t = {
  text : string;
  props : Props.t list;
  pos : Sql.pos;
  metadata : (int * metadata) list;
  comments : Sql.pos list;
  errors : (Sql.pos * string) list;
}

type inside = {
  props : Props.t list;
  next_props : metadata;
  metadata : (int * metadata) list;
  comments : Sql.pos list;
  errors : (Sql.pos * string) list;
  start : int;
  stop : int;
}

type state =
  | Prefix of Props.t list * (Sql.pos * string) list
  | Inside of inside

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

let lexemes text : (Sql.pos * lexeme) Seq.t =
  let lexbuf = Lexing.from_string text in
  Seq.of_dispenser (fun () ->
    let start = lexbuf.Lexing.lex_curr_pos in
    match Sql_lexer.ruleStatement lexbuf with
    | `Eof -> None
    | #lexeme as lexeme -> Some ((start, lexbuf.Lexing.lex_curr_pos), lexeme))

let split text =
  let initial = Prefix ([], []) in
  let take state (start, stop) =
    match state with
    | Inside ({ next_props = []; _ } as current) -> Inside { current with stop }
    | Inside current ->
      Inside { current with metadata = (start - current.start, current.next_props) :: current.metadata; next_props = []; stop }
    | Prefix (props, errors) ->
      Inside { props; next_props = []; metadata = []; comments = []; errors; start; stop }
  in
  let parse key value =
    let flag prop =
      match value with
      | "" | "true" -> Ok prop
      | _ -> Error (Printf.sprintf "%s is a flag, it does not take a value" key)
    in
    let enum of_string choices wrap =
      of_string (String.lowercase_ascii value) |> Option.map wrap
      |> Option.to_result ~none:(Printf.sprintf "unknown %s=%s (expected %s)" key value choices)
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
  let comment current pos = { current with comments = pos :: current.comments } in
  let finish current =
    let rebase (start, stop) = start - current.start, stop - current.start in
    { text = String.sub text current.start (current.stop - current.start);
      props = current.props;
      pos = (current.start, current.stop);
      metadata = List.rev current.metadata;
      comments = List.rev_map rebase (List.filter (fun (_, stop) -> stop <= current.stop) current.comments);
      errors = List.rev current.errors }
  in
  let stop acc = function Prefix _ -> acc | Inside current -> finish current :: acc in
  let step (acc, state) (pos, lexeme) =
    match lexeme, state with
    | `Blank, _ | `Comment, Prefix _ -> acc, state
    | (`Text | `Literal), _ -> acc, take state pos
    | `Open_literal, _ -> stop acc (take state pos), initial
    | `Comment, Inside current -> acc, Inside (comment current pos)
    | `Props pairs, Prefix (props, errors) ->
      let (props, errors) = properties pos pairs (props, errors) in
      acc, Prefix (props, errors)
    | `Props pairs, Inside current ->
      acc, Inside { (comment current pos) with next_props = pairs @ current.next_props }
    | `Bad_props, Prefix (props, errors) -> acc, Prefix (props, (pos, "malformed property list") :: errors)
    | `Bad_props, Inside current ->
      acc, Inside { (comment current pos) with errors = (pos, "malformed property list") :: current.errors }
    | `Semicolon, _ -> stop acc state, initial
  in
  let (acc, state) = Seq.fold_left step ([], initial) (lexemes text) in
  List.rev (stop acc state)

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

