open Sqlgg
open Stdlib

module I = Sql_parser_incremental.MenhirInterpreter

type lexeme = { token : Sql_tokens.token; pos : Sql.pos }

type stop =
  | Complete of Sql.stmt
  | Fail
  | Pause of Sql.stmt I.env * lexeme

type trace = { seen : lexeme list; recovery : bool; tables : string list; sources : Sql.source list }

type run = { trace : trace; stop : stop }

let lexbuf_of sql =
  Parser_state.mode_normal ();
  Lexing.from_string sql

let make_lexeme lexbuf token = { token; pos = Sql_lexer.pos lexbuf }

let position offset = { Lexing.dummy_pos with pos_cnum = offset }

let ident_name : Sql_tokens.token -> string option = function
  | IDENT name | TYPE name -> Some name
  | _ -> None

let qualifier_before = function
  | { token = DOT; _ } :: { token; _ } :: _ -> ident_name token
  | _ -> None

let protect_mode f =
  let mode = !Parser_state.mode in
  Fun.protect ~finally:(fun () -> Parser_state.mode := mode) f

type settle_result =
  | Needs_input of trace * Sql.stmt I.env
  | Accept of trace * Sql.stmt
  | Reject

let run sql offset =
  let recovery_states = 4 in
  let recovery_skips = 3 in
  let lexbuf = lexbuf_of sql in
  Parser_state.with_lexbuf lexbuf @@ fun () ->
  let rec settle (acc : trace) = function
    | I.Shifting _ as checkpoint -> settle acc (I.resume checkpoint)
    | I.AboutToReduce _ as checkpoint ->
      let checkpoint = I.resume checkpoint in
      let top = function
        | I.InputNeeded env | I.Shifting (env, _, _) | I.AboutToReduce (env, _)
        | I.HandlingError env -> I.top env
        | I.Accepted _ | I.Rejected -> None
      in
      let acc =
        match top checkpoint with
        | Some (I.Element (state, value, _, _)) ->
          begin match I.incoming_symbol state with
          | I.N I.N_source -> { acc with sources = value :: acc.sources }
          | I.N I.N_table_ident -> { acc with tables = value :: acc.tables }
          | I.N _ | I.T _ -> acc
          end
        | None -> acc
      in
      settle acc checkpoint
    | I.InputNeeded env -> Needs_input (acc, env)
    | I.Accepted stmt -> Accept (acc, stmt)
    | I.HandlingError _ | I.Rejected -> Reject
  in
  let finish (acc : trace) stop =
    { trace =
        { acc with
          seen = List.rev acc.seen;
          tables = List.rev acc.tables;
          sources = List.rev acc.sources };
      stop }
  in
  let next_lexeme lexbuf =
    match Sql_lexer.parse_rule lexbuf with
    | exception (Out_of_memory as exn) -> raise exn
    | exception _ -> None
    | token -> Some (make_lexeme lexbuf token)
  in
  let rec loop (acc : trace) env =
    match next_lexeme lexbuf with
    | None -> finish acc Fail
    | Some lexeme when snd lexeme.pos > offset -> finish acc (Pause (env, lexeme))
    | Some lexeme -> feed { acc with seen = lexeme :: acc.seen } env lexeme
  and feed acc env lexeme =
    let (start, stop) = lexeme.pos in
    match settle acc (I.offer (I.input_needed env) (lexeme.token, position start, position stop)) with
    | exception (Out_of_memory as exn) -> raise exn
    | exception _ -> finish acc Fail
    | Reject -> recover acc env lexeme
    | Accept (acc, stmt) -> finish acc (Complete stmt)
    | Needs_input (acc, env) -> loop acc env
  and recover acc env lexeme =
    let acc = { acc with recovery = true } in
    let states =
      Seq.unfold (fun env -> Option.map (fun parent -> parent, parent) (I.pop env)) env
      |> Seq.cons env |> Seq.take recovery_states |> List.of_seq
    in
    let accept lexeme =
      protect_mode (fun () ->
        List.find_opt
          (fun env ->
            I.acceptable (I.input_needed env) lexeme.token (position (fst lexeme.pos)))
          states)
    in
    let rec skip n acc lexeme =
      if snd lexeme.pos > offset then
        finish acc (Pause (Option.value (accept lexeme) ~default:env, lexeme))
      else
        match accept lexeme with
        | Some env -> feed acc env lexeme
        | None when n = 0 -> finish acc Fail
        | None ->
          match next_lexeme lexbuf with
          | None -> finish acc Fail
          | Some lexeme -> skip (n - 1) { acc with seen = lexeme :: acc.seen } lexeme
    in
    skip recovery_skips acc lexeme
  in
  let empty = { seen = []; recovery = false; tables = []; sources = [] } in
  match settle empty (Sql_parser_incremental.Incremental.input lexbuf.lex_curr_p) with
  | Needs_input (acc, env) -> loop acc env
  | Accept (acc, stmt) -> finish acc (Complete stmt)
  | Reject -> finish empty Fail

let tokens sql =
  let lexbuf = lexbuf_of sql in
  List.of_seq @@ Seq.of_dispenser (fun () ->
    match Sql_lexer.parse_rule lexbuf with
    | exception _ | EOF -> None
    | token -> Some (make_lexeme lexbuf token))

let accepts run token =
  match run.stop with
  | Pause (env, { pos = (start, _); _ }) ->
    protect_mode (fun () ->
      I.acceptable (I.input_needed env) token (position start))
  | Complete _ | Fail -> false

type role =
  | Table_name
  | Column_name
  | Qualifier
  | Function_name
[@@deriving ord]

type slot =
  | Parameter of int
  | Column_of of string
  | Name of role list

let slot ?next run =
  let reductions_of_ident checkpoint pos follow =
    let rec loop ~after_shift acc tokens = function
      | I.AboutToReduce (_, prod) as checkpoint when after_shift ->
        loop ~after_shift (I.lhs prod :: acc) tokens (I.resume checkpoint)
      | I.AboutToReduce _ as checkpoint -> loop ~after_shift acc tokens (I.resume checkpoint)
      | I.Shifting _ as checkpoint -> loop ~after_shift:true acc tokens (I.resume checkpoint)
      | I.InputNeeded _ as checkpoint ->
        begin match tokens with
        | token :: tokens -> loop ~after_shift acc tokens (I.offer checkpoint (token, pos, pos))
        | [] -> acc
        end
      | I.HandlingError _ | I.Accepted _ | I.Rejected -> acc
    in
    try loop ~after_shift:false [] [ IDENT ""; follow ] checkpoint with
    | Out_of_memory as exn -> raise exn
    | _ -> []
  in
  let role = function
    | I.X (I.N I.N_table_ident) -> Some Table_name
    | I.X (I.N I.N_attr_name)
    | I.X (I.N I.N_insert_column) -> Some Column_name
    | I.X (I.N I.N_qual_ident) -> Some Qualifier
    | I.X (I.N I.N_func_ident) -> Some Function_name
    | I.X _ -> None
  in
  let roles env start =
    let follows =
      Option.to_list next @ ([ EOF; COMMA; RPAREN; DOT; LPAREN ] : Sql_tokens.token list)
    in
    protect_mode @@ fun () ->
    Name (List.sort_uniq compare_role
      (List.concat_map
        (fun follow ->
          List.filter_map role
            (reductions_of_ident (I.input_needed env) (position start) follow))
        follows))
  in
  match run.stop with
  | Complete _ -> Name []
  | Fail -> Name [ Column_name; Qualifier ]
  | Pause (_, { token = PARAM _; pos = (sigil, _) }) -> Parameter sigil
  | Pause (env, { pos = (start, _); _ }) ->
    match qualifier_before (List.rev run.trace.seen) with
    | Some q -> Column_of q
    | None -> roles env start
