(**
  Main
*)

open ExtLib
open Prelude

module L = List
module S = String

let is_alpha = function
| 'a'..'z' -> true
| 'A'..'Z' -> true
| _ -> false

let common_prefix = function
| [] -> 0
| x::_ as l ->
  let rec loop i =
    if String.length x <= i then i
    else
      if List.for_all (fun s -> i < String.length s && s.[i] = x.[i]) l then
        loop (i+1)
      else
        i
  in
  let i = loop 0 in
  (* do not allow empty names or starting not with alpha *)
  if List.exists (fun s -> i = String.length s || not (is_alpha s.[i])) l then 0 else i

let parse_one_exn (sql,props) =
    if Sqlgg_config.debug1 () then  prerr_endline sql;
    let (schema,params,kind) = Parser.parse_stmt sql in
    (* fill inferred sql for VALUES or SET *)
    let (sql,params) = match kind with
    | Stmt.Insert (Some (kind,schema), _) ->
      let (pre,each,post) = match kind with
      | Stmt.Values -> "(", (fun _ -> ""), ")"
      | Stmt.Assign -> "", (fun name -> name ^" = "), ""
      in
      let module B = Buffer in
      let b = B.create 100 in
      B.add_string b sql;
      B.add_string b " ";
      B.add_string b pre;
      let params' = ref [] in
      let first = common_prefix @@ List.map (fun attr -> attr.RA.name) schema in
      schema |> List.iter (fun attr ->
        if !params' <> [] then B.add_string b ",";
        let attr_ref_prefix = each attr.RA.name in
        let attr_name = String.slice ~first attr.RA.name in
        let attr_ref = "@" ^ attr_name in
        let pos_start = B.length b + String.length attr_ref_prefix in
        let pos_end = pos_start + String.length attr_ref in
        let param = ((Some attr_name,(pos_start,pos_end)),attr.RA.domain) in
        B.add_string b attr_ref_prefix;
        B.add_string b attr_ref;
        params' := param :: !params'
      );
      B.add_string b post;
      (B.contents b, params @ (List.rev !params'))
    | _ -> (sql,params)
    in
    {Stmt.schema=schema; params=params; kind=kind; props=Props.set props "sql" sql}

let parse_one x =
  try
    Some (parse_one_exn x)
  with
  | Parser_utils.Error (exn,(line,cnum,tok,tail)) ->
    begin
     let extra = match exn with
     | RA.Schema.Error (_,msg) -> msg
     | exn -> Printexc.to_string exn
     in
     let sql = fst x in
     Error.log "==> %s" sql;
     if cnum = String.length sql && tok = "" then
       Error.log "Error: %s" extra
     else
       Error.log "Position %u:%u Tokens: %s%s\nError: %s" line cnum tok (String.slice ~last:32 tail) extra;
     None
    end

let parse_one (sql,props as x) =
  match Props.get props "noparse" with
  | Some _ -> Some { Stmt.schema=[]; params=[]; kind=Stmt.Other; props=Props.set props "sql" sql }
  | None -> parse_one x

let drop_while p e =
  while Option.map p (Enum.peek e) = Some true do
    Enum.junk e
  done

type token = [`Comment of string | `Token of string | `Char of char |
              `Space of string | `Prop of string * string | `Semicolon ]

let get_statements ch =
  let lexbuf = Lexing.from_channel ch in
  let tokens = Enum.from (fun () ->
    if lexbuf.Lexing.lex_eof_reached then raise Enum.No_more_elements else
    match Sql_lexer.ruleStatement lexbuf with
    | `Eof -> raise Enum.No_more_elements
    | #token as x -> x)
  in
  let extract () =
    let b = Buffer.create 1024 in
    let props = ref Props.empty in
    let answer () = Buffer.contents b, !props in
    let rec loop smth =
      match Enum.get tokens with
      | None -> if smth then Some (answer ()) else None
      | Some x ->
        match x with
        | `Comment s -> ignore s; loop smth (* do not include comments (option?) *)
        | `Char c -> Buffer.add_char b c; loop true
        | `Space _ when smth = false -> loop smth (* drop leading whitespaces *)
        | `Token s | `Space s -> Buffer.add_string b s; loop true
        | `Prop (n,v) -> props := Props.set !props n v; loop smth
        | `Semicolon -> Some (answer ())
    in
    loop false
  in
  let extract () = try extract () with e -> Error.log "lexer failed (%s)" (Printexc.to_string e); None in
  let rec next () =
    match extract () with
    | None -> raise Enum.No_more_elements
    | Some sql ->
      begin match parse_one sql with
      | None -> next ()
      | Some stmt ->
          if not (RA.Schema.is_unique stmt.Stmt.schema) then
            Error.log "Error: this SQL statement will produce rowset with duplicate column names:\n%s\n" (fst sql);
          stmt
      end
  in
  Enum.from next |> List.of_enum

let with_channel filename f =
  match try Some (open_in filename) with _ -> None with
  | None -> Error.log "cannot open file : %s" filename; f None
  | Some ch -> Std.finally (fun () -> close_in_noerr ch) f (Some ch)
