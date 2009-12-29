(**
  Main
*)

open Printf
open Prelude
open ListMore
open ExtLib

module L = List
module S = String

let repeat f x k =
  let rec loop () =
    match f x with
    | Some z -> k z
    | None -> ()
  in
  loop ()

let parse_one_exn (sql,props) =
    if Config.debug1 () then  prerr_endline sql;
    let (s,p,k) = Parser.parse_stmt sql in
    (* fill VALUES *)
    let (sql,p) = match k with
    | Stmt.Insert (Some s,_) ->
      let module B = Buffer in
      let b = B.create 100 in
      B.add_string b sql;
      B.add_string b " (";
      let params = ref [] in
      s >> List.iter (fun attr ->
        if !params <> [] then B.add_string b ",";
        let name = "@" ^ attr.RA.name in
        let param = ((Some attr.RA.name,(B.length b,B.length b + String.length name)),attr.RA.domain) in
        B.add_string b name;
        params := param :: !params
      );
      B.add_string b ")";
      (B.contents b, p @ (List.rev !params))
    | _ -> (sql,p)
    in
    {Stmt.schema=s; params=p; kind=k; props=Props.set props "sql" sql}

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
  Enum.from next >> List.of_enum

let with_channel filename f =
  match catch open_in filename with
  | None -> Error.log "cannot open file : %s" filename; f None
  | Some ch -> Std.finally (fun () -> close_in_noerr ch) f (Some ch)

