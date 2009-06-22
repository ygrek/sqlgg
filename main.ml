(**
  Main
*)

open Printf
open Operators
open ListMore
open ExtString
open Apply

module L = List
module S = String

let repeat f x k =
  let rec loop () =
    match f x with
    | Some z -> k z
    | None -> ()
  in
  loop ()

let parse_one (stmt,props) =
  try
(*     print_endline stmt; *)
    let (s,p,k) = Parser.parse_stmt stmt in
    if not (RA.Schema.is_unique s) then
    begin
      Error.log "Error: this SQL statement will produce rowset with duplicate column names:\n%s\n" stmt
    end;
    (* fill VALUES *)
    let (stmt,p) = match k with
    | Stmt.Insert (Some s,_) ->
      let module B = Buffer in
      let b = B.create 100 in
      B.add_string b stmt;
      B.add_string b " (";
      let params = ref [] in
      s >> List.iter (fun attr ->
        if !params <> [] then B.add_string b ",";
        let name = "@" ^ attr.RA.name in
        let param = ((Some attr.RA.name,(B.length b,B.length b + String.length name)),Some attr.RA.domain) in
        B.add_string b name;
        params := param :: !params
      );
      B.add_string b ")";
      (B.contents b, p @ (List.rev !params))
    | _ -> (stmt,p)
    in
    Some {Stmt.schema=s; params=p; kind=k; props=Props.set props "sql" stmt}
  with
  | exn ->
    begin
      Error.log "==> %s" stmt;
      None
    end

let get_statements ch =
  let lexbuf = Lexing.from_channel ch in
  let f () = try Sql_lexer.ruleStatement Props.empty lexbuf with exn -> None in
  let rec next () =
    match f () with
    | None -> raise Enum.No_more_elements
    | Some sql ->
      begin match parse_one sql with
      | None -> next ()
      | Some stmt -> stmt
      end
  in
  Enum.from next

let with_file filename f =
  match catch Std.input_file filename with
  | None -> Error.log "cannot open file : %s" filename
  | Some s -> f s

let with_channel filename f =
  match catch open_in filename with
  | None -> Error.log "cannot open file : %s" filename
  | Some ch -> Std.finally (fun () -> close_in_noerr ch) f ch

