(* 
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
    Some ((Parser.parse_stmt stmt), Props.set props "sql" stmt)
  with
  | exn ->
    begin
      Error.log "==> %s" stmt;
      None
    end

let show_one ((s,p),props) =
  RA.Scheme.print s;
  print_endline (Stmt.params_to_string p)

let get_statements ch =
  let lexbuf = Lexing.from_channel ch in
  let f () = try Sql_lexer.ruleStatement Props.empty lexbuf with exn -> None in
  let rec next () =
    match f () with
    | None -> raise Enum.No_more_elements
    | Some s ->
      begin match parse_one s with
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

