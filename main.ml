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

let statements s =
   let lexbuf = Lexing.from_string s in
   let rec loop l = 
    match (try Sql_lexer.ruleStatement Props.empty lexbuf with exn -> None) with
    | Some x -> loop (x::l)
    | None -> l
   in
    L.rev (loop [])

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

let parse_sql s =
(* Tables.reset (); *)
  s >> statements >> L.map parse_one 
  >> L.filter_valid 
(*   >> tee (L.iter show_one)  *)

let with_file filename f =
  match catch Std.input_file filename with
  | None -> Error.log "cannot open file : %s" filename
  | Some s -> f s

