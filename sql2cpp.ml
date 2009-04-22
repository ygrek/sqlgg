(* 
  $Id$ 
*)

open Printf
open Operators
open ListMore
open ExtString

module L = List
module S = String

module T_SQL_parser = 
  struct 
    type token = Sql_parser.token
    type result = unit
    let rule = Sql_lexer.parse_rule
    let input = Sql_parser.input
  end

module P = Parser_utils.Make (T_SQL_parser)

let work filename =
  match
    (try Some (Std.input_file filename) with exn -> None)
  with
  | None -> Error.log "cannot open file : %s" filename
  | Some s ->
     let lexbuf = Lexing.from_string s in
     let rec lines l = 
      match (try Sql_lexer.ruleStatement Props.empty lexbuf with exn -> None) with
      | Some x -> lines (x::l)
      | None -> l
     in
      let all = lines [] >> List.rev in
      (* parse in direct order *)
      let parse1 (stmt,props) = 
        try
          print_endline stmt;
          P.parse_buf_exn (Lexing.from_string stmt)
        with
        | exn ->
          begin
            print_endline (Printexc.to_string exn)
          end
      in
      List.iter parse1 all
(*
      let stmts =
       List.map 
        (fun (str,props) -> match P.parse_string str with 
             | Some ((k,n,placeholders) as stmt) -> 
                  (*Error.logs (Show.show<Stmt.Raw.parsed> stmt); *)
                  Some (k,n,placeholders,props,str)
             | None -> Error.log "Failed to parse : %s" str; None)
        all
       >> List.filter_valid
      in
      Gen.process (Stmt.resolve stmts)
*)

(*  match P.parse_file filename with
  | None -> print_endline "none"
  | Some stmts -> 
      Error.logs (Show.show<Stmt.t list> stmts);
      Gen.process stmts*)

let show_help () =
  Error.log "SQL to C++ Code Generator Version %s (%s)" Version.version (Version.revision >> S.explode >> L.take 8 >> S.implode);
  Error.log "";
  Error.log " Usage: %s file_with_statements.sql" (Filename.basename Sys.executable_name);
  Error.log "";
  Error.log " Parse given file (treating content as SQL statements) and emit corresponding code to stdout"

let main () =
  match Array.to_list Sys.argv with
  | _::[file] -> work file
  | _ -> show_help ()

let _ = Printexc.print main ()
