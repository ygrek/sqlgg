(* 
  $Id$ 
*)

open Printf
open Operators
open ListMore
open ExtString

module L = List
module S = String

let statements s =
   let lexbuf = Lexing.from_string s in
   let rec loop l = 
    match (try Sql_lexer.ruleStatement Props.empty lexbuf with exn -> None) with
    | Some x -> loop (x::l)
    | None -> l
   in
    List.rev (loop [])

let parse_one (stmt,props) = 
  try
    print_endline stmt;
    Some ((Parser.parse_stmt stmt), props)
  with
  | exn ->
    begin
      print_endline (Printexc.to_string exn);
      None
    end

let show_one ((s,p),props) =
  RA.Scheme.print s;
  print_endline (Show.show<Stmt.Raw.params>(p))

let catch f x = try Some (f x) with _ -> None

let with_file filename f =
  match catch Std.input_file filename with
  | None -> Error.log "cannot open file : %s" filename
  | Some s -> f s

let tee f x = f x; x

let work filename = 
  with_file filename (fun s -> s 
  >> statements >> L.map parse_one >> L.filter_valid 
  >> tee (L.map show_one) >> Gen.process)

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

let show_help () =
  Error.log "SQL to C++ Code Generator Version %s (%s)" Version.version (Version.revision >> S.explode >> L.take 8 >> S.implode);
  Error.log "";
  Error.log " Usage: %s file_with_statements.sql" (Filename.basename Sys.executable_name);
  Error.log "";
  Error.log " Parse given file (treating content as SQL statements) and emit corresponding code to stdout"

let main () =
  match Array.to_list Sys.argv with
  | _::"-test"::_ -> Test.run ()
  | _::[file] -> work file
  | _ -> show_help ()

let _ = Printexc.print main ()
