(** command-line *)

open ListMore
open ExtString
open Operators

let work filename = 
  Main.with_file filename Main.parse_sql

let show_help () =
  Error.log "SQL to C++ Code Generator Version %s (%s)" 
    Version.version 
    (Version.revision >> String.explode >> List.take 8 >> String.implode);
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

