(*
  $Id$
*)

open Printf

let logs = prerr_endline
let log format = ksprintf logs format
let report format = ksprintf (fun str -> logs ("Project : warning _____: " ^ str)) format

(* path(lineno) : warning _____: string*)