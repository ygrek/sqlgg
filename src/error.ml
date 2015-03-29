(** Error output *)

open Printf

let errors = ref false

let logs s = errors := true; prerr_endline s
let log format = ksprintf logs format
(* let report format = ksprintf (fun str -> logs ("Project : warning _____: " ^ str)) format *)
(* let report = log *)

(* VS error format:
  path(lineno) : warning _____: string
*)
