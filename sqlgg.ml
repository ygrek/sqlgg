(** command-line *)

open Printf
open Operators

module Cxx = Gen.Make(Gen_cxx)
module Caml = Gen.Make(Gen_caml)

let generate = ref Cxx.process

let set_out s = 
  generate :=
  match s with
  | "cxx" -> Cxx.process
  | "caml" -> Caml.process
  | _ -> failwith (sprintf "Unknown output language: %s" s)

let work = 
  let f s = s >> Main.parse_sql >> !generate in
  function
  | "-" -> f (Std.input_all stdin)
  | filename -> Main.with_file filename f

let usage_msg =
  let s1 = sprintf "SQL Guided (code) Generator ver. %s\n" Config.version in
  let s2 = sprintf "Usage: %s <options> <file.sql>\n" (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

let show_version () = print_endline Config.version

let main () =
  let args = 
  [
    "-version", Arg.Unit show_version, " Show version";
    "-out", Arg.String set_out, "cxx|caml Set output language";
    "-test", Arg.Unit Test.run, " Run unit tests";
  ]
  in
  Arg.parse (Arg.align args) work usage_msg

let _ = Printexc.print main ()

