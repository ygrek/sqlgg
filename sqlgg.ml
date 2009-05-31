(** command-line *)

open Printf
open Operators

module Cxx = Gen.Make(Gen_cxx)
module Caml = Gen.Make(Gen_caml)
module Xml_gen = Gen.Make(Gen_xml)
module Java = Gen.Make(Gen_java)

let generate = ref Cxx.process
let name = ref "sqlgg"

let set_out s =
  generate :=
  match s with
  | "cxx" -> Cxx.process
  | "caml" -> Caml.process
  | "xml" -> Xml_gen.process
  | "java" -> Java.process
  | _ -> failwith (sprintf "Unknown output language: %s" s)

let set_name s = name := s

let work =
  let run ch = ch >> Main.get_statements >> !generate !name in
  function
  | "-" -> run stdin
  | filename -> Main.with_channel filename run

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
    "-gen", Arg.String set_out, "cxx|caml|java|xml Set output language";
    "-name", Arg.String set_name, "<identifier> Set output module name";
    "-", Arg.Unit (fun () -> work "-"), " Read sql from stdin";
    "-test", Arg.Unit Test.run, " Run unit tests";
  ]
  in
  Arg.parse (Arg.align args) work usage_msg

let _ = Printexc.print main ()

