(** command-line *)

open Printf
open Operators
open ExtLib

module Cxx = Gen.Make(Gen_cxx)
module Caml = Gen.Make(Gen_caml)
module Xml_gen = Gen.Make(Gen_xml)
module Java = Gen.Make(Gen_java)
module CSharp = Gen.Make(Gen_csharp)

let generate = ref Cxx.process
let name = ref "sqlgg"

let set_out s =
  generate :=
  match (String.lowercase s) with
  | "cxx" | "c++" | "cpp" -> Cxx.process
  | "caml" | "ocaml" | "ml" -> Caml.process
  | "xml" -> Xml_gen.process
  | "java" -> Java.process
  | "csharp" | "c#" | "cs" -> CSharp.process
  | "none" -> (fun _ e -> Enum.force e)
  | _ -> failwith (sprintf "Unknown output language: %s" s)

let set_params_mode s =
  Gen.params_mode :=
  match String.lowercase s with
  | "named" -> Some Gen.Named
  | "unnamed" -> Some Gen.Unnamed
  | "oracle" -> Some Gen.Oracle
  | _ -> None

let process l =
  let each =
    let run = function Some ch -> Main.get_statements ch | None -> Enum.empty () in
    function
    | "-" -> run (Some stdin)
    | filename -> Main.with_channel filename run
  in
  List.enum (List.map each l) >> Enum.concat >> !generate !name

let usage_msg =
  let s1 = sprintf "SQL Guided (code) Generator ver. %s\n" Config.version in
  let s2 = sprintf "Usage: %s <options> <file.sql>\n" (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

let show_version () = print_endline Config.version

let main () =
  let l = ref [] in
  let work s = l := s :: !l in
  let args =
  [
    "-version", Arg.Unit show_version, " Show version";
    "-gen", Arg.String set_out, "cxx|caml|java|xml|csharp|none Set output language (default: cxx)";
    "-name", Arg.String (fun x -> name := x), "<identifier> Set output module name (default: sqlgg)";
    "-params", Arg.String set_params_mode, "named|unnamed|oracle|none Output query parameters substitution (default: none)";
    "-debug", Arg.Int (fun x -> Config.debug_level := x), "<N> set debug level";
    "-", Arg.Unit (fun () -> work "-"), " Read sql from stdin";
    "-test", Arg.Unit Test.run, " Run unit tests";
  ]
  in
  Arg.parse (Arg.align args) work usage_msg;
  process !l

let _ = Printexc.print main ()

