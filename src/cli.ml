(** command-line *)

open Printf
open ExtLib

module Cxx = Gen.Make(Gen_cxx)
module Caml = Gen.Make(Gen_caml.Generator)
module Caml_io = Gen.Make(Gen_caml.Generator_io)
module Xml_gen = Gen.Make(Gen_xml)
module Java = Gen.Make(Gen_java)
module CSharp = Gen.Make(Gen_csharp)

(*
  common usecase:
     sqlgg [-gen none] ddl.sql -gen cxx dml.sql
*)
let generate = ref None
let name = ref "sqlgg"

let set_out s =
  generate :=
  match (String.lowercase s) with
  | "cxx" | "c++" | "cpp" -> Some Cxx.process
  | "caml" | "ocaml" | "ml" -> Some Caml.process
  | "caml_io" -> Some Caml_io.process
  | "xml" -> Some Xml_gen.process
  | "java" -> Some Java.process
  | "csharp" | "c#" | "cs" -> Some CSharp.process
  | "none" -> None
  | _ -> failwith (sprintf "Unknown output language: %s" s)

let set_params_mode s =
  Gen.params_mode :=
  match String.lowercase s with
  | "named" -> Some Gen.Named
  | "unnamed" -> Some Gen.Unnamed
  | "oracle" -> Some Gen.Oracle
  | "postgresql" -> Some Gen.PostgreSQL
  | "none" -> None
  | _ -> failwith (sprintf "Unknown params mode: %s" s)

let each_input =
  let run input =
    let l = match input with Some ch -> Main.get_statements ch | None -> [] in
    match !generate with
    | None -> []
    | Some _ -> l
  in
  function
  | "-" -> run (Some stdin)
  | filename -> Main.with_channel filename run

let generate l =
  match !generate with
  | None -> ()
  | Some f -> f !name l

let usage_msg =
  let s1 = sprintf "SQL Guided (code) Generator ver. %s\n" Sqlgg_config.version in
  let s2 = sprintf "Usage: %s <options> <file.sql> [<file2.sql> ...]\n" (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

let show_version () = print_endline Sqlgg_config.version

let main () =
  let l = ref [] in
  let work s = l := each_input s :: !l in
  let args = Arg.align
  [
    "-version", Arg.Unit show_version, " Show version";
    "-gen", Arg.String set_out, "cxx|caml|java|xml|csharp|none Set output language (default: none)";
    "-name", Arg.String (fun x -> name := x), "<identifier> Set output module name (default: sqlgg)";
    "-params", Arg.String set_params_mode, "named|unnamed|oracle|postgresql|none Output query parameters substitution (default: none)";
    "-debug", Arg.Int (fun x -> Sqlgg_config.debug_level := x), "<N> set debug level";
    "-no-header", Arg.Unit (fun () -> Sqlgg_config.gen_header := None),
      "do not put version header in generated output";
    "-no-header-timestamp", Arg.Unit (fun () -> Sqlgg_config.gen_header := Some `Without_timestamp),
      "do not put timestamp in version header in generated output";
    "-show-tables", Arg.Unit Tables.print_all, " Show all current tables";
    "-show-table", Arg.String Tables.print1, "<name> Show specified table";
    "-", Arg.Unit (fun () -> work "-"), " Read sql from stdin";
    "-test", Arg.Unit Test.run, " Run unit tests";
  ]
  in
  Arg.parse args work usage_msg;
  match !l with
  | [] -> if Array.length Sys.argv = 1 then Arg.usage args usage_msg; 0
  | l ->
    if !Error.errors then
      begin Error.log "Errors encountered, no code generated"; 1 end
    else
      begin generate @@ List.concat @@ List.rev l; 0 end

let () = exit @@ main ()
