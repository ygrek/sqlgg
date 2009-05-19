open Ocamlbuild_plugin
open Command

let chomp s =
  let is_nl ch = match ch with | '\n' | '\r' -> true | _ -> false in
  let rec cut n =
    if n = 0 then 0 else if is_nl s.[n-1] then cut (n-1) else n
  in
  let ls = String.length s in
  let n = cut ls in
  if n = ls then s else String.sub s 0 n

let ocamlfind lib =
  let cin = Unix.open_process_in (Printf.sprintf "ocamlfind -query %s" lib) in
  let s = chomp (input_line cin) in
(*  let s = Filename.quote s in*)
  ignore (Unix.close_process_in cin);
  s

;;

dispatch begin function
| After_rules ->
     let extlib_dir = ocamlfind "extlib" in
     let deriving_dir = Myocamlbuild_config.deriving_dir in
     let ounit_dir = ocamlfind "oUnit" in
     let xml_dir = ocamlfind "xml-light" in

     ocaml_lib ~extern:true ~dir:extlib_dir "extLib";
     ocaml_lib ~extern:true ~dir:deriving_dir "deriving";
     ocaml_lib ~extern:true ~dir:ounit_dir "oUnit";
     ocaml_lib ~extern:true ~dir:xml_dir "xml-light";

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A extlib_dir]);

| _ -> ()
end
