open Ocamlbuild_plugin
open Command

module C = Myocamlbuild_config

;;

let () =
  let bracket res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = bracket r d input_line in

  bracket (open_out "version.ml") close_out (fun out ->
   let revision =
    try
     get_line (Unix.open_process_in "git describe --always") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "version.id") close_in with _ -> "<unknown>")
   in
   Printf.fprintf out "let id=\"%s\"\n" (String.escaped revision)
  )

;;

dispatch begin function
| After_rules ->

     C.extern ~cma:"extLib" "extlib";
     C.extern "deriving";
     C.extern "oUnit";

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A (C.lib "extlib")]);

| _ -> ()
end
