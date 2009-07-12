open Ocamlbuild_plugin
open Command

module C = Myocamlbuild_config

;;

dispatch begin function
| After_rules ->

     let extlib_dir = C.lib "extlib" in

     ocaml_lib ~extern:true ~dir:extlib_dir "extLib";
     ocaml_lib ~extern:true ~dir:(C.lib "deriving") "deriving";
     ocaml_lib ~extern:true ~dir:(C.lib "oUnit") "oUnit";

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A extlib_dir]);

| _ -> ()
end
