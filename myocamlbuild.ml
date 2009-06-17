open Ocamlbuild_plugin
open Command

module C = Myocamlbuild_config

;;

dispatch begin function
| After_rules ->

     ocaml_lib ~extern:true ~dir:C.extlib_dir "extLib";
     ocaml_lib ~extern:true ~dir:C.deriving_dir "deriving";
     ocaml_lib ~extern:true ~dir:C.ounit_dir "oUnit";

     flag ["ocaml"; "doc"; "use_extLib"] (S[A"-I"; A C.extlib_dir]);

| _ -> ()
end
