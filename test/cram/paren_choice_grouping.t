Resolved SQL: choice branch body is wrapped in parens (manual parens redundant):
  $ cd test_build_paren_poc
  $ cat poc.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml - > output.ml
  $ cp ../print_ocaml_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe output.ml print_ocaml_impl.ml test_run.ml
  $ ./test_run.exe 2>&1 | grep -F '[SQL]'
  [SQL] SELECT 1 WHERE FALSE AND  ( FALSE OR TRUE ) 
  [SQL] SELECT 1 WHERE FALSE AND  ( (FALSE OR TRUE) ) 
  $ cd ..
