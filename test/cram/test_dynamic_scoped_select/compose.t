Two derived fragments compose with let+/and+ into one dynamic query:

  $ cp ../print_ocaml_impl.ml .
  $ cat dyn_scoped.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx,yojson -I . -c compose_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o compose.exe print_ocaml_impl.cmo output.cmo compose_run.cmo
  $ ./compose.exe 2>&1 | grep -E '\[SQL\]|COMPOSED'
  [SQL] SELECT id, name, category FROM products WHERE id = 7
  COMPOSED id=7 name=widget category=tools
