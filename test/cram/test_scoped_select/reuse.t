One derived fragment (string + opaque User_id.t) reused across two different queries:

  $ cp ../print_ocaml_impl.ml .
  $ cat reuse.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c user_id.mli
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c user_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx,yojson -I . -c reuse_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o reuse.exe print_ocaml_impl.cmo user_id.cmo output.cmo reuse_run.cmo
  $ ./reuse.exe 2>&1 | grep -E 'Q[12] who|Q[34] pair'
  Q1 who: id=1 name=alice
  Q2 who: id=7 name=bob
  Q2 who: id=8 name=carol
  Q3 pair: id=3 name=dave
  Q4 pair: id=9 name=erin
