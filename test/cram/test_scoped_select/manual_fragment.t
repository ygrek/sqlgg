A hand-written fragment (row-polymorphic object + let+/and+, no ppx) reused across two queries:

  $ cat scope.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "(col : _ t)" output.ml
  4
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c scope_frag.ml
  $ echo "scope fragment reused across two queries: OK"
  scope fragment reused across two queries: OK
