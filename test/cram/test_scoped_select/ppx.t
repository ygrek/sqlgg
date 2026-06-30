A record with [@@deriving sqlgg] is aligned by name to two generated queries:

  $ cat scope.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c scope_frag_ppx.ml
  $ echo "derived who_of_cols reused across two queries: OK"
  derived who_of_cols reused across two queries: OK
