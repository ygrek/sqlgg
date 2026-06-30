Dynamic selectors honor the [sqlgg] module= column meta in derived records:

  $ cat scope_mod.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "Product_id.get_column" output.ml
  1
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c product_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c scope_mod_frag.ml
  $ echo "dynamic + module= meta + derived record: OK"
  dynamic + module= meta + derived record: OK
