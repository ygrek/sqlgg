Per-statement [dynamic_select=both]: dynamic module + classic companion.

  $ cat both.sql | sqlgg -gen caml -no-header -dialect=mysql - > both.ml
  $ diff both.ml both.compare.ml

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c both.ml
