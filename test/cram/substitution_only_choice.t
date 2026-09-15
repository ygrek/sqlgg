Substitution-only values in choice branches are not bound by set_params.
Exercise argument order, nesting, repeated names, statement clauses, and DML:

  $ /bin/sh ./sqlgg_test.sh substitution_only_choice.sql substitution_only_choice.compare.ml
  $ ocamlfind ocamlc -w +27 -warn-error +27 -package sqlgg.traits,sqlgg -c output.ml

The dynamic-select generator uses the same set_params pattern logic:

  $ /bin/sh ./sqlgg_test.sh substitution_only_dynamic_choice.sql substitution_only_dynamic_choice.compare.ml -dynamic-select
  $ ocamlfind ocamlc -w +27 -warn-error +27 -package sqlgg.traits,sqlgg -c output.ml
