Fragments reuse across queries, raw selectors do not.

  $ cat dyn_scoped.sql | sqlgg -no-header -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml

Positive control:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c dyn_scoped_frag.ml

Raw selector from q1 in q2 is rejected:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c raw_mix_bad.ml 2>errors0.log
  [2]
  $ grep -q "Dscope_q1.brand" errors0.log && grep -q "Dscope_q2" errors0.log && echo "rejected: selector belongs to another query"
  rejected: selector belongs to another query

A fragment cannot demand a column the query does not select:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c missing_field_bad.ml 2>errors.log
  [2]
  $ grep -q "price" errors.log && echo "rejected: q2 does not provide price"
  rejected: q2 does not provide price
