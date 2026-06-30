Fragments are pinned to their query.

  $ cat isolation.sql | sqlgg -no-header -gen caml -dialect mysql - > output.ml
  $ grep -c "module Cols = struct" output.ml
  2
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c same_query_compose_ok.ml

q1 fragment with q2 select is rejected:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c cross_query_frag_bad.ml 2>errors.log
  [2]
  $ grep -q "Q1.brand" errors.log && grep -q "Q2.brand" errors.log && echo "rejected: q1 fragment is not a q2 fragment"
  rejected: q1 fragment is not a q2 fragment

Nor combined through the library-level Sqlgg_scope.apply:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c mix_apply_bad.ml 2>errors2.log
  [2]
  $ grep -q "Q1.brand" errors2.log && grep -q "Q2" errors2.log && echo "rejected: apply does not mix brands"
  rejected: apply does not mix brands
