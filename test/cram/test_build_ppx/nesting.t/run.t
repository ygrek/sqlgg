  $ cat joins.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > joins.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c joins.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c run.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe joins.cmo print_impl.cmo run.cmo
  $ ./run.exe | grep '^|'
  | row                 | [@sqlgg.nested]                | default_none
  | all present         | 2/bb c=3/cc                    | 2/bb c=3/cc
  | leaf absent         | 2/bb c=none                    | 2/bb c=none
  | middle absent       | none                           | none
  | half a leaf         | raises sqlgg: cc.cname is NULL | 2/bb c=none
  | plain child, absent | none                           | none

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c inner_in_left.ml 2>&1 | head -2
  File "inner_in_left.ml", line 8, characters 19-42:
  8 | let _ = aa_of_cols Db.Left_then_inner.cols
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c inner_alongside.ml
