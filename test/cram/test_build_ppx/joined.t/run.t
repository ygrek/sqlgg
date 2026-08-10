  $ cat posts.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > posts.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c posts.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c run.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe posts.cmo print_impl.cmo run.cmo
  $ ./run.exe | grep '^channel'
  channel 10/ocaml img=pic

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c no_strict_column.ml 2>&1 | tail -4
  1 | type note = { note : string option; tag : string option }
  2 | [@@deriving sqlgg ~nullable_cols]
  Error: deriving sqlgg: ~nullable_cols needs a column that cannot be NULL, or
         drop the option
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c no_nullable_cols.ml 2>&1 | grep -o ch_nullable_cols | head -1
  ch_nullable_cols
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c by_without_conversion.ml 2>&1 | grep -o channel_nullable_cols | head -1
  channel_nullable_cols
