ENUM column with [sqlgg] module= meta decoded through a custom variant across two queries:

  $ cp ../print_ocaml_impl.ml .
  $ cat custom_enum.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ grep -c "Color.get_column" output.ml
  2
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c color.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx,yojson -I . -c custom_enum_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o custom_enum.exe print_ocaml_impl.cmo color.cmo output.cmo custom_enum_run.cmo
  $ ./custom_enum.exe 2>&1 | grep -E '\[SQL\]|Q1|Q2'
  [SQL] SELECT id, color, name FROM tinted_items WHERE id = 7
  Q1 id=7 color=Green
  Q1 name=gadget
  [SQL] SELECT id, color FROM tinted_items WHERE id > 1
  Q2 id=8 color=Blue
