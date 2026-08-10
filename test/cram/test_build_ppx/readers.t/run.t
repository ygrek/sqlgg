  $ cat shop.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > shop.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c shop.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c run.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe shop.cmo print_impl.cmo run.cmo
  $ ./run.exe | grep '^|'
  | row                    | [@sqlgg.nested]                    | default_none
  | matched                | 7/shelf/3                          | 7/shelf/3
  | no match               | none                               | none
  | half a relation        | raises sqlgg: stock.place is NULL  | none
  | defaulted column NULL  | 8/bin/0                            | 8/bin/0

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c drop_join.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o drop_join.exe shop.cmo print_impl.cmo drop_join.cmo
  $ ./drop_join.exe | grep -A1 "^\[SQL\]"
  [SQL] SELECT i.iid, i.tag
  FROM item i

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c strict_reader.ml 2>&1 | head -2
  File "strict_reader.ml", line 5, characters 22-34:
  5 | let _ = stock_of_cols Db.Wide.cols
