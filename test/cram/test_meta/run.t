  $ /bin/sh ../sqlgg_test.sh meta.sql meta.compare.ml -dialect=mysql
  $ echo $?
  0

  $ ocamlfind ocamlc -package sqlgg.traits -c codecs.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
