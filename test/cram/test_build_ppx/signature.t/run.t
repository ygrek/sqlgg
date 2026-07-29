The interface and the implementation are derived separately, so they have to
agree. The signature is compiled first, the implementation against it.

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c feed.mli
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c feed.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c m.mli
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c m.ml

A nested record's columns reach the parent through the derived <t>_cols type.
The parent never names them, and the child can live in another module.

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c use_post.ml

A function says nothing about the column type, so an interface cannot be
derived from it.

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c wrong.mli 2>&1 | grep -c "sqlgg.map"
  2
