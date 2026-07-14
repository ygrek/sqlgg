Generated code for two dynamic queries is diffed against the committed golden file:

  $ cat scope.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ diff scope.expected.ml output.ml
