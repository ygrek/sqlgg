Global -dynamic-select: every select gets both versions.

  $ cat global.sql | sqlgg -gen caml -no-header -dialect=mysql -dynamic-select - > global.ml
  $ diff global.ml global.compare.ml
