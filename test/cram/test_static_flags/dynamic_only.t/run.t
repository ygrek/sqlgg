Per-statement [dynamic_select=true]: dynamic module only.

  $ cat dynamic_only.sql | sqlgg -gen caml -no-header -dialect=mysql - > dynamic_only.ml
  $ diff dynamic_only.ml dynamic_only.compare.ml
