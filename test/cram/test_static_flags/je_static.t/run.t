The static companion of a join-eliminating query keeps every join: the dynamic
module gets the join hole, wide_static gets the full fixed SQL.

  $ cat je_static.sql | sqlgg -gen caml -no-header -dialect=mysql - > je_static.ml
  $ diff je_static.ml je_static.compare.ml
