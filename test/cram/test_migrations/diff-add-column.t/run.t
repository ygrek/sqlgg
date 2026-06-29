Add a column. The generation result is committed as golden files in this dir:
migrations.sql (the up/down log: the up statement followed by the down as a plain
SQL statement, numbered with `id`) and migrations.ml (the code derived from it).
The test just asserts the generator still reproduces them byte-for-byte (no diff
output == equal).

migrations.sql (source of truth, what `-migrate` would append):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql | diff migrations.sql -

migrations.ml (derived from the log):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen caml -name mig -base initial.sql -target target.sql | diff migrations.ml -

`-diff` is the stateless primitive that computes one delta; the stateful,
append-only `-migrate` (cases 12-15) is what actually writes/extends
migrations.sql over time.
