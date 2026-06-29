Drop a column: up = DROP COLUMN, down = ADD COLUMN with the reconstructed
definition. The committed migrations.sql / migrations.ml are the generation
result; the generator must still reproduce them (no diff output == equal).

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base initial.sql -target target.sql | diff migrations.sql -

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen caml -name mig -base initial.sql -target target.sql | diff migrations.ml -
