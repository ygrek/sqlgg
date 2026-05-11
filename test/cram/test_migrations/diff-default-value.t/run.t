Regression: a column DEFAULT must survive into the generated up migration.
Previously the default SQL could be silently dropped when its source text was
not captured; now the value is always emitted (or the generator fails loudly).

migrations.sql is the golden delta; the test asserts the generator reproduces it
byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -base initial.sql -target target.sql | diff migrations.sql -
