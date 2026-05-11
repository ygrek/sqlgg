Materialize the current schema from initial + the up-migration stack (-gen sql).
Declaration order is preserved, new columns are appended, indexes are inlined
into CREATE TABLE. The output is round-trip verified internally before printing.

current.sql is the golden snapshot; the test asserts the generator reproduces it
byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql -base 002.up.sql | diff current.sql -
