Materialize a five-table schema from initial + a two-step up-migration stack.
Declaration order across all -base files is preserved, ALTER-added columns are
appended in place, and indexes (UNIQUE/plain) are inlined into CREATE TABLE.
The output is round-trip verified internally before printing.

current.sql is the golden snapshot; the test asserts the generator still
reproduces it byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql -base 002.up.sql | diff current.sql -
