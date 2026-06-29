Regression: unnamed indexes/UNIQUE and inline FULLTEXT must not be silently
dropped. Unnamed indexes get a deterministic MySQL-style name (first column,
with a numeric suffix on collision); FULLTEXT is emitted via ALTER TABLE.
The output is round-trip verified internally before printing.

current.sql is the golden snapshot; the test asserts the generator reproduces it
byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql | diff current.sql -
