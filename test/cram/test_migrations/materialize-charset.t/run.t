Materializing a table charset/collation change: the table starts with no
explicit charset, an up-migration converts it to utf8mb4 with an explicit
collation, and the materialized snapshot must keep BOTH the charset and the
COLLATE. This guards the silent-collation-drop regression - the collation used
to be lost because the charset was modelled as optional and the COLLATE clause
was dropped when no charset was present.

current.sql is the golden snapshot; the generator round-trips it internally and
must reproduce it byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql | diff current.sql -
