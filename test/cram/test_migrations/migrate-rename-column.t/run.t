Hand-written RENAME migrations (RENAME COLUMN and RENAME TABLE) are applied when
materializing the schema: the column `name` becomes `full_name` and the table
`users` becomes `members`, while the other columns keep their order. The output
is round-trip verified internally before printing. current.sql is the golden
snapshot; the generator must reproduce it byte-for-byte (no diff output ==
equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql | diff current.sql -
