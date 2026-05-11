Column positioning (FIRST / AFTER) is honoured when materializing: `b` is
inserted right after `a`, and `z` is moved to the front, so the final column
order is z, a, b, c. The output is round-trip verified internally before
printing. current.sql is the golden snapshot; the generator must reproduce it
byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql | diff current.sql -
