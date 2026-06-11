Charset + collation diff is fully reversible when both sides carry an explicit
collation. The up converts to the target charset/collation, the down converts
back to the baseline. This is the regression guard for the fix that made the
table charset non-optional: the COLLATE clause must survive into BOTH the up and
the down (previously it could be silently dropped on one side).

The whole delta is round-trip verified internally before printing; the generator
must reproduce the committed golden byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -base initial.sql -base initial.charset.sql -target target.sql -target target.charset.sql | diff migrations.sql -
