Three independent changes to three different tables in a single target edit must
produce three separate migrations (one per table). Changes on distinct tables are
never collapsed into one migration (only multiple changes to the SAME table are
merged into one composite ALTER). Here: c changes a column type, b drops a column,
a adds a column - each becomes its own migration with its own inverse down.

With the timestamp_name id scheme each migration id is <timestamp>_<descriptive>,
so the action is visible in the id itself. migrations.sql is the golden delta; the
test asserts the generator reproduces it byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101120000 -base initial.sql -target target.sql | diff migrations.sql -

migrations.ml (derived from the same delta): under timestamp_name the generated
function names carry the full <timestamp>_<descriptive> id:

  $ sqlgg -no-header -dialect mysql -diff -gen caml -name mig -now 20260101120000 -base initial.sql -target target.sql | diff migrations.ml -
