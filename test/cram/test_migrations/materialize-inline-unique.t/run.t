Regression guard: materializing a table that declares uniqueness as an inline
column-level `UNIQUE` (no named index) must keep it inline and portable -- it must
NOT be rewritten into a separate `UNIQUE KEY`. Only the diff/migration path
normalizes inline `UNIQUE` into an explicit index; the materialize/CREATE path
stays byte-for-byte faithful to the source form.

current.sql is the golden snapshot; the test asserts the generator reproduces it
byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql | diff current.sql -
