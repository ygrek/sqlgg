When a charset change cannot be faithfully reversed, the generator refuses
instead of emitting a lying down-migration. Here the baseline only has an
implicit collation (DEFAULT CHARSET latin1, no COLLATE), so a down that switches
back to latin1 cannot reset the collation - replaying it would leave the target
collation in place. Internal round-trip verification catches this and fails
loudly, telling the author to write the down by hand (create, don't validate):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -base initial.sql -base initial.charset.sql -target target.sql -target target.charset.sql
  migration verification failed (write this step manually):
  reverting the migrations did not restore the original schema
  
  the schema we got:
  t(`id` INT NOT NULL,`name` TEXT) pk: idx: cs:(Named "latin1")/utf8mb4_unicode_ci ttl:
  
  the schema we expected:
  t(`id` INT NOT NULL,`name` TEXT) pk: idx: cs:(Named "latin1")/ ttl:
  [1]
