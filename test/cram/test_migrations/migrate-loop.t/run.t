All-in-one -migrate: diff initial+migrations against target, append the
generated up/down to migrations.sql, and (re)generate the code. The human only
ever edits target.sql.

Start from an empty migration log:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260101000000_alter_users_add_col_age)

The up and down landed in migrations.sql, tagged `generated` and stamped with an
`id` of <timestamp>_<descriptive> (the timestamp is the clock by default, here
pinned with -now); the down is the plain statement right after the up, which
materialize skips so it replays only the up.

  $ diff expected.sql migrations.sql

The code is regenerated from the migration SQL:

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101000000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL") T.no_params
  
    let revert_20260101000000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `age`") T.no_params
  
    let migrations = [
      ("20260101000000_alter_users_add_col_age", apply_20260101000000_alter_users_add_col_age, revert_20260101000000_alter_users_add_col_age);
    ]
  
  end (* module Migrations *)

Re-running with no change to target.sql is a no-op (files untouched):

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 1 recorded migration(s)
