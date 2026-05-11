Pinning the clock with `-now`: migration ids are <timestamp>_<descriptive>. The
timestamp is the current clock by default; pinning it makes the output reproducible
and lets two people on separate branches get distinct ids (no race for `max+1`).
The wide spacing also leaves room to slot a hand-written migration *after* a given
timestamp.

First target (`target_age.sql`) - add `age`, numbered with a fixed timestamp:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101120000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target_age.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260101120000_alter_users_add_col_age)

  $ diff expected-age.sql migrations.sql

Step B - a hand-written `status` migration is slotted right AFTER `age` by giving
it the next timestamp (id=20260101120001) in extends.sql:

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20260101120001
  ALTER TABLE users ADD COLUMN status INT NOT NULL DEFAULT 1;
  ALTER TABLE users DROP COLUMN status;

Now also add `city` via target.sql; -migrate generates only `city`, numbered with
the new timestamp (which lands after the manual one):

  $ sqlgg -no-header -dialect mysql -migrate -now 20260102000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260102000000_alter_users_add_col_city)

Generated file holds only the generated blocks (age, city); extends.sql is
untouched:

  $ diff expected-age-city.sql migrations.sql

The code merges all three by id, so the manual `status` sits between `age` and
`city`, exactly where its timestamp puts it:

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101120000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL") T.no_params
  
    let revert_20260101120000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `age`") T.no_params
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users ADD COLUMN status INT NOT NULL DEFAULT 1") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE users DROP COLUMN status") T.no_params
  
    let apply_20260102000000_alter_users_add_col_city db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `city` TEXT") T.no_params
  
    let revert_20260102000000_alter_users_add_col_city db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `city`") T.no_params
  
    let migrations = [
      ("20260101120000_alter_users_add_col_age", apply_20260101120000_alter_users_add_col_age, revert_20260101120000_alter_users_add_col_age);
      ("alter_users_1", apply_alter_users_1, revert_alter_users_1);
      ("20260102000000_alter_users_add_col_city", apply_20260102000000_alter_users_add_col_city, revert_20260102000000_alter_users_add_col_city);
    ]
  
  end (* module Migrations *)

The loop is closed: re-running (with a later clock) finds nothing new, which
proves the id-merged current schema already equals target:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260103000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 3 recorded migration(s)
