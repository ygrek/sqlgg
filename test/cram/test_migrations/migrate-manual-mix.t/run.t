Generated and hand-written migrations live in two files merged by `id`:
migrations.sql holds the generated blocks, extends.sql the hand-written ones.
A manual migration (the `bio` column, pinned by hand to a day id=20251231, up and
down written as plain statements) is already
applied via extends.sql. -migrate merges both by id, sees `bio` as part of the
current state, and only generates the genuinely new delta (`age`), stamped with a
timestamp id that sorts after it. extends.sql is read-only and never touched.

Start with an empty generated log; the manual migration lives in extends.sql:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260101000000_alter_users_add_col_age)

Only the generated delta lands in migrations.sql, stamped with the pinned -now
20260101000000 (which sorts after the manual day id=20251231):

  $ diff expected.sql migrations.sql

extends.sql is untouched:

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20251231
  ALTER TABLE users ADD COLUMN bio TEXT;
  ALTER TABLE users DROP COLUMN bio;

The code merges both by id (manual day 20251231 first, generated 20260101000000_* second):

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_0 db  =
      T.execute db ("ALTER TABLE users ADD COLUMN bio TEXT") T.no_params
  
    let revert_alter_users_0 db  =
      T.execute db ("ALTER TABLE users DROP COLUMN bio") T.no_params
  
    let apply_20260101000000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL") T.no_params
  
    let revert_20260101000000_alter_users_add_col_age db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `age`") T.no_params
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
      ("20260101000000_alter_users_add_col_age", apply_20260101000000_alter_users_add_col_age, revert_20260101000000_alter_users_add_col_age);
    ]
  
  end (* module Migrations *)

Re-running is a no-op (nothing new to diff):

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 2 recorded migration(s)

Materialize confirms the loop is closed (current == target), replaying bases in
id order:

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base extends.sql -base migrations.sql | diff current.sql -
