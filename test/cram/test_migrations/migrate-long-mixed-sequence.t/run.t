Long sequence with manual and generated migrations interleaved by `id`, in the
declarative -migrate / block format - the single migrations tool. Every
migration is anchored with `-- [sqlgg] id=N`; the statement that follows an
id-anchored up (and carries no id of its own) is taken as that migration's down
(see glue_downs). The `id` does all the disambiguation, so up and down are just
two plain statements - no special flag is involved.
All inputs are hand-written: initial.sql (baseline), extends.sql (MANUAL blocks,
with a hand-written up and down, read-only), seed.sql (the GENERATED blocks from
earlier runs) and target.sql (the desired final schema). The tool itself only
computes the NEW delta and appends it to the working migrations.sql.

ids are timestamps. Generated blocks carry a full YYYYMMDDHHMMSS stamp down to
the second (whatever the clock was); a human need not be that precise and may pin
a manual block to just a day (20251202). Coarse ids are compared as the start of
their period (20251202 sorts as 20251202000000), so the merge interleaves manual
and generated purely by id: seed 20251201093015, manual 20251202, seed
20251203142233, manual 20251204.

initial schema: two small tables.

  $ cat initial.sql
  CREATE TABLE users (id INT NOT NULL, name TEXT);
  CREATE TABLE posts (id INT NOT NULL, title TEXT);

Prior generated migrations (full timestamps, the output of earlier runs) live in
their own file, seed.sql:

  $ cat seed.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20251201093015
  ALTER TABLE `users` ADD COLUMN `email` TEXT;
  ALTER TABLE `users` DROP COLUMN `email`;
  
  -- [sqlgg] generated
  -- [sqlgg] id=20251203142233
  ALTER TABLE `posts` ADD COLUMN `author_id` INT NOT NULL;
  ALTER TABLE `posts` DROP COLUMN `author_id`;

-migrate rewrites the migrations log in place, so copy the seed into a writable
working file first:

  $ cat seed.sql > migrations.sql

Run -migrate: it replays initial + all recorded blocks (merged by id), diffs
against target, and appends only the genuinely new delta. Two new columns are
missing, so two generated blocks are appended, both stamped with the pinned -now
(20260101000000) and told apart by their descriptive name:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > migrations.ml
  appended 2 migration(s) to migrations.sql (id 20260101000000_alter_posts_add_col_published, 20260101000000_alter_users_add_col_created_at)

The generated log now carries the two seed timestamps and the two new
20260101000000_* (the manual day-pinned blocks live in extends):

  $ diff expected.sql migrations.sql

extends.sql is untouched (manual blocks pinned to a day):

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20251202
  ALTER TABLE users ADD COLUMN bio TEXT;
  ALTER TABLE users DROP COLUMN bio;
  
  -- [sqlgg] manual
  -- [sqlgg] id=20251204
  ALTER TABLE posts ADD COLUMN body TEXT;
  ALTER TABLE posts DROP COLUMN body;

The generated code merges everything by id: manual (day-pinned) and generated
(full-timestamp) interleaved into one ordered sequence:

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_0 db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `email` TEXT") T.no_params
  
    let revert_alter_users_0 db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `email`") T.no_params
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users ADD COLUMN bio TEXT") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE users DROP COLUMN bio") T.no_params
  
    let apply_alter_posts_2 db  =
      T.execute db ("ALTER TABLE `posts` ADD COLUMN `author_id` INT NOT NULL") T.no_params
  
    let revert_alter_posts_2 db  =
      T.execute db ("ALTER TABLE `posts` DROP COLUMN `author_id`") T.no_params
  
    let apply_alter_posts_3 db  =
      T.execute db ("ALTER TABLE posts ADD COLUMN body TEXT") T.no_params
  
    let revert_alter_posts_3 db  =
      T.execute db ("ALTER TABLE posts DROP COLUMN body") T.no_params
  
    let apply_20260101000000_alter_posts_add_col_published db  =
      T.execute db ("ALTER TABLE `posts` ADD COLUMN `published` INT NOT NULL") T.no_params
  
    let revert_20260101000000_alter_posts_add_col_published db  =
      T.execute db ("ALTER TABLE `posts` DROP COLUMN `published`") T.no_params
  
    let apply_20260101000000_alter_users_add_col_created_at db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `created_at` INT NOT NULL") T.no_params
  
    let revert_20260101000000_alter_users_add_col_created_at db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `created_at`") T.no_params
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
      ("alter_users_1", apply_alter_users_1, revert_alter_users_1);
      ("alter_posts_2", apply_alter_posts_2, revert_alter_posts_2);
      ("alter_posts_3", apply_alter_posts_3, revert_alter_posts_3);
      ("20260101000000_alter_posts_add_col_published", apply_20260101000000_alter_posts_add_col_published, revert_20260101000000_alter_posts_add_col_published);
      ("20260101000000_alter_users_add_col_created_at", apply_20260101000000_alter_users_add_col_created_at, revert_20260101000000_alter_users_add_col_created_at);
    ]
  
  end (* module Migrations *)

Re-running is a no-op: current already equals target (the diff engine sorts by
id, so the interleaved manual blocks are seen in the right order):

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -extends extends.sql -target target.sql > /dev/null
  nothing new to migrate; regenerated code from 6 recorded migration(s)
