End-to-end closed loop on a 5-table schema:
baseline + stack of up-migrations -> materialized current -> a new declarative
diff -> proof that replaying the whole stack reproduces the edited target.

Step 1 - materialize the current schema from initial + 001 + 002.
Order of all five tables is preserved; new columns appended; indexes inlined.
current.sql is the golden snapshot (also reused as the -base for step 2):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql -base 002.up.sql | diff current.sql -

Step 2 - day-to-day change: target.sql adds `posts.views`.
-diff produces migration 003 with exact downs.

  $ sqlgg -no-header -dialect mysql -gen caml -name mig003 -diff -now 20260101000000 -base current.sql -target target.sql
  module Mig003 (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101000000_alter_posts_add_col_views db  =
      T.execute db ("ALTER TABLE `posts` ADD COLUMN `views` INT NOT NULL DEFAULT 0") T.no_params
  
    let revert_20260101000000_alter_posts_add_col_views db  =
      T.execute db ("ALTER TABLE `posts` DROP COLUMN `views`") T.no_params
  
    let migrations = [
      ("20260101000000_alter_posts_add_col_views", apply_20260101000000_alter_posts_add_col_views, revert_20260101000000_alter_posts_add_col_views);
    ]
  
  end (* module Mig003 *)

Step 3 - append 003.up.sql to the stack and re-materialize everything.

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql -base 002.up.sql -base 003.up.sql > current_v2.sql

Step 4 - the loop is closed: diffing the replayed stack against the edited
target yields no migrations.

  $ sqlgg -no-header -dialect mysql -gen caml -name check -diff -now 20260101000000 -base current_v2.sql -target target.sql
  module Check (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let migrations = [
    ]
  
  end (* module Check *)
