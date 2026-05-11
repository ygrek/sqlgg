A purely manual change (a column rename, which the diff engine cannot express as
anything but drop+add) lives in extends.sql. The target already reflects it, so
there is no new delta to append. Even so, -migrate must still (re)generate the
code from the recorded migrations - otherwise the hand-written migration would
never reach migrations.ml.

Nothing is auto-generated here, so -migrations-file is not even needed; the
manual rename lives in extends.sql:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -extends extends.sql -target target.sql > migrations.ml
  nothing new to migrate; regenerated code from 1 recorded migration(s)

extends.sql is untouched:

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20260609000000
  ALTER TABLE users RENAME COLUMN email TO email_address;
  ALTER TABLE users RENAME COLUMN email_address TO email;

But the code is regenerated and contains the hand-written rename:

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_0 db  =
      T.execute db ("ALTER TABLE users RENAME COLUMN email TO email_address") T.no_params
  
    let revert_alter_users_0 db  =
      T.execute db ("ALTER TABLE users RENAME COLUMN email_address TO email") T.no_params
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
    ]
  
  end (* module Migrations *)

Materialize confirms the loop is closed (current == target):

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base extends.sql | diff current.sql -
