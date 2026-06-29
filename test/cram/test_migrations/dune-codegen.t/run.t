End-to-end dune integration: a committed migration log (migrations.sql, the
generated blocks) plus a hand-written log (extends.sql, manual blocks) are fed to
a plain `(with-stdout-to sql_migrations.ml (run sqlgg -migrate ...))` rule. No
shell wrapper, no temp-file dance: because -migrate always re-emits the module on
stdout (even with nothing to do), the rule is safe to run on every build and the
committed *.sql logs are never rewritten by it.

A tiny dune project (committed as a fixture next to the *.sql logs) wires
target.sql + initial.sql + the two logs into one rule whose only target is the
generated module:

  $ cat dune-project
  (lang dune 2.7)

  $ cat dune
  (rule
   (deps initial.sql extends.sql migrations.sql target.sql)
   (target sql_migrations.ml)
   (action
    (with-stdout-to sql_migrations.ml
     (run sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml
      -name migrations -initial initial.sql -migrations-file migrations.sql
      -extends extends.sql -target target.sql))))

Building the target regenerates the module from both logs merged by id (manual
day id=20251231 first, generated 20260101000000_* second). target already matches
current, so -migrate only regenerates and prints the human note to stderr:

  $ dune build ./sql_migrations.ml
  nothing new to migrate; regenerated code from 2 recorded migration(s)

  $ cat _build/default/sql_migrations.ml
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

The committed logs are untouched by the build - the rule only writes its target:

  $ cat migrations.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_users_add_col_age
  ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
  ALTER TABLE `users` DROP COLUMN `age`;

  $ cat extends.sql
  -- [sqlgg] manual
  -- [sqlgg] id=20251231
  ALTER TABLE users ADD COLUMN bio TEXT;
  ALTER TABLE users DROP COLUMN bio;

Rebuilding is a no-op for dune (nothing changed) and the generated module is
still there, byte-for-byte - the committed file is never blanked:

  $ dune build ./sql_migrations.ml
  $ test -s _build/default/sql_migrations.ml && echo non-empty
  non-empty
