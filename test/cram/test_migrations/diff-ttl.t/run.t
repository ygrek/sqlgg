TiDB TTL is tracked as table state (like the default charset), so the diff
engine generates TTL transitions with auto-derived downs. TTL_ENABLE is sticky
in TiDB - setting TTL keeps the previous flag - so the generated SQL always
pins it explicitly to keep up/down deterministic. Every delta below is
round-trip verified internally before printing.

Adding TTL: the up sets the expression, the down removes it:

  $ sqlgg -no-header -dialect tidb -diff -now 20260101000000 -gen sql -base initial.sql -target initial.sql -target initial.ttl.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_foo_set_ttl
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON';
  ALTER TABLE `foo` REMOVE TTL;

Removing TTL is fully reversible - the down restores the recorded expression
(the old per-statement migration mode could not invert REMOVE TTL at all):

  $ sqlgg -no-header -dialect tidb -diff -now 20260101000000 -gen sql -base initial.sql -base initial.ttl.sql -target initial.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_foo_remove_ttl
  ALTER TABLE `foo` REMOVE TTL;
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON';

Toggling TTL_ENABLE only: both sides keep the same expression, only the flag
flips, and the down flips it back:

  $ sqlgg -no-header -dialect tidb -diff -now 20260101000000 -gen sql -base initial.sql -base initial.ttl.sql -target initial.sql -target target.ttl-off.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_foo_set_ttl
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'OFF';
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON';

Changing the interval: the down restores the previous expression:

  $ sqlgg -no-header -dialect tidb -diff -now 20260101000000 -gen sql -base initial.sql -base initial.ttl.sql -target initial.sql -target target.ttl-90d.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_foo_set_ttl
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 90 DAY TTL_ENABLE = 'ON';
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON';

TTL survives schema materialization (and parses back, closing the loop):

  $ sqlgg -no-header -dialect tidb -gen sql -base initial.sql -base initial.ttl.sql
  CREATE TABLE `foo` (`id` INT NOT NULL, `created_at` DATETIME NOT NULL);
  ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON';

The full -migrate cycle generates apply/revert code for the TTL change:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect tidb -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target initial.sql -target initial.ttl.sql > migrations.ml
  appended 1 migration(s) to migrations.sql (id 20260101000000_alter_foo_set_ttl)

  $ cat migrations.ml
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101000000_alter_foo_set_ttl db  =
      T.execute db ("ALTER TABLE `foo` TTL = `created_at` + INTERVAL 6 MONTH TTL_ENABLE = 'ON'") T.no_params
  
    let revert_20260101000000_alter_foo_set_ttl db  =
      T.execute db ("ALTER TABLE `foo` REMOVE TTL") T.no_params
  
    let migrations = [
      ("20260101000000_alter_foo_set_ttl", apply_20260101000000_alter_foo_set_ttl, revert_20260101000000_alter_foo_set_ttl);
    ]
  
  end (* module Migrations *)

Re-running is a no-op: the recorded TTL migration replays into the current
state and matches the target:

  $ sqlgg -no-header -dialect tidb -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target initial.sql -target initial.ttl.sql > /dev/null
  nothing new to migrate; regenerated code from 1 recorded migration(s)
