Creating a table with a single-column UNIQUE index must not double the constraint:
the uniqueness is emitted once, as a `UNIQUE KEY` clause of the CREATE, and the
column-level `UNIQUE` (which Tables.mark_unique_columns attaches to the column) is
stripped. The internal round-trip verification (which re-parses the generated SQL
and now compares index kinds too) would reject a duplicate unique key.

  $ sqlgg -no-header -dialect mysql -diff -ddl-as-migration -now 20260101000000 -gen caml -name mig -base initial.sql -target target.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101000000_create_u db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE `u` (`id` INT, `email` VARCHAR(255), PRIMARY KEY (`id`), UNIQUE KEY `uniq_email` (`email`))") ~name:"apply_20260101000000_create_u" ~kind:Sqlgg_traits.Query.Other ())
  
    let revert_20260101000000_create_u db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("DROP TABLE `u`") ~name:"revert_20260101000000_create_u" ~kind:Sqlgg_traits.Query.Other ())
  
    let migrations = [
      ("20260101000000_create_u", apply_20260101000000_create_u, revert_20260101000000_create_u);
    ]
  
  end (* module Mig *)
