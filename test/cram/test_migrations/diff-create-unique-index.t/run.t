Creating a table with a single-column UNIQUE index must not double the constraint:
the uniqueness is emitted once, as a follow-up ADD UNIQUE INDEX, and the inline
`UNIQUE` (which Tables.mark_unique_columns attaches to the column) is stripped from
the CREATE. The internal round-trip verification (which re-parses the generated SQL
and now compares index kinds too) would reject a duplicate unique key.

(SQL output is intentionally not exercised here: a CREATE with a follow-up index is
multi-statement and cannot be a single SQL migration entry; the ml generator runs
the whole apply string, so it shows the result.)

  $ sqlgg -no-header -dialect mysql -diff -ddl-as-migration -now 20260101000000 -gen caml -name mig -base initial.sql -target target.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_20260101000000_create_u db  =
      T.execute db ("CREATE TABLE `u` (`id` INT, `email` VARCHAR(255), PRIMARY KEY (`id`));\n\
  ALTER TABLE `u` ADD UNIQUE INDEX `uniq_email` (`email`)") T.no_params
  
    let revert_20260101000000_create_u db  =
      T.execute db ("DROP TABLE `u`") T.no_params
  
    let migrations = [
      ("20260101000000_create_u", apply_20260101000000_create_u, revert_20260101000000_create_u);
    ]
  
  end (* module Mig *)
