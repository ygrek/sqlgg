-migrate is idempotent by codegen: even when there is no delta AND no recorded
migration at all (the empty-log "nothing to do" branch), it still re-emits the
generated module to stdout. This is what makes a plain dune rule
`(with-stdout-to sql_migrations.ml (run sqlgg -migrate ...))` safe: a no-op run
never blanks the committed file. stderr keeps saying "nothing to do" and the
exit code stays 0; migrations.sql is NOT created/touched (append only on delta).

Empty generated log, target already equals initial - the totally-empty state:

  $ : > migrations.sql

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql 2>/dev/null
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let migrations = [
    ]
  
  end (* module Migrations *)

The human-facing note goes to stderr, not stdout:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql 2>&1 >/dev/null
  nothing to do

migrations.sql stays empty - nothing is appended on a no-op:

  $ wc -c < migrations.sql
  0

Running it again produces byte-identical stdout (true idempotency):

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql > a.ml 2>/dev/null
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -migrations-file migrations.sql -target target.sql > b.ml 2>/dev/null
  $ diff a.ml b.ml

Even with no -migrations-file at all (pure stdout codegen) the no-op still emits
the module instead of nothing:

  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations -initial initial.sql -target target.sql 2>/dev/null
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let migrations = [
    ]
  
  end (* module Migrations *)
