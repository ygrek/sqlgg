Materializing a generated schema again yields the exact same DDL (idempotent).

  $ sqlgg -no-header -dialect mysql -gen sql -base initial.sql -base 001.up.sql > current.sql
  $ sqlgg -no-header -dialect mysql -gen sql -base current.sql > current2.sql
  $ diff current.sql current2.sql && echo IDEMPOTENT
  IDEMPOTENT
