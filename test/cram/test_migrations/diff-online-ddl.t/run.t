-online-ddl appends ALGORITHM/LOCK clauses for safe MySQL and TiDB alters.
Without the flag, output stays byte-identical to the ordinary diff tests.

MySQL add unique index (INPLACE, LOCK=NONE):

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-target.sql | diff add-index-online-mysql.sql -

MySQL drop index:

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base drop-index-initial.sql -target drop-index-target.sql | diff drop-index-online-mysql.sql -

TiDB add index (ALGORITHM=INPLACE, no LOCK):

  $ sqlgg -no-header -dialect tidb -diff -online-ddl -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-target.sql | diff add-index-online-tidb.sql -

MySQL add nullable column (ALGORITHM=INSTANT):

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base add-column-initial.sql -target add-column-target.sql | diff add-column-online-mysql.sql -

Change column type stays without online clauses:

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base change-type-initial.sql -target change-type-target.sql | diff change-type-no-online.sql -

Two DROP COLUMN with same up online class but different inverse keys split apart:

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base drop-two-columns-initial.sql -target drop-two-columns-target.sql | diff drop-two-columns-online.sql -

Mixed unsafe CHANGE and safe ADD INDEX split into two migrations:

  $ sqlgg -no-header -dialect mysql -diff -online-ddl -gen sql -now 20260101000000 -base mixed-initial.sql -target mixed-target.sql | diff mixed-change-index-online.sql -

Without -online-ddl, add-index matches the existing golden (no suffix):

  $ sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base add-index-initial.sql -target add-index-target.sql | diff add-index-plain.sql -
