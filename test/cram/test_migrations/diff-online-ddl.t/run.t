Without an alter-lock policy, generated ALTER TABLE statements use the server
default.

  $ D="sqlgg -no-header -dialect mysql -diff -gen sql -now 20260101000000 -base initial.sql -target target.sql"
  $ $D | diff plain.sql -

An explicit policy is applied to both directions of every generated ALTER.

  $ $D -alter-lock none | diff lock-none.sql -

Algorithm and lock policies can be combined.

  $ $D -alter-algorithm inplace -alter-lock none | diff alter-options.sql -

The policy is not accepted for dialects without MySQL LOCK semantics.

  $ sqlgg -no-header -dialect tidb -diff -gen sql -alter-lock none -base initial.sql -target target.sql 2>&1
  -alter-lock is only supported for dialect mysql
  [1]

Input ALTER options participate in dialect validation on the XML path.

  $ sqlgg -no-header -dialect tidb -gen xml explicit-lock.sql 2>&1
  Feature AlterLock is not supported for dialect TiDB (supported by: MySQL) at LOCK=NONE
  Errors encountered, no code generated
  [1]

Unknown lock values fail during argument parsing.

  $ $D -alter-lock invalid 2>&1
  unknown ALTER TABLE lock "invalid" (expected default|none|shared|exclusive)
  [1]

  $ $D -alter-algorithm invalid 2>&1
  unknown ALTER TABLE algorithm "invalid" (expected default|instant|inplace|copy)
  [1]
