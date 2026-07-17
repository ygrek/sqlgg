WHERE ... IS NOT NULL makes the column strict, in static and dynamic selects
alike; OR between guards gives no guarantee and keeps columns nullable:
  $ /bin/sh ./sqlgg_test.sh not_null_refine.sql not_null_refine.compare.ml
  $ echo $?
  0
