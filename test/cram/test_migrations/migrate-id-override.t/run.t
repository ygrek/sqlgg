A human overrides the migration id from the console with -now. The id is
<timestamp>_<descriptive>; -now pins the <timestamp> to whatever value the human
wants (the default would be the current clock). The merge then orders strictly by
that timestamp, and next_base always bumps a new id past everything already
recorded - so an override can never collide with or reorder existing migrations.

Start from an empty log:

  $ : > migrations.sql

The human pins -now to a far-future timestamp; it becomes the id verbatim:

  $ sqlgg -no-header -dialect mysql -migrate -now 20990704120000 -gen caml -name m -initial initial.sql -migrations-file migrations.sql -target target.sql > /dev/null
  appended 1 migration(s) to migrations.sql (id 20990704120000_alter_t_add_col_x)

Now the human pins -now to a much EARLIER timestamp for the next migration. It
cannot land before the one already recorded: next_base bumps it to max_id + 1, so
the override is honoured only up to the collision-free guarantee:

  $ sqlgg -no-header -dialect mysql -migrate -now 20200101000000 -gen caml -name m -initial initial.sql -migrations-file migrations.sql -target target2.sql > /dev/null
  appended 1 migration(s) to migrations.sql (id 20990704120001_alter_t_add_col_y)

The recorded ids are strictly increasing regardless of the pins chosen:

  $ grep '^-- \[sqlgg\] id=' migrations.sql
  -- [sqlgg] id=20990704120000_alter_t_add_col_x
  -- [sqlgg] id=20990704120001_alter_t_add_col_y
