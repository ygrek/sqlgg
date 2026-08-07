module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_drop_col_age db  =
    T.execute_unprepared db ("ALTER TABLE `users` DROP COLUMN `age`")

  let revert_20260101000000_alter_users_drop_col_age db  =
    T.execute_unprepared db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL")

  let migrations = [
    ("20260101000000_alter_users_drop_col_age", apply_20260101000000_alter_users_drop_col_age, revert_20260101000000_alter_users_drop_col_age);
  ]

end (* module Mig *)
