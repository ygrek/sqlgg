module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101120000_alter_c_change_col_id db  =
    T.execute_unprepared db ("ALTER TABLE `c` CHANGE COLUMN `id` `id` BIGINT NOT NULL")

  let revert_20260101120000_alter_c_change_col_id db  =
    T.execute_unprepared db ("ALTER TABLE `c` CHANGE COLUMN `id` `id` INT NOT NULL")

  let apply_20260101120000_alter_b_drop_col_old db  =
    T.execute_unprepared db ("ALTER TABLE `b` DROP COLUMN `old`")

  let revert_20260101120000_alter_b_drop_col_old db  =
    T.execute_unprepared db ("ALTER TABLE `b` ADD COLUMN `old` INT NOT NULL")

  let apply_20260101120000_alter_a_add_col_x db  =
    T.execute_unprepared db ("ALTER TABLE `a` ADD COLUMN `x` INT NOT NULL")

  let revert_20260101120000_alter_a_add_col_x db  =
    T.execute_unprepared db ("ALTER TABLE `a` DROP COLUMN `x`")

  let migrations = [
    ("20260101120000_alter_c_change_col_id", apply_20260101120000_alter_c_change_col_id, revert_20260101120000_alter_c_change_col_id);
    ("20260101120000_alter_b_drop_col_old", apply_20260101120000_alter_b_drop_col_old, revert_20260101120000_alter_b_drop_col_old);
    ("20260101120000_alter_a_add_col_x", apply_20260101120000_alter_a_add_col_x, revert_20260101120000_alter_a_add_col_x);
  ]

end (* module Mig *)
