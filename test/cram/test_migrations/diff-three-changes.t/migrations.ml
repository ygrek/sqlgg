module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101120000_alter_c_change_col_id db  =
    T.execute db ("ALTER TABLE `c` CHANGE COLUMN `id` `id` BIGINT NOT NULL") T.no_params

  let revert_20260101120000_alter_c_change_col_id db  =
    T.execute db ("ALTER TABLE `c` CHANGE COLUMN `id` `id` INT NOT NULL") T.no_params

  let apply_20260101120000_alter_b_drop_col_old db  =
    T.execute db ("ALTER TABLE `b` DROP COLUMN `old`") T.no_params

  let revert_20260101120000_alter_b_drop_col_old db  =
    T.execute db ("ALTER TABLE `b` ADD COLUMN `old` INT NOT NULL") T.no_params

  let apply_20260101120000_alter_a_add_col_x db  =
    T.execute db ("ALTER TABLE `a` ADD COLUMN `x` INT NOT NULL") T.no_params

  let revert_20260101120000_alter_a_add_col_x db  =
    T.execute db ("ALTER TABLE `a` DROP COLUMN `x`") T.no_params

  let migrations = [
    ("20260101120000_alter_c_change_col_id", apply_20260101120000_alter_c_change_col_id, revert_20260101120000_alter_c_change_col_id);
    ("20260101120000_alter_b_drop_col_old", apply_20260101120000_alter_b_drop_col_old, revert_20260101120000_alter_b_drop_col_old);
    ("20260101120000_alter_a_add_col_x", apply_20260101120000_alter_a_add_col_x, revert_20260101120000_alter_a_add_col_x);
  ]

end (* module Mig *)
