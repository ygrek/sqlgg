module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_change_col_id db  =
    T.execute db ("ALTER TABLE `users` CHANGE COLUMN `id` `id` BIGINT NOT NULL") T.no_params

  let revert_20260101000000_alter_users_change_col_id db  =
    T.execute db ("ALTER TABLE `users` CHANGE COLUMN `id` `id` INT NOT NULL") T.no_params

  let migrations = [
    ("20260101000000_alter_users_change_col_id", apply_20260101000000_alter_users_change_col_id, revert_20260101000000_alter_users_change_col_id);
  ]

end (* module Mig *)
