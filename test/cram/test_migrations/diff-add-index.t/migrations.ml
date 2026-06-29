module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_add_unique_email_idx db  =
    T.execute db ("ALTER TABLE `users` ADD UNIQUE INDEX `email_idx` (`email`)") T.no_params

  let revert_20260101000000_alter_users_add_unique_email_idx db  =
    T.execute db ("ALTER TABLE `users` DROP INDEX `email_idx`") T.no_params

  let migrations = [
    ("20260101000000_alter_users_add_unique_email_idx", apply_20260101000000_alter_users_add_unique_email_idx, revert_20260101000000_alter_users_add_unique_email_idx);
  ]

end (* module Mig *)
