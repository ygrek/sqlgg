module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_add_pk db  =
    T.execute db ("ALTER TABLE `users` ADD PRIMARY KEY (`id`)") T.no_params

  let revert_20260101000000_alter_users_add_pk db  =
    T.execute db ("ALTER TABLE `users` DROP PRIMARY KEY") T.no_params

  let migrations = [
    ("20260101000000_alter_users_add_pk", apply_20260101000000_alter_users_add_pk, revert_20260101000000_alter_users_add_pk);
  ]

end (* module Mig *)
