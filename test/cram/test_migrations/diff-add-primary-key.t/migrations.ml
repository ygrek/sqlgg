module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_20260101000000_alter_users_add_pk db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` ADD PRIMARY KEY (`id`)") ~name:"apply_20260101000000_alter_users_add_pk" ~kind:Sqlgg_traits.Query.Other ())

  let revert_20260101000000_alter_users_add_pk db  =
    T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE `users` DROP PRIMARY KEY") ~name:"revert_20260101000000_alter_users_add_pk" ~kind:Sqlgg_traits.Query.Other ())

  let migrations = [
    ("20260101000000_alter_users_add_pk", apply_20260101000000_alter_users_add_pk, revert_20260101000000_alter_users_add_pk);
  ]

end (* module Mig *)
