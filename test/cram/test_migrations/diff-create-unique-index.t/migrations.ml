module Mig (T : Sqlgg_traits.M_io) = struct

  module IO = T.IO

  let apply_create_u db  =
    T.execute db ("CREATE TABLE `u` (`id` INT, `email` VARCHAR(255), PRIMARY KEY (`id`));\n\
ALTER TABLE `u` ADD UNIQUE INDEX `uniq_email` (`email`)") T.no_params

  let revert_create_u db  =
    T.execute db ("DROP TABLE `u`") T.no_params

  let migrations = [
    ("create_u", [(apply_create_u, revert_create_u)]);
  ]

end (* module Mig *)
