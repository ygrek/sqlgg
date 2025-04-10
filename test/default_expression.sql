CREATE TABLE IF NOT EXISTS registration_feedbacks (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT(''),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
