CREATE TABLE IF NOT EXISTS registration_feedbacks (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT(''),
  `grant_types` varchar(80) COLLATE utf8_bin NOT NULL DEFAULT 'implicit authorization_code',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

INSERT INTO `registration_feedbacks`
SET
  `user_message` = @user_message,
  `grant_types` = @grant_types { None { DEFAULT } | Some { @grant_types } };

INSERT INTO `registration_feedbacks`
SET
  `user_message` = DEFAULT;
