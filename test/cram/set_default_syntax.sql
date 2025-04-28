CREATE TABLE IF NOT EXISTS registration_feedbacks (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL DEFAULT(''),
  `grant_types` varchar(80) COLLATE utf8_bin NOT NULL DEFAULT 'implicit authorization_code',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

INSERT INTO `registration_feedbacks`
SET
  `user_message` = { CONCAT(@user_message, '22222') }??,
  `grant_types` = { @grant_types { A {'2'} | B {'2'} } }??;
