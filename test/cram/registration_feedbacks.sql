CREATE TABLE IF NOT EXISTS registration_feedbacks (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `user_message` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `user_message_2` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

-- @test_name
SELECT * FROM registration_feedbacks WHERE
  id = @id AND
 { 
  `user_message` = @search 
    OR `user_message` = @search
    OR `user_message_2` = @search2 
    OR `user_message_2` IN @xs
    OR @xss { A { user_message_2 = @a } | B { user_message_2 = @b } }
 }?;
