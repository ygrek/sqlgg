CREATE TABLE `users` (`id` INT, `name` VARCHAR(255) NOT NULL, `status` INT NOT NULL DEFAULT 1, PRIMARY KEY (`id`), UNIQUE KEY `uniq_name` (`name`));
CREATE TABLE `posts` (`id` INT, `user_id` INT NOT NULL, `body` TEXT, `created_at` INT, PRIMARY KEY (`id`));
