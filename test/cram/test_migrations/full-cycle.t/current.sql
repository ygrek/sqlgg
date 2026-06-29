CREATE TABLE `users` (`id` BIGINT NOT NULL, `bio` TEXT, `name` TEXT, `email` VARCHAR(255), PRIMARY KEY (`id`), UNIQUE KEY `email_idx` (`email`));
CREATE TABLE `posts` (`id` INT NOT NULL, `title` TEXT, `body` TEXT);
