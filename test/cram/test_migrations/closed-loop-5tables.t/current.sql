CREATE TABLE `users` (`id` INT, `name` TEXT NOT NULL, `email` VARCHAR(255) NOT NULL, `status` INT NOT NULL DEFAULT 1, PRIMARY KEY (`id`), UNIQUE KEY `uniq_email` (`email`));
CREATE TABLE `posts` (`id` INT, `user_id` INT NOT NULL, `title` TEXT NOT NULL, `body` TEXT, `created_at` INT, PRIMARY KEY (`id`));
CREATE TABLE `comments` (`id` INT, `post_id` INT NOT NULL, `author` TEXT NOT NULL, `created_at` INT, PRIMARY KEY (`id`));
CREATE TABLE `tags` (`id` INT, `label` TEXT NOT NULL, PRIMARY KEY (`id`));
CREATE TABLE `post_tags` (`post_id` INT NOT NULL, `tag_id` INT NOT NULL, KEY `idx_tag` (`tag_id`));
