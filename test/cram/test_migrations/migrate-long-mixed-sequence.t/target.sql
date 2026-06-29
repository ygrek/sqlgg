CREATE TABLE `users` (`id` INT NOT NULL, `name` TEXT, `email` TEXT, `bio` TEXT, `created_at` INT NOT NULL);
CREATE TABLE `posts` (`id` INT NOT NULL, `title` TEXT, `author_id` INT NOT NULL, `body` TEXT, `published` INT NOT NULL);
