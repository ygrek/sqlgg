-- [sqlgg] generated
-- [sqlgg] id=20251201093015
ALTER TABLE `users` ADD COLUMN `email` TEXT;
ALTER TABLE `users` DROP COLUMN `email`;

-- [sqlgg] generated
-- [sqlgg] id=20251203142233
ALTER TABLE `posts` ADD COLUMN `author_id` INT NOT NULL;
ALTER TABLE `posts` DROP COLUMN `author_id`;
