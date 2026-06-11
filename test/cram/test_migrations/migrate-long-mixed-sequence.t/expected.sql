-- [sqlgg] generated
-- [sqlgg] id=20251201093015
ALTER TABLE `users` ADD COLUMN `email` TEXT;
ALTER TABLE `users` DROP COLUMN `email`;

-- [sqlgg] generated
-- [sqlgg] id=20251203142233
ALTER TABLE `posts` ADD COLUMN `author_id` INT NOT NULL;
ALTER TABLE `posts` DROP COLUMN `author_id`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_posts_add_col_published
ALTER TABLE `posts` ADD COLUMN `published` INT NOT NULL;
ALTER TABLE `posts` DROP COLUMN `published`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_created_at
ALTER TABLE `users` ADD COLUMN `created_at` INT NOT NULL;
ALTER TABLE `users` DROP COLUMN `created_at`;
