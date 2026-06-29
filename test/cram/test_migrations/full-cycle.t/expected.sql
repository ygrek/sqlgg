-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_posts_add_col_title
ALTER TABLE `posts` ADD COLUMN `title` TEXT;
ALTER TABLE `posts` DROP COLUMN `title`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_name
ALTER TABLE `users` ADD COLUMN `name` TEXT;
ALTER TABLE `users` DROP COLUMN `name`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000001_alter_posts_add_col_body
ALTER TABLE `posts` ADD COLUMN `body` TEXT;
ALTER TABLE `posts` DROP COLUMN `body`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000001_alter_users_add_col_email_add_pk
ALTER TABLE `users` ADD COLUMN `email` VARCHAR(255), ADD PRIMARY KEY (`id`);
ALTER TABLE `users` DROP COLUMN `email`, DROP PRIMARY KEY;

-- [sqlgg] generated
-- [sqlgg] id=20260101000002_alter_users_change_col_id_add_unique_email_idx
ALTER TABLE `users` CHANGE COLUMN `id` `id` BIGINT NOT NULL, ADD UNIQUE INDEX `email_idx` (`email`);
ALTER TABLE `users` CHANGE COLUMN `id` `id` INT NOT NULL, DROP INDEX `email_idx`;
