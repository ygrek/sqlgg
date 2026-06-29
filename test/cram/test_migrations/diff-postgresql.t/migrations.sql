-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_email
ALTER TABLE `users` ADD COLUMN `email` TEXT NOT NULL;
ALTER TABLE `users` DROP COLUMN `email`;
