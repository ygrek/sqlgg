-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_unique_email_idx
ALTER TABLE `users` ADD UNIQUE INDEX `email_idx` (`email`);
ALTER TABLE `users` DROP INDEX `email_idx`;
