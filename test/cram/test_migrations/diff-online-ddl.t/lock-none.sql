-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_index_email_idx
ALTER TABLE `users` ADD INDEX `email_idx` (`email`), LOCK=NONE;
ALTER TABLE `users` DROP INDEX `email_idx`, LOCK=NONE;
