-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_drop_index_email_idx
ALTER TABLE `users` DROP INDEX `email_idx`, ALGORITHM=INPLACE, LOCK=NONE;
ALTER TABLE `users` ADD UNIQUE INDEX `email_idx` (`email`), ALGORITHM=INPLACE, LOCK=NONE;
