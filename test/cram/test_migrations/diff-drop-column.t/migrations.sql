-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_drop_col_age
ALTER TABLE `users` DROP COLUMN `age`;
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
