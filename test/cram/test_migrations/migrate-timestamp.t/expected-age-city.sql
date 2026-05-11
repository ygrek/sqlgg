-- [sqlgg] generated
-- [sqlgg] id=20260101120000_alter_users_add_col_age
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
ALTER TABLE `users` DROP COLUMN `age`;

-- [sqlgg] generated
-- [sqlgg] id=20260102000000_alter_users_add_col_city
ALTER TABLE `users` ADD COLUMN `city` TEXT;
ALTER TABLE `users` DROP COLUMN `city`;
