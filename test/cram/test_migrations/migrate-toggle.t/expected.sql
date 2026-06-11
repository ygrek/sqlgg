-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_age
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
ALTER TABLE `users` DROP COLUMN `age`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000001_alter_users_drop_col_age
ALTER TABLE `users` DROP COLUMN `age`;
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;

-- [sqlgg] generated
-- [sqlgg] id=20260101000002_alter_users_add_col_age
ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL;
ALTER TABLE `users` DROP COLUMN `age`;
