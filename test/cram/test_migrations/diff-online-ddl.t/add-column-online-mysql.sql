-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_age
ALTER TABLE `users` ADD COLUMN `age` INT, ALGORITHM=INSTANT;
ALTER TABLE `users` DROP COLUMN `age`, ALGORITHM=INPLACE, LOCK=NONE;
