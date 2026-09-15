-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_drop_col_a
ALTER TABLE `users` DROP COLUMN `a`, ALGORITHM=INPLACE, LOCK=NONE;
ALTER TABLE `users` ADD COLUMN `a` INT, ALGORITHM=INSTANT;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_drop_col_b
ALTER TABLE `users` DROP COLUMN `b`, ALGORITHM=INPLACE, LOCK=NONE;
ALTER TABLE `users` ADD COLUMN `b` INT NOT NULL;
