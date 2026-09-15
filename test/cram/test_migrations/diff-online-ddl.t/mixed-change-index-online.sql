-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_change_col_id
ALTER TABLE `users` CHANGE COLUMN `id` `id` BIGINT NOT NULL;
ALTER TABLE `users` CHANGE COLUMN `id` `id` INT NOT NULL;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_index_idx_name
ALTER TABLE `users` ADD INDEX `idx_name` (`name`), ALGORITHM=INPLACE, LOCK=NONE;
ALTER TABLE `users` DROP INDEX `idx_name`, ALGORITHM=INPLACE, LOCK=NONE;
