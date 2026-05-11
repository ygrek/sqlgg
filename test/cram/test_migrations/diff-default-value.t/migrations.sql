-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_t_add_col_status
ALTER TABLE `t` ADD COLUMN `status` TEXT NOT NULL DEFAULT 'new';
ALTER TABLE `t` DROP COLUMN `status`;
