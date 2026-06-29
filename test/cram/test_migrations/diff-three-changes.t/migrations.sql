-- [sqlgg] generated
-- [sqlgg] id=20260101120000_alter_c_change_col_id
ALTER TABLE `c` CHANGE COLUMN `id` `id` BIGINT NOT NULL;
ALTER TABLE `c` CHANGE COLUMN `id` `id` INT NOT NULL;

-- [sqlgg] generated
-- [sqlgg] id=20260101120000_alter_b_drop_col_old
ALTER TABLE `b` DROP COLUMN `old`;
ALTER TABLE `b` ADD COLUMN `old` INT NOT NULL;

-- [sqlgg] generated
-- [sqlgg] id=20260101120000_alter_a_add_col_x
ALTER TABLE `a` ADD COLUMN `x` INT NOT NULL;
ALTER TABLE `a` DROP COLUMN `x`;
