-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_t_drop_index_ix_add_index_ix
ALTER TABLE `t` DROP INDEX `ix`, ADD INDEX `ix` (`a`, `b`);
ALTER TABLE `t` DROP INDEX `ix`, ADD INDEX `ix` (`a`);
