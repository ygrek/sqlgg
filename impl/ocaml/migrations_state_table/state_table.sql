-- [sqlgg] name=create_state_table
-- [sqlgg] subst=state_table
CREATE TABLE IF NOT EXISTS `%%state_table%%` (
  -- [sqlgg] get_column=Fun.id
  -- [sqlgg] set_param=Sqlgg_migrations.Migration_id.to_string
  `migration_id` VARBINARY(191) NOT NULL PRIMARY KEY,
  -- [sqlgg] get_column=Fun.id
  -- [sqlgg] set_param=Int64.of_int
  `batch` BIGINT NOT NULL,
  -- [sqlgg] get_column=Fun.id
  -- [sqlgg] set_param=Sqlgg_migrations.Migration_state.to_string
  `state` ENUM(
    'applying',
    'applied',
    'reverting',
    'apply_failed',
    'revert_failed'
  ) NOT NULL,
  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    ON UPDATE CURRENT_TIMESTAMP
);

-- [sqlgg] name=list_rows
-- [sqlgg] subst=state_table
SELECT `migration_id`, `batch`, `state` FROM `%%state_table%%`;

-- [sqlgg] name=insert_applying
-- [sqlgg] subst=state_table
INSERT INTO `%%state_table%%` (`migration_id`, `batch`, `state`) VALUES (@migration_id, @batch, @state);

-- [sqlgg] name=transition_state
-- [sqlgg] subst=state_table
UPDATE `%%state_table%%` SET `state` = @next WHERE `migration_id` = @migration_id AND `state` = @expected;

-- [sqlgg] name=delete_reverting
-- [sqlgg] subst=state_table
DELETE FROM `%%state_table%%` WHERE `migration_id` = @migration_id AND `state` = @state;
