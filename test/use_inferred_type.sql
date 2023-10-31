CREATE TABLE IF NOT EXISTS `some_table` (
 `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
 `some_field` TEXT NULL,
 `some_field_2` TEXT NULL DEFAULT NULL
);

-- @create
INSERT INTO some_table (
  some_field,
  some_field_2
) VALUES (
  @some_field,
  case when @some_field = 'TEST' then 'A' else NULL end
);
