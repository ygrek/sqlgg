CREATE TABLE IF NOT EXISTS `test` (
  `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  `nullable` TIMESTAMP NULL,
  `nullable_too` TIMESTAMP DEFAULT NULL,
  `nullable_int` INTEGER NULL
);

-- issue #45
-- @insert1
INSERT INTO `test` SET
  `nullable` = CASE @maybe WHEN 0 THEN NULL ELSE @maybe END
;

-- @insert2
INSERT INTO `test` SET `nullable` = NULLIF(@maybe, 0);

-- @list
SELECT `id`, IFNULL(`nullable`, 0) `nullable` FROM `test`;

-- @insert_nullable
INSERT INTO `test` SET `nullable` = @maybe;

-- @insert_all
INSERT INTO test VALUES;

-- @list_nullable
SELECT `id`, `nullable` FROM test;

-- @case_todo
SELECT CASE @x WHEN 0 THEN NULL ELSE @x END;

-- @select_nullif
SELECT NULLIF(@maybe, 0);

-- @select_ifnull
SELECT IFNULL(@maybe, 0);

-- @select_plus
SELECT id + IFNULL(nullable_int, 0) FROM test;

-- @select_func
SELECT nullable, floor(unix_timestamp(nullable)/3600) FROM test;
