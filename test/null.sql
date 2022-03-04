CREATE TABLE IF NOT EXISTS `test` (
  `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  `nullable` TIMESTAMP NULL,
  `nullable_too` TIMESTAMP DEFAULT NULL
);

-- issue #45
-- @insert1
INSERT INTO `test` SET
  `nullable` = CASE @nullable WHEN 0 THEN NULL ELSE @nullable END
;

-- @insert2
INSERT INTO `test` SET `nullable` = NULLIF(@nullable, 0);

-- @list
SELECT `id`, IFNULL(`nullable`, 0) `nullable` FROM `test`;

-- @insert_nullable
INSERT INTO `test` SET `nullable` = @nullable;

-- @insert_all
INSERT INTO test VALUES;

-- @list_nullable
SELECT `id`, `nullable` FROM test;

-- @case_todo
SELECT CASE @x WHEN 0 THEN NULL ELSE @x END
