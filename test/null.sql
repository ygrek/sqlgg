CREATE TABLE IF NOT EXISTS `test` (
  `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  `nullable` TIMESTAMP NULL
);

-- issue #45
-- @create
INSERT INTO `test` SET
  `nullable` = CASE @nullable WHEN 0 THEN NULL ELSE @nullable END
;

-- @create2
INSERT INTO `test` SET `nullable` = IFNULL(@nullable, 0);

-- @list
SELECT `id`, NULLIF(`nullable`, 0) `nullable` FROM `test`;
