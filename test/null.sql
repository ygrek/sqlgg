CREATE TABLE IF NOT EXISTS `test` (
  `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  `nullable` TIMESTAMP NULL
);

-- issue #45
-- @create
INSERT INTO `test` (`nullable`) VALUES (
  `nullable` = CASE @nullable WHEN 0 THEN NULL ELSE @nullable END
);
