CREATE TABLE IF NOT EXISTS `test` (
  `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
  `nullable` TIMESTAMP NULL,
  `nullable_too` TIMESTAMP DEFAULT NULL,
  `nullable_int` INTEGER NULL
);

-- issue #45
-- BUG/misfeature picks nullable for @maybe, while it could be strict
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

-- @case_BUG
-- out expected int nullable, not any
-- in could be strict
SELECT CASE @x WHEN 0 THEN NULL ELSE @x END;

-- @select_nullif_BUG
SELECT NULLIF(@maybe, 0);

-- BUG? IFNULL should make first param nullable?
-- @select_ifnull
SELECT IFNULL(@maybe, 0);

-- @select_ifnull_int
SELECT IFNULL(nullable_int, 0) FROM test;

-- @select_plus
SELECT id + IFNULL(nullable_int, 0) FROM test;

-- @select_plus_null
SELECT id + NULL FROM test;

-- @select_plus_nullable_BUG
SELECT id + nullable_int FROM test;

-- @select_func
SELECT nullable, floor(unix_timestamp(nullable)/3600) FROM test;

-- @get_max
SELECT
  max(nullable),
  IF(max(nullable) IS NULL, NULL, max(nullable))
FROM
  test
WHERE
  id = @id
LIMIT 1;

-- @params_unification
INSERT INTO test (
  nullable,
  nullable_too
) VALUES (
  @p,
  case when @p = 42 then 100 else NULL end
);
