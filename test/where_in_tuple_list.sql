CREATE TABLE table1 (
  col1 TEXT NULL,
  col2 INT NULL
);

SELECT `col1`, `col2`
FROM `table1`
WHERE (1, col1, col2, col1 + col1, 6, 11, (@test3 :: Text)) IN @in_ AND col2 > 3;
