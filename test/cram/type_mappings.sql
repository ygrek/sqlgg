CREATE TABLE table_37 (
        -- [sqlgg] module=HelloWorld
        col_1 INT PRIMARY KEY,
        -- [sqlgg] module=Abcdefg
        -- [sqlgg] get_column=test
        col_2 INT NOT NULL
      );

 CREATE TABLE table_38 (
        -- [sqlgg] module=FooBar
        col_3 INT PRIMARY KEY,
        col_4 TEXT NOT NULL
      );

SELECT 
  (
    SELECT MAX(x.col_val)
    FROM (
      SELECT col_1 as col_val,
           1 + 3 as aaaaa,
           1 + col_1 as bbb
      FROM table_37 
      WHERE col_1 > (
        SELECT MIN(col_1) 
        FROM table_37
      )
    ) as x
  ) as deeply_nested_query,
  col_2,
  col_3,
  col_4
FROM table_37
LEFT JOIN table_38
  ON table_37.col_1 = table_38.col_3;
