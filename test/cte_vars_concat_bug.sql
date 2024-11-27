CREATE TABLE test_table (
    test_1_column DATETIME,
    test_2_column INT,
    test_3_column INT
);

WITH RECURSIVE test_1 AS (
    SELECT MAKEDATE(@year, 1) AS date_
    UNION ALL
    SELECT date_ + INTERVAL 1 DAY
    FROM test_1
    WHERE date_ + INTERVAL 1 DAY < LEAST(MAKEDATE(@year + 1, 1), CURRENT_DATE + INTERVAL 1 DAY)
),
test_1_1 AS (
    SELECT
        DATE(test_1_column) AS test_1_column_1,
        COUNT(DISTINCT test_2_column) AS test_2_count,
        COUNT(DISTINCT test_3_column) AS test_3_count
    FROM test_table
    WHERE test_1_column >= MAKEDATE(@year, 1) 
      AND test_1_column < MAKEDATE(@year + 1, 1)
    GROUP BY DATE(test_1_column)
),
test_2 AS (
    SELECT
        COUNT(1) AS test_2_count_1,
        COUNT(DISTINCT test_3_column) AS test_3_count_1
    FROM test_table
    WHERE test_1_column >= MAKEDATE(@year, 1) 
      AND test_1_column < MAKEDATE(@year + 1, 1)
)
SELECT
    test_2.test_2_count_1,
    test_2.test_3_count_1,
    COALESCE(test_1_1.test_2_count, 0) AS test_2_count,
    COALESCE(test_1_1.test_3_count, 0) AS test_3_count,
    test_1.date_
FROM test_1
LEFT JOIN test_1_1 ON test_1.date_ = test_1_1.test_1_column_1
CROSS JOIN test_2
ORDER BY test_1.date_ DESC;
