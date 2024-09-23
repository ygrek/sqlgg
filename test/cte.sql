CREATE TABLE test_table1 (
    col1 INT,
    col2 INT,
    col3 DECIMAL(10, 2)
);

CREATE TABLE test_table2 (
    col4 INT,
    col5 VARCHAR(50)
);

WITH cte1 AS (
    SELECT col2, AVG(col3) AS col6
    FROM test_table1
    GROUP BY col2
),
cte2 AS (
    SELECT t2.col4, t2.col5, c1.col6
    FROM test_table2 t2
    JOIN cte1 c1 ON t2.col4 = c1.col2
    WHERE c1.col6 > @some_value
)
SELECT *
FROM cte2;
