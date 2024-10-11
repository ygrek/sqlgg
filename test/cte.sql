CREATE TABLE test_table1 (
    col1 INT,
    col2 INT,
    col3 DECIMAL(10, 2)
);

CREATE TABLE test_table2 (
    col4 INT,
    col5 VARCHAR(50)
);

CREATE TABLE test22 (
    col_id INT PRIMARY KEY,
    col_value DECIMAL(10, 2),
    col_group VARCHAR(100)
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

SELECT *
FROM (
    WITH
    cte_grouped AS (
        SELECT col_group, AVG(col_value) AS avg_value
        FROM test22
        GROUP BY col_group
    ),
    cte_counts AS (
        SELECT col_group, COUNT(*) AS group_count
        FROM test22
        GROUP BY col_group
    ),
    cte_max_values AS (
        SELECT col_group, MAX(col_value) AS max_value
        FROM test22
        GROUP BY col_group
    ),
    cte_combined AS (
        SELECT 
            g.col_group, 
            g.avg_value, 
            c.group_count, 
            m.max_value
        FROM cte_grouped g
        JOIN cte_counts c ON g.col_group = c.col_group
        JOIN cte_max_values m ON g.col_group = m.col_group
    )
    SELECT col_group, avg_value, group_count, max_value
    FROM cte_combined
) AS dt
WHERE dt.group_count > 1 AND dt.avg_value > 50.00;
