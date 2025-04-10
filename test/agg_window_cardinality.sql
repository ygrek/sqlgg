-- @this_cardinality_one
SELECT SUM(running_total) as total_sum
FROM (
    SELECT sum(evt) OVER (PARTITION BY id ORDER BY dt) as running_total
    FROM (
        VALUES 
            row(1, 1, 1), 
            row(1, 1, 1), 
            row(1, 2, -1), 
            row(1, 2, 1), 
            row(1, 3, -1), 
            row(1, 4, -1)
    ) as t(id, dt, evt)
) as subquery;


-- @this_cardinality_n
SELECT SUM(evt) OVER (PARTITION BY id ORDER BY dt) as running_total
FROM (
    VALUES 
        row(1, 1, 1), 
        row(1, 1, 1), 
        row(1, 2, -1), 
        row(1, 2, 1), 
        row(1, 3, -1), 
        row(1, 4, -1)
) AS t(id, dt, evt);
