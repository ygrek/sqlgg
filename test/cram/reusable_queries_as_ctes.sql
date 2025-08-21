CREATE TABLE categories (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL
);

CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    category_id INTEGER,
    price NUMERIC(10, 2) NOT NULL,
    FOREIGN KEY (category_id) REFERENCES categories(id)
);

-- @abcd | include: reuse
SELECT 
    1 as y, 
    4 + @five as y1
FROM ( 
        SELECT 1 AS id, 'l' AS creator_name
        UNION ALL
        SELECT 2, 'k'
) AS x 
WHERE @param { None { TRUE } | Some { FALSE } };

-- @test2 
WITH x AS &abcd
SELECT 1 + @test - @test2 + @test5 + x.y1 as y2
FROM x;

-- @test3
WITH x AS (SELECT 
    1 as y, 
    4 + @five as y1
FROM ( 
        SELECT 1 AS id, 'l' AS creator_name
        UNION ALL
        SELECT 2, 'k'
) AS x 
WHERE @param { None { TRUE } | Some { FALSE } })
SELECT 1 + @test - @test2 + @test5 + x.y1 as y2
FROM x;


-- @reuseme | include: reuse_and_execute
WITH inner_cte AS (
        SELECT id, name, category_id 
        FROM products 
        WHERE price > 100
    )
SELECT inner_cte.*, c.name AS category_name
FROM inner_cte
JOIN categories c ON inner_cte.category_id = c.id;


-- @reuse_reusable
WITH outer_cte AS &reuseme
SELECT * FROM outer_cte;
