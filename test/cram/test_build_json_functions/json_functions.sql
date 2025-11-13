CREATE TABLE table1 (
    id INT PRIMARY KEY,
    col1 JSON,
    col2 JSON,
    col3 JSON
);

CREATE TABLE table2 (
    id INT PRIMARY KEY,
    name VARCHAR(255),
    col1 JSON,
    col2 JSON
);

CREATE TABLE table_custom (
    id INT PRIMARY KEY,
    -- [sqlgg] module=Custom
    custom_col JSON
);

-- @test1
SELECT JSON_SEARCH(@json, @one, 'dark');

-- @test2
SELECT JSON_ARRAY_APPEND('["a", ["b", "c"], "d"]', @path, 1);

-- @test3
SELECT JSON_EXTRACT(col1, @name_path) FROM table1 WHERE id = @id;

-- @test4
SELECT JSON_UNQUOTE(JSON_EXTRACT(col1, @email_path)) FROM table1 WHERE id = @id;

-- @test5
UPDATE table1 SET col1 = JSON_SET(col1, @login_path, NOW()) WHERE id = @id;

-- @test6
SELECT JSON_CONTAINS(col1, '"value"') FROM table1 WHERE id = @id;

-- @test7
SELECT id, name FROM table2 WHERE JSON_EXTRACT(col1, @category_path) = @category;

-- @test8
SELECT 
    id,
    JSON_EXTRACT(col1, @name_path) as user_name,
    JSON_EXTRACT(col2, @theme_path) as theme
FROM table1 
WHERE JSON_EXTRACT(col1, @active_path) = 'true';

-- @test9
UPDATE table2 
SET col2 = JSON_SET(col2, @update_path, NOW())
WHERE JSON_EXTRACT(col1, @role_path) = @role;

-- @test10
SELECT JSON_SEARCH(col1, 'one', @search_value) FROM table1 WHERE id = @id;

-- @test11 - Universal JSON_CONTAINS test (reusable with any JSON type)
SELECT JSON_CONTAINS(col1, @json_value) FROM table1 WHERE id = @id;

-- @test12 - Universal INSERT test (reusable with any JSON type)
INSERT INTO table1 (id, col1) VALUES (@id, @json_data);

-- @test13 - Test custom JSON type INSERT
INSERT INTO table_custom (id, custom_col) VALUES (@id, @custom_data);

-- @test14 - Test custom JSON type SELECT
SELECT custom_col FROM table_custom WHERE id = @id;
