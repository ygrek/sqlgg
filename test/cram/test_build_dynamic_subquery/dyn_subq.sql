CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price INT);
CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE orders (id INT PRIMARY KEY, user_id INT, total INT);

-- Pass-through (SELECT *) over a subquery source: dynamic columns are pushed down
-- INTO the subquery projection.
-- [sqlgg] dynamic_select=true
-- @star_over_subq
SELECT * FROM (SELECT id, name, price FROM products WHERE price > @min) AS sub;

-- Pass-through via explicit src.* over a subquery source.
-- [sqlgg] dynamic_select=true
-- @star_alias_over_subq
SELECT sub.* FROM (SELECT id, name, price FROM products) AS sub;

-- Pass-through over a LEFT JOIN subquery: nullability of joined columns is honest,
-- and the dynamic projection still penetrates the subquery.
-- [sqlgg] dynamic_select=true
-- @star_over_join_subq
SELECT * FROM (SELECT u.id AS uid, u.name AS uname, o.total AS ototal FROM users u LEFT JOIN orders o ON o.user_id = u.id) AS sub;

-- Non-pass-through: the outer query selects specific columns, so the dynamic stays
-- at the outer level and the subquery projection remains fixed.
-- [sqlgg] dynamic_select=true
-- @cols_over_subq
SELECT sub.id, sub.name FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = @id;
