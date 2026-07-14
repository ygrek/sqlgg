CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price INT);
CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE orders (id INT PRIMARY KEY, user_id INT, total INT);

-- SELECT * over subquery: dynamic columns pushed down.
-- [sqlgg] dynamic_select=true
-- @star_over_subq
SELECT * FROM (SELECT id, name, price FROM products WHERE price > @min) AS sub;

-- Same via explicit sub.*.
-- [sqlgg] dynamic_select=true
-- @star_alias_over_subq
SELECT sub.* FROM (SELECT id, name, price FROM products) AS sub;

-- Over a LEFT JOIN subquery: joined columns stay nullable.
-- [sqlgg] dynamic_select=true
-- @star_over_join_subq
SELECT * FROM (SELECT u.id AS uid, u.name AS uname, o.total AS ototal FROM users u LEFT JOIN orders o ON o.user_id = u.id) AS sub;

-- Non-pass-through: dynamic stays at the outer level.
-- [sqlgg] dynamic_select=true
-- @cols_over_subq
SELECT sub.id, sub.name FROM (SELECT id, name, price FROM products) AS sub WHERE sub.id = @id;
