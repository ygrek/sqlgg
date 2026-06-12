CREATE TABLE users (id INT PRIMARY KEY, name TEXT, manager_id INT);
-- [sqlgg] dynamic_select=true
-- @bad
SELECT u1.id, u2.name FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id;
-- [sqlgg] dynamic_select=true
-- @good
SELECT u1.id, u2.name FROM users u1 LEFT JOIN users u2 ON u2.id = u1.manager_id;
