CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
CREATE TABLE orders (id INT PRIMARY KEY, user_id INT, total INT);
-- [sqlgg] dynamic_select=true
-- @ok
SELECT u.id, u.name, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @nonuniq
SELECT u.id, o.total FROM users u LEFT JOIN orders o ON o.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @ref_in_where
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE p.bio = @b;
