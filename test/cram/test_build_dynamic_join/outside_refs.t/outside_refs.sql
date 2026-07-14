CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
-- [sqlgg] dynamic_select=true
-- @ref_in_group
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio;
-- [sqlgg] dynamic_select=true
-- @ref_in_order
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio;
-- [sqlgg] dynamic_select=true
-- @ref_in_having
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0;
-- [sqlgg] dynamic_select=true
-- @complex_proj
SELECT u.id, CONCAT(p.bio, '!') AS shout FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_in_where
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles);
-- [sqlgg] dynamic_select=true
-- @unqualified_where
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x';
-- [sqlgg] dynamic_select=true
-- @join_unreferenced
SELECT u.id, u.name FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
