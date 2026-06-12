CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
-- [sqlgg] dynamic_select=true
-- @join_subq_source
SELECT u.id, s.bio FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_join_dup
SELECT u.id, s.bio FROM users u LEFT JOIN (SELECT p.user_id, p.bio FROM profiles p, users x) s ON s.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_union_dup
SELECT u.id, s.bio FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles UNION ALL SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_base_join
SELECT s.id, p.bio FROM (SELECT id FROM users) s LEFT JOIN profiles p ON p.user_id = s.id;
