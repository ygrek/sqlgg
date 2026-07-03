CREATE TABLE users (id INT PRIMARY KEY, name TEXT, user_id INT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
CREATE TABLE orders (bio TEXT, amount INT);
CREATE TABLE shipments (user_id INT, status TEXT);
-- [sqlgg] dynamic_select=true
-- @inner_join
SELECT u.id, p.bio FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @join_using
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @join_natural
SELECT u.id, p.bio FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @using_after_candidate
SELECT u.id, p.bio, o.amount FROM users u LEFT JOIN profiles p ON p.user_id = u.id JOIN orders o USING (bio) WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @natural_after_candidate
SELECT u.id, p.bio, o.amount FROM users u LEFT JOIN profiles p ON p.user_id = u.id NATURAL JOIN orders o WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @right_join
SELECT u.id, p.bio FROM users u RIGHT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @comma_join
SELECT u.id, p.bio FROM users u, profiles p WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @implicit_before_candidate
SELECT u.id, p.bio, s.status FROM users u JOIN shipments s USING (user_id) LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
