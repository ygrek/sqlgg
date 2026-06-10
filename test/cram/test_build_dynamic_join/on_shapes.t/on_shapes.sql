CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
-- [sqlgg] dynamic_select=true
-- @param_in_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = @b WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @extra_const_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @on_inequality
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @no_alias
SELECT u.id, profiles.bio FROM users u LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @flipped_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON u.id = p.user_id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @const_key_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = 5 WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @or_in_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id OR p.user_id = 0 WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_own_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = (SELECT MAX(id) FROM users) WHERE u.id = @uid;
