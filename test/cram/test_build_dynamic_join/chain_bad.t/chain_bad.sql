CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (id INT PRIMARY KEY, user_id INT, bio TEXT);
CREATE TABLE avatars (id INT PRIMARY KEY, profile_id INT, url TEXT);
-- [sqlgg] dynamic_select=true
-- @chain_bad
SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.profile_id = p.id WHERE u.id = @uid;
