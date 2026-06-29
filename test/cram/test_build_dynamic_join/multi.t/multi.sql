CREATE TABLE users (id INT PRIMARY KEY, name TEXT, mentor_id INT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
CREATE TABLE avatars (id INT PRIMARY KEY, url TEXT);
-- [sqlgg] dynamic_select=true
-- @two_indep
SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = u.id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @subq_in_on
SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = (SELECT MAX(id) FROM avatars) WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @same_twice
SELECT u.id, p1.bio AS bio1, p2.bio AS bio2 FROM users u LEFT JOIN profiles p1 ON p1.user_id = u.id LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = @uid;
