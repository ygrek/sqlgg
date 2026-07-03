CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT, avatar_id INT);
CREATE TABLE avatars (id INT PRIMARY KEY, url TEXT, badge_id INT);
CREATE TABLE badges (id INT PRIMARY KEY, label TEXT);
-- [sqlgg] dynamic_select=true
-- @chain
SELECT u.id, p.bio, a.url FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @chain3
SELECT u.id, p.bio, a.url, b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @diamond
SELECT u.id, a.url, b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = @uid;
-- [sqlgg] dynamic_select=true
-- @chain_pinned
SELECT u.id, p.bio, a.url, b.label FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.id = p.avatar_id LEFT JOIN badges b ON b.id = a.badge_id WHERE b.label = @label;
