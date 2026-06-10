CREATE TABLE users (id INT PRIMARY KEY, name TEXT, note TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
CREATE TABLE billing (user_id INT PRIMARY KEY, plan TEXT);
-- [sqlgg] dynamic_select=true
-- @ws
SELECT u.id, u.name,
       p.bio,
       b.plan
FROM users u
LEFT JOIN profiles p ON p.user_id = u.id
LEFT JOIN billing b ON b.user_id = u.id
WHERE u.note = '  two  spaces  inside  ' AND u.id = @uid;
