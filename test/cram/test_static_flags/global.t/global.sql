CREATE TABLE users (id INT NOT NULL, name TEXT NULL);
-- @get_user
SELECT id, name FROM users WHERE id = @id;
-- @list_users
SELECT id, name FROM users;
