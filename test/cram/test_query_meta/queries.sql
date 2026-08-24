CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL, email TEXT NULL);

-- @find_user
SELECT id, name FROM users WHERE id = @id;

SELECT COUNT(*) AS total FROM users;

-- @find_users
SELECT id, name FROM users WHERE id IN @ids;

-- @add_users
INSERT INTO users (id, name) VALUES @rows;

-- @rename_user
UPDATE users SET name = @name WHERE id = @id;

DELETE FROM users WHERE id = @id;
