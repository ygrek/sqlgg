CREATE TABLE users (id INT NOT NULL, name TEXT NULL);
-- [sqlgg] dynamic_select=both
-- @get_user
SELECT id, name FROM users WHERE id = @id;
