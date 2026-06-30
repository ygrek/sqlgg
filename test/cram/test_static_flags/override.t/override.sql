CREATE TABLE users (id INT NOT NULL, name TEXT NULL);
-- @get_user
SELECT id, name FROM users WHERE id = @id;
-- [sqlgg] dynamic_select=false
-- @get_user_classic
SELECT id, name FROM users WHERE id = @id;
