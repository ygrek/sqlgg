CREATE TABLE users (
    -- [sqlgg] module=User_id
    id INT PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT
);

CREATE TABLE admins (
    role TEXT,
    -- [sqlgg] module=User_id
    id INT NOT NULL,
    name TEXT NOT NULL
);

-- [sqlgg] dynamic_select=true
-- @get_user
SELECT id, name, email FROM users WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- @list_admins
SELECT role, id, name FROM admins;
