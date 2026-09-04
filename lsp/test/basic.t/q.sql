CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT
);

-- @get_user
SELECT id, name, email FROM users WHERE id = @id;

-- @count_users
SELECT count(*) AS total FROM users;

-- @names
SELECT group_concat(name ORDER BY id + 1) AS names FROM users;

-- @typo
SELECT id, nmae FROM users;

-- @broken
SELECT * FRM users;

-- @find
SELECT id FROM users WHERE @filter { ByName { name = @n } | ById { id = @i } | All { TRUE } };

SELECT users.id, nmae2 FROM users;

-- a comment mentioning users
SELECT 'users' AS s FROM users;

-- @in_list
SELECT id FROM users WHERE id IN @ids AND name NOT IN @excluded;
