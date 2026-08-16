CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  nick TEXT
);

-- [sqlgg] name=insert_returning_id
INSERT INTO users (name, nick) VALUES (@name, @nick) RETURNING id;

-- [sqlgg] name=insert_returning_all
INSERT INTO users (name, nick) VALUES (@name, @nick) RETURNING *;

-- [sqlgg] name=insert_returning_nullable
INSERT INTO users (name, nick) VALUES (@name, @nick) RETURNING nick;

-- [sqlgg] name=insert_returning_expr
INSERT INTO users (name, nick) VALUES (@name, @nick) RETURNING id, CONCAT(name, '!') AS greeting;

-- [sqlgg] name=insert_returning_param
INSERT INTO users (name, nick) VALUES (@name, @nick) RETURNING id, CONCAT(name, @suffix) AS tagged;

-- [sqlgg] name=insert_set_returning
INSERT INTO users SET name = @name, nick = @nick RETURNING id;

-- [sqlgg] name=insert_tuple_list_returning
INSERT INTO users (name, nick) VALUES @values RETURNING id, nick;

-- [sqlgg] name=insert_multi_values_returning
INSERT INTO users (name, nick) VALUES (@name1, @nick1), (@name2, @nick2) RETURNING id;

-- [sqlgg] name=insert_select_returning
INSERT INTO users (name, nick) SELECT name, nick FROM users WHERE id > @min RETURNING id;

-- [sqlgg] name=insert_on_conflict_returning
INSERT INTO users (id, name) VALUES (@id, @name) ON CONFLICT(id) DO UPDATE SET name = excluded.name RETURNING id, nick;

-- [sqlgg] name=insert_do_nothing_returning
INSERT INTO users (id, name) VALUES (@id, @name) ON CONFLICT(id) DO NOTHING RETURNING id, nick;

-- [sqlgg] name=update_returning
UPDATE users SET name = @name WHERE id = @id RETURNING id, nick;

-- [sqlgg] name=update_returning_param
UPDATE users SET name = @name WHERE id = @id RETURNING id, CONCAT(name, @suffix) AS tagged;

-- [sqlgg] name=delete_returning
DELETE FROM users WHERE id = @id RETURNING *;

-- [sqlgg] name=delete_returning_param
DELETE FROM users WHERE id = @id RETURNING id, CONCAT(name, @suffix) AS tagged;
