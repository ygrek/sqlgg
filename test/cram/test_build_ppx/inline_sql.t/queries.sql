-- @find_user
SELECT id, name
FROM users
WHERE id = @id;

-- @add_user
INSERT INTO users (id, name, email) VALUES (@id, @name, @email);

-- @add_users
INSERT INTO users (id, name, email) VALUES @rows;

-- @rename_user
UPDATE users SET name = @name, nickname = @nickname WHERE id = @id;

-- @titles_by
SELECT p.title, u.nickname
FROM posts p
JOIN users u ON u.id = p.author_id
WHERE p.author_id = @author AND p.state = @state;

-- @publish
UPDATE posts SET state = @state WHERE id = @id;
