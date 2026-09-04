CREATE TABLE users (id INT, name TEXT);
CREATE TABLE posts (id INT, author INT, title TEXT);

WITH recent AS (SELECT id, title FROM posts WHERE id > 5)
SELECT u.name, r.title, sub.n
FROM users AS u
JOIN recent AS r ON u.id = r.id
JOIN (SELECT author, count(*) AS n FROM posts GROUP BY author) AS sub ON sub.author = u.id;

WITH recent AS (SELECT id FROM posts)
SELECT u.nmae, r.id FROM users AS u JOIN recent AS r ON u.id = r.id;

UPDATE users AS uu SET nmae = 'x' WHERE uu.id = 1;

SELECT id FROM users WHERE id IN (SELECT author FROM posts AS p WHERE p.title = '') ORDER BY id;

UPDATE users AS uu, (SELECT author, count(*) AS n FROM posts GROUP BY author) AS agg
SET uu.nmae = agg.n WHERE uu.id = agg.author;

SELECT id FROM users;
CREATE TABLE archive (id INT);
SELECT id FROM users AS only_users;
SELECT title FROM users;
SELECT id FROM users JOIN posts ON users.id = posts.id;
SELECT 1 AS one;
SELECT id FROM users UNION SELECT id FROM posts;
INSERT INTO users (id, name) VALUES (@new_id, @new_name);
UPDATE users SET name = @next_name WHERE id = @user_id;
DELETE FROM users WHERE id = @delete_id;

-- @shared_users | include: reuse
SELECT name FROM users;

-- @shared_use
WITH shared_users_cte AS &shared_users
SELECT name FROM shared_users_cte;
