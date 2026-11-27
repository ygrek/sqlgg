CREATE TABLE users (id INT NOT NULL, parent_id INT);
CREATE TABLE teams (id INT NOT NULL, parent_id INT);
CREATE TABLE tags (id TEXT NOT NULL, parent_id TEXT);
CREATE TABLE events (user_id INT NOT NULL, kind TEXT);

-- @pair
SELECT COUNT(1) AS n FROM @left_src AS l JOIN @right_src AS r ON l.id = r.id;

-- @tree
SELECT child.id FROM @src AS child JOIN @src AS parent ON child.parent_id = parent.id;

-- @active
SELECT u.id FROM users AS u WHERE EXISTS (SELECT 1 FROM @ev AS e WHERE e.user_id = u.id);
