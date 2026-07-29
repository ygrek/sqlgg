CREATE TABLE posts (
  id INT NOT NULL PRIMARY KEY,
  body TEXT NULL,
  reply_count INT NOT NULL,
  hits INT NULL
);

-- [sqlgg] dynamic_select=true
-- @feed
SELECT id, body, reply_count, hits FROM posts WHERE id > @min_id;

-- [sqlgg] dynamic_select=true
-- @counts
SELECT id, reply_count AS n FROM posts WHERE id > @min_id;

-- [sqlgg] dynamic_select=true
-- @labels
SELECT id, body AS n FROM posts WHERE id > @min_id;

-- @add_post
INSERT INTO posts (id, body, reply_count, hits)
VALUES (@id, @body, @reply_count, @hits);
