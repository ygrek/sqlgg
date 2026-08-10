CREATE TABLE posts (
  id INT NOT NULL PRIMARY KEY,
  body TEXT NULL,
  channel_id INT NULL
);

CREATE TABLE channels (
  channel_id INT NOT NULL PRIMARY KEY,
  channel_name TEXT NOT NULL,
  image_url TEXT NULL
);

-- [sqlgg] dynamic_select=true
-- @feed
SELECT p.id, p.body, c.channel_id, c.channel_name, c.image_url
FROM posts p
LEFT JOIN channels c ON c.channel_id = p.channel_id
WHERE p.id > @min_id;
