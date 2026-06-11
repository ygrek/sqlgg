CREATE TABLE comments (
  id INT PRIMARY KEY,
  post_id INT NOT NULL,
  author TEXT NOT NULL,
  created_at INT
);
CREATE TABLE tags (
  id INT PRIMARY KEY,
  label TEXT NOT NULL
);
CREATE TABLE post_tags (
  post_id INT NOT NULL,
  tag_id INT NOT NULL
);
