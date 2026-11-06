ALTER TABLE users ADD COLUMN nickname TEXT;

CREATE TABLE posts (
  id INT PRIMARY KEY,
  author_id INT NOT NULL,
  title TEXT NOT NULL,
  state ENUM('draft', 'published') NOT NULL
);
