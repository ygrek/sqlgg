CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT NOT NULL,
  email VARCHAR(255) NOT NULL
);
CREATE TABLE posts (
  id INT PRIMARY KEY,
  user_id INT NOT NULL,
  title TEXT NOT NULL,
  body TEXT,
  created_at INT
);
