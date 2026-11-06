CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT,
  -- [sqlgg] non_nullifiable=true
  seen_at DATETIME NULL
);

CREATE TABLE audit (
  id INT PRIMARY KEY,
  name TEXT,
  -- an ordinary comment
  noted_at DATETIME NULL
);

CREATE TABLE posts (
  id INT PRIMARY KEY,
  state ENUM('draft', 'published') NOT NULL
);
