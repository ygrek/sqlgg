CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT,
  email TEXT NOT NULL
);

CREATE TABLE wrapped_users (
  -- [sqlgg] module=Wrapped_id
  id INT PRIMARY KEY,
  -- [sqlgg] module=Wrapped_id
  parent_id INT
);
