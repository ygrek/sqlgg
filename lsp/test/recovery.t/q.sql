CREATE TABLE users (
  id INT PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT
);

CREATE TABLE posts (
  id INT PRIMARY KEY,
  author_id INT NOT NULL,
  title TEXT
);

-- a comma left before FROM
SELECT id, FROM users WHERE na1;

-- an operand missing
SELECT id FROM users WHERE id = AND na2;

-- a keyword misspelt : the table is only known from the tokens stepped over
SELECT * FRM users WHERE na3;

-- an operand missing in a JOIN condition, the aliases still resolve
SELECT u.id FROM users u JOIN posts p ON = p.author_id WHERE p.ti;
